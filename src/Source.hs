module Source
  ( SourceInfo(..)
  , ModuleType(..)
  , handleModule
  )
where

import qualified GHC.LanguageExtensions as LangExt
import Control.Monad

import           Data.Maybe
import           Control.Arrow                  ( first )

import           Language.Haskell.GHC.ExactPrint
                                               as EP
import           Language.Haskell.GHC.ExactPrint.Parsers
                                               as EP
import           Language.Haskell.GHC.ExactPrint.Utils
                                               as EP
import           Language.Haskell.GHC.ExactPrint.Delta
                                               as EP

import           GHC
import qualified Avail                         as GHC
import qualified HscMain                       as GHC
import qualified OccName                       as GHC
import qualified RdrName                       as GHC
import qualified SrcLoc                        as GHC
import qualified Bag                           as GHC
import qualified Name                          as GHC
import qualified Module                        as GHC
import qualified FastString                    as GHC
import qualified BasicTypes                    as GHC
import qualified HscTypes                      as GHC
import qualified TcRnTypes                     as GHC
import qualified UniqDFM                       as GHC
import           Outputable                    as Out
import           GHC.Paths                      ( libdir )
import           DynFlags

import           HIE.Bios                      as Bios
import           HIE.Bios.Environment          as Bios

import           System.Log.Logger             as Log
import           System.Directory
import           Control.Monad.IO.Class

import           Data.Generics                 as SYB

import           Utils
import           Transform.Query

type FixityEnv = [(String, [(GHC.OccName, GHC.Fixity)])]

lookupFixityByName :: FixityEnv -> GHC.OccName -> String -> GHC.Fixity
lookupFixityByName env name modname
  | Just symbs <- lookup modname env
  = fromMaybe GHC.defaultFixity $ lookup name symbs
lookupFixityByName _ _ _ = GHC.defaultFixity

--
-- At https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/parser:
-- > Infix operators are parsed as if they were all left-associative.
-- > The renamer uses the fixity declarations to re-associate the syntax tree.
-- 
fixInfixRInParsedSource
  :: FixityEnv
  -> [Located (GHC.OccName, Maybe String)]
  -> ParsedSource
  -> ParsedSource
fixInfixRInParsedSource fenv rvs = SYB.everywhere (SYB.mkT $ fix fenv rvs)
 where
  fix
    :: FixityEnv
    -> [Located (GHC.OccName, Maybe String)]
    -> LHsExpr GhcPs
    -> LHsExpr GhcPs
  fix fenv rvs (L loc (OpApp _ right@(L _ (OpApp _ right' op' left')) op left))
    | opNm   <- GHC.rdrNameOcc <$> opName op
    , opNm'  <- GHC.rdrNameOcc <$> opName op'
    , opFix  <- lookupFixityByName fenv (unLoc opNm) $ findModName rvs opNm
    , opFix' <- lookupFixityByName fenv (unLoc opNm') $ findModName rvs opNm'
    , (_, True) <- compareFixity opFix' opFix
    = L loc $ opapp right' op' $ fix fenv rvs $ noLoc $ opapp left' op left
  fix _ _ x = x

  opapp = OpApp noExt

  -- | Returns operator's name.
  --  Operators are _always_ just names not any other expressions.
  opName :: LHsExpr GhcPs -> Located RdrName
  opName (L _ (HsVar _ name)) = name

  findModName
    :: [Located (GHC.OccName, Maybe String)] -> Located GHC.OccName -> String
  findModName rvs (L loc name) =
    let rvs' = map unLoc $ filter ((loc ==) . getLoc) rvs
    in  fromJust $ fromJust $ lookup name rvs'

-- | Returns a map with modules and their exportes symbols with fixities
getFixities :: HscEnv -> IO FixityEnv
getFixities hsc_env = do
  -- External Package State
  eps <- GHC.hscEPS hsc_env
  let mif_eps          = GHC.moduleEnvElts $ GHC.eps_PIT eps
  let global_fixities  = getFixities' mif_eps

  -- Home Package Table
  let hpt              = GHC.hsc_HPT hsc_env
  let mif_hpt          = map GHC.hm_iface $ GHC.eltsUDFM hpt
  let project_fixities = getFixities' mif_hpt

  let fixities         = project_fixities ++ global_fixities
  let fixEnv           = first getModName <$> fixities
  return fixEnv
 where
  getModName = GHC.moduleNameString . GHC.moduleName
  getFixities' mif = zip (map GHC.mi_module mif) (map GHC.mi_fixities mif)


data ModuleType
  = SimpleModule { mt_opts :: [String] }
  | ProjectModule { mt_wdir :: FilePath }

data SourceInfo
  = SourceInfo
  { si_annotations :: Anns
  , si_parsed_source :: ParsedSource
  , si_qualified_names :: [Located Name]
  -- ^List of all names with their right qualifications obtained from Typechecked tree.
  , si_exported :: Maybe [(LIE GhcRn, GHC.Avails)]
  , si_dynflags :: DynFlags
    --, si_internals_ :: Maybe (TypecheckedModule, HscEnv)
  }

-- handleModule :: ModuleType -> FilePath -> (Ans, Src, Rvs, DFlags)
handleModule (SimpleModule opts)  = handleModuleWith (getSourceSimple opts)
handleModule (ProjectModule wdir) = handleModuleWith (getSourceProject wdir)

handleModuleWith get path = do
  (tm', dflags, hsc_env) <- get path
  let tm      = fromMaybe (error "Module isn't typechecked") tm'

  let pmod    = tm_parsed_module tm
  let src     = pm_parsed_source pmod
  let apianns = pm_annotations pmod
  -- To get annotations in ExactPrint format.
  let Right (ans, _) =
        postParseTransform (Right (apianns, [], dflags, src)) normalLayout

  let Just (group, _, exports, _) = tm_renamed_source tm
  let rvs                   = collectLocatedRenamedNames group
  let rvs'                  = fmap destructNameToOcc <$> rvs
  fixities <- getFixities hsc_env
  let src' = fixInfixRInParsedSource fixities rvs' src

  return $ SourceInfo { si_annotations     = ans
                      , si_parsed_source   = src'
                      , si_qualified_names = rvs
                      , si_exported        = exports
                      , si_dynflags        = dflags
                      }
 where
  collectLocatedRenamedNames :: HsGroup GhcRn -> [Located Name]
  collectLocatedRenamedNames = collect varName

  -- Variables and field selectors
  varName :: HsExpr GhcRn -> [Located Name]
  varName (HsVar _ var) = [var]
  varName (HsRecFld _ (Unambiguous n rdrn)) = [L (getLoc rdrn) n]
  varName (HsRecFld _ (Ambiguous _ _ )) = error "Soucrce:varName:Ambiguous: What case?"
  varName x             = []

-- getSource :: String -> IO (Maybe TypecheckedModule, DynFlags)
getSourceProject wdir path =
  withCurrentDirectory wdir
    $ defaultErrorHandler defaultFatalMessager defaultFlushOut
    $ do
      -- TODO: construct cradle in memory
      --       Maybe possible to use implicit-hie-cradle package?
      --       `loadImplicitCradle` doesn't work by some reason
        cradle <- Bios.loadCradle "./hie.yaml"
        copt   <- Bios.getCompilerOptions path cradle
        case copt of
          CradleNone -> fail "CradleNone: nani?"
          CradleFail CradleError { cradleErrorStderr = msgs } ->
            fail $ show msgs
          CradleSuccess opt -> do
            libdir <- fromCradle (fail "Unable to get runtime libdir")
              <$> Bios.getRuntimeGhcLibDir cradle
            runGhc (Just libdir) $ do
              -- TODO: initSession creates cache directories for IDE purposes,
              --       need to init without it.
              targets <- Bios.initSession opt
              setTargets targets
              dflags <- getSessionDynFlags
              -- To get comments from source we need to use Opt_KeepRawTokenStream option.
              let dflags1 = dflags `gopt_set` Opt_KeepRawTokenStream
              let dflags2 = dflags1 { hscTarget = HscInterpreted
                                    , ghcLink   = LinkInMemory
                                    , ghcMode   = CompManager
                                    }
              setSessionDynFlags dflags2

              (t, _)  <- loadFile (path, path)

              dflags  <- getSessionDynFlags
              hsc_env <- getSession
              return (t, dflags, hsc_env)
 where
  fromCradle _ (CradleSuccess s) = s
  fromCradle f _                 = f

getSourceSimple flags path =
  defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    runGhc (Just libdir) $ do
      dflags <- getSessionDynFlags
      parseDynamicFlags dflags $ map noLoc flags
      let dflags1 = dflags `gopt_set` Opt_KeepRawTokenStream
      let dflags2 = dflags1 { hscTarget = HscInterpreted
                            , ghcLink   = LinkInMemory
                            , ghcMode   = CompManager
                            }
      setSessionDynFlags dflags2
      target <- guessTarget path Nothing
      setTargets [target]
      load LoadAllTargets
      hsc_env  <- getSession

      modGraph <- depanal [] False
      let modSum = findModSumWithLocation path $ mgModSummaries modGraph
      p <- GHC.parseModule modSum
      t <- typecheckModule p
      return (Just t, dflags, hsc_env)
 where
  findModSumWithLocation loc =
    head . filter ((loc ==) . fromMaybe "" . ml_hs_file . ms_location)
