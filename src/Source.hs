module Source where

import Data.Maybe
import Control.Arrow (first)

import Language.Haskell.GHC.ExactPrint as EP
import Language.Haskell.GHC.ExactPrint.Parsers as EP
import Language.Haskell.GHC.ExactPrint.Utils as EP
import Language.Haskell.GHC.ExactPrint.Delta as EP

import GHC
import qualified HscMain as GHC
import qualified OccName as GHC
import qualified RdrName as GHC
import qualified SrcLoc as GHC
import qualified Bag as GHC
import qualified Name as GHC
import qualified Module as GHC
import qualified FastString as GHC
import qualified BasicTypes as GHC
import qualified HscTypes as GHC
import qualified TcRnTypes as GHC
import qualified UniqDFM as GHC
import Outputable as Out
import GHC.Paths (libdir)
import DynFlags

import HIE.Bios as Bios
import HIE.Bios.Environment as Bios

import System.Log.Logger as Log
import System.Directory
import Control.Monad.IO.Class

import Data.Generics as SYB

import Utils

type FixityEnv = [(String, [(GHC.OccName, GHC.Fixity)])]

lookupFixityByName :: FixityEnv -> GHC.OccName -> String -> Maybe GHC.Fixity
lookupFixityByName env name modname
  | Just symbs <- lookup modname env
  = lookup name symbs
lookupFixityByName _ _ _ = Nothing

--
-- At https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/parser:
-- > Infix operators are parsed as if they were all left-associative.
-- > The renamer uses the fixity declarations to re-associate the syntax tree.
-- 
fixInfixRInParsedSource :: FixityEnv -> [Located (GHC.OccName, Maybe String)] -> ParsedSource -> ParsedSource
fixInfixRInParsedSource fenv rvs = SYB.everywhere (SYB.mkT $ fix fenv rvs)
  where
    fix :: FixityEnv -> [Located (GHC.OccName, Maybe String)] -> LHsExpr GhcPs -> LHsExpr GhcPs
    fix fenv rvs (L loc (OpApp _ right@(L _ (OpApp _ right' op' left')) op left))
      | opNm  <- GHC.rdrNameOcc <$> opName op
      , opNm' <- GHC.rdrNameOcc <$> opName op'
      , Just opFix   <- lookupFixityByName fenv (unLoc opNm) $ findModName rvs opNm
      , Just opFix'  <- lookupFixityByName fenv (unLoc opNm') $ findModName rvs opNm'
      , (_, True) <- compareFixity opFix' opFix
      = L loc $ opapp right' op' $ noLoc $ opapp left' op left
    fix _ _ x = x

    opapp = OpApp noExt

    -- | Returns operator's name.
    --  Operators are _always_ just names.
    opName :: LHsExpr GhcPs -> Located RdrName
    opName (L _ (HsVar _ name)) = name

    findModName :: [Located (GHC.OccName, Maybe String)] -> Located GHC.OccName -> String
    findModName rvs (L loc name) = let
        rvs' = map unLoc $ filter ((loc ==) . getLoc) rvs
      in fromJust $ fromJust $ lookup name rvs'


getFixities :: HscEnv -> IO FixityEnv
getFixities hsc_env = do
  -- External Package State
  eps <- GHC.hscEPS hsc_env
  -- Home Package Table
  let mif_eps = GHC.moduleEnvElts $ GHC.eps_PIT eps
  let global_fixities = getFixities' mif_eps

  let hpt = GHC.hsc_HPT hsc_env
  let mif_hpt = map GHC.hm_iface $ GHC.eltsUDFM hpt
  let project_fixities = getFixities' mif_hpt

  let fixities = first (GHC.moduleNameString . GHC.moduleName) <$> (project_fixities ++ global_fixities)
  -- putStrLn $ showElem fixities
  return fixities
  where
    getFixities' mif = zip (map GHC.mi_module mif) (map GHC.mi_fixities mif)

--------------------------------------------------------------------
--------------------------------------------------------------------
--------------------------------------------------------------------
--------------------------------------------------------------------

collectLocatedRenamedNames :: HsGroup GhcRn -> [Located Name]
collectLocatedRenamedNames = collect t1
  where
    t1 :: HsExpr GhcRn -> [Located Name]
    t1 (HsVar _ var) = [var]
    t1 x = []

--------------------------------------------------------------------
--------------------------------------------------------------------
--------------------------------------------------------------------
--------------------------------------------------------------------

-- getSourceSimple :: String -> String -> IO (TypecheckedModule, DynFlags)
getSourceSimple path mod = defaultErrorHandler defaultFatalMessager defaultFlushOut $
  do
    -- libdir <- PS.getLibDir
    runGhc (Just libdir) $ do
      dflags <- getSessionDynFlags
      setSessionDynFlags dflags
      target <- guessTarget path Nothing
      setTargets [target]
      load LoadAllTargets
      hsc_env <- getSession
      modSum <- getModSummary $ mkModuleName mod
      p <- GHC.parseModule modSum
      t <- typecheckModule p
      return (t, dflags, hsc_env)

-- getSource :: String -> IO (Maybe TypecheckedModule, DynFlags)
getSource path =
  -- withCurrentDirectory "../04" $
  -- withCurrentDirectory "./resources/testProj" $
  defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
      -- TODO: construct cradle in memory
      cradle <- Bios.loadCradle "./hie.yaml"
      copt <- Bios.getCompilerOptions path cradle
      -- putStrLn $ show copt
      -- putStrLn "===="
      case copt of
        CradleNone                      -> fail "CradleNone: nani?"
        CradleFail (CradleError { cradleErrorStderr = msgs }) -> fail $ show msgs
        CradleSuccess opt -> do
          CradleSuccess libdir <- Bios.getRuntimeGhcLibDir cradle
          putStrLn $ "Libdir: " ++ libdir
          -- putStrLn $ "Opts: " ++ show opt
          -- let libdir = "/home/maar/.stack/programs/x86_64-linux/ghc-tinfo6-8.8.3/lib/ghc-8.8.3"
          runGhc (Just libdir) $ do
              targets <- Bios.initSession opt
              setTargets targets
              dflags <- getSessionDynFlags
              -- To get comments from source we need to use Opt_KeepRawTokenStream option.
              let dflags1 = (dflags `gopt_set` Opt_KeepRawTokenStream)
              let dflags2 = dflags1 {hscTarget = HscInterpreted, ghcLink = LinkInMemory , ghcMode = CompManager }
              setSessionDynFlags dflags2

              liftIO $ putStrLn $ showElem $ packageFlags dflags1

              -- g <- depanal [] False
              -- liftIO $ putStrLn $ show (map ms_location $ mgModSummaries g)

              -- Bios.setTargetFilesWithMessage (Just GHC.batchMsg) [(path, path)]

              -- let get_fp = ml_hs_file . ms_location . pm_mod_summary . tm_parsed_module
              -- liftIO $ putStrLn $ "Typechecked modules for: " ++ (unlines $ map (show . get_fp) ts)

              (t, _) <- loadFile (path, path)

              -- modSum <- getModSummary $ mkModuleName "Utils"
              -- p <- GHC.parseModule modSum
              -- t <- typecheckModule p

              dflags <- getSessionDynFlags
              hsc_env <- getSession
              return (t, dflags, hsc_env)

handleModule path = do
  updateGlobalLogger "hie-bios" (setLevel DEBUG)
  (tm', dflags, hsc_env) <- getSource path
  let tm = fromMaybe (error "Module isn't typechecked") tm'

  let pmod = tm_parsed_module tm
  let src = pm_parsed_source pmod
  let apianns = pm_annotations pmod
  -- To get annotations in ExactPrint format.
  let Right (ans, _) = postParseTransform (Right (apianns, [], dflags, src)) normalLayout
  let Just (group, _, _, _) = tm_renamed_source tm

  let rvs = collectLocatedRenamedNames group
  let rvs' = fmap destructNameToOcc <$> rvs
  fixities <- getFixities hsc_env
  let src' = fixInfixRInParsedSource fixities rvs' src

  return (ans, src', rvs, dflags)

handleModule' path = do
  Right (ans, src) <- EP.parseModule path
  let mod = fromMaybe "Main" $ getModuleName $ unLoc src
  (tm, dflags, hsc_env) <- getSourceSimple path mod

  let Just (group, _, _, _) = tm_renamed_source tm
  let rvs = collectLocatedRenamedNames group
  let rvs' = fmap destructNameToOcc <$> rvs
  fixities <- getFixities hsc_env
  let src' = fixInfixRInParsedSource fixities rvs' src

  return (ans, src', rvs, dflags)


test dir path = do
  updateGlobalLogger "hie-bios" (setLevel ERROR)
  withCurrentDirectory dir $ do
    (tm', dflags, hsc_env) <- getSource path
    let tm = fromMaybe (error "Module isn't typechecked") tm'
    let src = pm_parsed_source $ tm_parsed_module tm
    putStrLn $ showElem src
