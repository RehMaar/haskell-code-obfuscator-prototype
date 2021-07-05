module Transform.Internal.Builtins where

import Data.String
import GHC.SourceGen

--
-- map f [] = []
-- map f (x:xs) = f x : map f xs
--
builtinMap :: String -> HsDecl'
builtinMap name =
  funBinds (fromString name)
    [ match [wildP, nil] nil
    , match [bvar "f", conP "(:)" [bvar "x", bvar "xs"]] $
        cons @@ (bvar "f" @@ bvar "x") @@ (bvar (fromString name) @@ bvar "f" @@ bvar "xs")
    ]
