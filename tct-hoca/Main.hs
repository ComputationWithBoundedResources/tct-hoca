module Main where

import           Tct.Core.Main        (tct3)
import           Tct.Hoca.Config      (hocaConfig, hocaDeclarations)
import           Tct.Hoca.Types

import qualified Tct.Core.Data        as T

import           Tct.Trs.Data
import           Tct.Trs.Declarations (trsDeclarations)


instance T.Declared TrsProblem TrsProblem where
  decls = trsDeclarations

instance T.Declared ML ML where
  decls = hocaDeclarations

main :: IO ()
main = tct3 hocaConfig

