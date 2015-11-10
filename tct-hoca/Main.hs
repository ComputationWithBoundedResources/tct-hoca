module Main where

import Tct.Core        (Declared (..), runTct)
import Tct.Trs         (trsDeclarations)

import Tct.Hoca.Config (hocaConfig, hocaDeclarations)
import Tct.Hoca.Types

instance Declared TrsProblem TrsProblem where
  decls = trsDeclarations

instance Declared ML ML where
  decls = hocaDeclarations

main :: IO ()
main = runTct hocaConfig

