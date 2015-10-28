module Main where

import Tct.Hoca.Config (hocaConfig)
import Tct.Hoca.Types
import Tct.Core.Main (tct3)

import qualified Tct.Core.Data as T

import Tct.Trs.Data
import Tct.Trs.Strategy.Web


instance T.Declared TrsProblem TrsProblem where
  decls = [T.SD webDeclaration]

instance T.Declared ML ML where

main :: IO ()
main = tct3 hocaConfig

