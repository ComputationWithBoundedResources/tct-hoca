module Main where

import Tct.Hoca.Config (hocaConfig)
import Tct.Core.Main (tct3)

main :: IO ()
main = tct3 hocaConfig
