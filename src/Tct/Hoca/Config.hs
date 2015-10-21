module Tct.Hoca.Config where

import Control.Monad.Error (catchError)
import Tct.Core
import Tct.Core.Main
import qualified Tct.Hoca.Strategies as S
import qualified Tct.Trs.Strategy.Runtime as RC

import qualified Tct.Core.Parse as TP
import qualified Tct.Core.Common.Parser as TP
import Tct.Trs.Data (TrsStrategy)
import Tct.Trs.Declarations () -- TODO

import Tct.Hoca.Types


fun :: Argument Optional (Maybe String)
fun = optional (some string) Nothing
      `withName` "function"
      `withHelp` ["The analysed ML function.",  "It defaults to the last defined function."]

tctStrategy :: Argument Required TrsStrategy
tctStrategy = strat `withName` "tct-strategy"
                       `withHelp` ["The TRS strategy to apply after a successfull transformation."]

hocaDefault :: StrategyDeclaration ML ML
hocaDefault = SD $ strategy "hoca" (fun, tctStrategy) $ \ mn solve ->
  S.hoca mn .>>> solve .>>> abort

hocaDefunctionalize :: StrategyDeclaration ML ML
hocaDefunctionalize = SD $ strategy "defunctionalize" (fun, tctStrategy) $ \ mn solve ->
  S.hocaDefunctionalize mn .>>> solve .>>> abort

hocaConfig :: TctConfig ML
hocaConfig =
  (defaultTctConfig parser) { strategies = [hocaDefault, hocaDefunctionalize]
                            , defaultStrategy = S.hoca Nothing .>>> RC.runtime .>>> abort }
  where
    parser fn = (Right <$> ML fn <$> readFile fn) `catchError` (return . Left . show)
