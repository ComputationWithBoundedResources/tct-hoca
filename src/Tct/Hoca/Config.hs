module Tct.Hoca.Config where

import           Tct.Core.Common.Error    (catchError)

import           Tct.Core
import qualified Tct.Core.Data            as T
import qualified Tct.Hoca.Strategies      as S
import           Tct.Hoca.Types
import           Tct.Trs.Data
import qualified Tct.Trs.Strategy.Runtime as RC


hocaDeclarations :: T.Declared TrsProblem TrsProblem => [StrategyDeclaration ML ML]
hocaDeclarations = [ hocaDefault, hocaDefunctionalize ]

fun :: Argument 'Optional (Maybe String)
fun = some (string "function" ["The analysed ML function.",  "It defaults to the last defined function."])
  `optional` Nothing

tctStrategy :: T.Declared TrsProblem TrsProblem => Argument 'Required TrsStrategy
tctStrategy = strat "tct-strategy" ["The TRS strategy to apply after a successfull transformation."]

hocaDefault :: T.Declared TrsProblem TrsProblem => StrategyDeclaration ML ML
hocaDefault = SD $ strategy "hoca" (fun, tctStrategy) $ \ mn solve ->
  S.hoca mn .>>> solve .>>> abort

hocaDefunctionalize :: T.Declared TrsProblem TrsProblem => StrategyDeclaration ML ML
hocaDefunctionalize = SD $ strategy "defunctionalize" (fun, tctStrategy) $ \ mn solve ->
  S.hocaDefunctionalize mn .>>> solve .>>> abort

hocaConfig :: TctConfig ML
hocaConfig =
  (defaultTctConfig parser) { defaultStrategy = S.hoca Nothing .>>> RC.runtime .>>> abort }
  where
    parser fn = (Right <$> ML fn <$> readFile fn) `catchError` (return . Left . show)

