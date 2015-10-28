{-# LANGUAGE MultiParamTypeClasses #-}
module Tct.Hoca.Config where

import Control.Monad.Error (catchError)
import Tct.Core
import qualified Tct.Core.Data as T
import qualified Tct.Hoca.Strategies as S
import qualified Tct.Trs.Strategy.Runtime as RC

import Tct.Trs.Data (TrsStrategy)
import Tct.Trs.Declarations () -- TODO

import Tct.Hoca.Types

import qualified Tct.Core.Data as T

import Tct.Trs.Data
-- import qualified Tct.Core.Data as T

-- import Tct.Trs.Data
-- import Tct.Trs.Strategy.Web

-- instance T.Declared TrsProblem TrsProblem where
--   decls = [T.SD webDeclaration]

fun :: Argument 'Optional (Maybe String)
fun = some (string "function" ["The analysed ML function.",  "It defaults to the last defined function."])
  `optional` Nothing

-- tctStrategy :: T.Declared TrsProblem TrsProblem => Argument 'Required TrsStrategy
tctStrategy :: T.Declared TrsProblem TrsProblem => Argument 'Required TrsStrategy
tctStrategy = strat "tct-strategy" ["The TRS strategy to apply after a successfull transformation."]

instance (T.Declared ML ML , T.Declared TrsProblem TrsProblem) => T.DefaultDeclared ML ML where
  defaultDecls = [hocaDefault, hocaDefunctionalize]

hocaDefault :: (T.Declared ML ML, T.Declared TrsProblem TrsProblem) => StrategyDeclaration ML ML
-- hocaDefault = SD $ strategy "hoca" (fun, tctStrategy) $ \ mn solve ->
  -- S.hoca mn .>>> solve .>>> abort
hocaDefault = SD $ strategy "hoca" (OneTuple tctStrategy) $ \ solve ->
  S.hoca Nothing .>>> solve .>>> abort

hocaDefunctionalize :: (T.Declared ML ML, T.Declared TrsProblem TrsProblem) => StrategyDeclaration ML ML
-- hocaDefunctionalize = SD $ strategy "defunctionalize" (fun, tctStrategy) $ \ mn solve ->
  -- S.hocaDefunctionalize mn .>>> solve .>>> abort
hocaDefunctionalize = SD $ strategy "defunctionalize" (OneTuple tctStrategy) $ \ solve ->
  S.hocaDefunctionalize Nothing .>>> solve .>>> abort

hocaConfig :: (T.Declared ML ML, T.Declared TrsProblem TrsProblem) => TctConfig ML
hocaConfig =
  (defaultTctConfig parser) { strategies = [hocaDefault, hocaDefunctionalize]
                            , defaultStrategy = S.hoca Nothing .>>> RC.runtime .>>> abort }
  where
    parser fn = (Right <$> ML fn <$> readFile fn) `catchError` (return . Left . show)
