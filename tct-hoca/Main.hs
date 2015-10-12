module Main where

import Control.Monad.Error (catchError)
import Tct.Core
import Tct.Core.Main.Mode
import Tct.Core.Main.Options
import qualified Tct.Hoca.Strategies as S
import qualified Tct.Core.Parse as TP
import qualified Tct.Core.Common.Parser as TP
import qualified Tct.Trs.Processor as T (defaultDeclarations)
import qualified Tct.Trs.Data as T (TrsStrategy)
import Tct.Hoca.Types
import qualified Tct.RC as RC

funArg = string `withName` "function"
                `withHelp` ["The analysed ML function.",  "It defaults to the last defined function."]

trsStrategyArg :: Argument Required T.TrsStrategy
trsStrategyArg = strat `withName` "tct-strategy"
                       `withHelp` ["The TRS strategy to apply."]

-- MA: if possible, this should go to tct-trs; it should also include all the strategies of tct-mode
instance TP.SParsable i o T.TrsStrategy where
  parseS = TP.withState ds TP.strategy
    where ds = SD RC.runtimeSD : T.defaultDeclarations


hocaDefault :: StrategyDeclaration ML ML
hocaDefault = SD $ strategy "hoca" (optional (some funArg) Nothing, trsStrategyArg) S.hoca 

hocaDefunctionalize :: StrategyDeclaration ML ML 
hocaDefunctionalize = SD $ strategy "defunctionalize" (optional (some funArg) Nothing, trsStrategyArg) S.hocaDefunctionalize

hocaMode :: TctMode ML ML ()
hocaMode =
  TctMode { modeId = "tct-hoca"
          , modeParser = \ fn -> (Right <$> ML fn <$> readFile fn) `catchError` (return . Left . show)
          , modeDefaultStrategy = S.hoca Nothing RC.runtime
          , modeOptions = unit
          , modeModifyer = const id
          , modeStrategies = [hocaDefault, hocaDefunctionalize]
          , modeAnswer = \ _ _ -> return ()
          , modeProof = \ _ _ -> return ()
          }

main :: IO ()
main = hocaMode `setModeWith` defaultTctConfig { recompile = False }
