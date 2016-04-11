-- | This module provides Hoca specific commands for the interactive mode.
module Tct.Hoca.Interactive
  ( module M

  , loadML
  , listML
  , parseML
  ) where

import           Tct.Core.Interactive as M
import           Tct.Hoca.Processors  as M
import           Tct.Hoca.Strategies  as M

import qualified Tct.Core             as T
import           Tct.Hoca.Config      (hocaConfig)
import           Tct.Hoca.Types       (ML)


loadML :: FilePath -> IO ()
loadML = load' hocaConfig

-- | Parse 'ML strategy. Specialised version of 'parse'.
parseML :: T.Declared ML ML => String -> T.Strategy ML ML
parseML = parse (T.decls :: [T.StrategyDeclaration ML ML])

-- | List 'ML strategies. Specialised version of 'list'.
listML :: T.Declared ML ML => IO ()
listML = list (T.decls :: [T.StrategyDeclaration ML ML])

