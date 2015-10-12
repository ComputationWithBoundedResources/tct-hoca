{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Tct.Hoca.Types (
  ML (..)
  , PCF (..)
  , TypedPCF (..)
  , RewriteSystem (..)
  , TRS
  , ATRS
  , TrsProblem
  ) where

import           Hoca.Data.Symbol                as Symbol
import qualified Hoca.PCF.Core                   as PCF
import qualified Hoca.PCF.Sugar.Types            as PCFS
import qualified Hoca.Problem                    as Prob
import qualified Tct.Core.Common.Xml             as Xml
import           Tct.Trs.Data.Problem (TrsProblem)
import qualified Text.PrettyPrint.ANSI.Leijen    as PP

data ML = ML { file :: FilePath, source :: String }
instance Show ML      where show     = show . source
instance PP.Pretty ML where pretty   = PP.vcat . map PP.pretty . lines . source
instance Xml.Xml ML   where toXml ml = Xml.elt "ml" [Xml.text $ show ml]

type PCF = PCF.Program PCFS.Context
instance Show PCF      where show p         = show $ PP.pretty p
instance Xml.Xml PCF   where toXml p        = Xml.elt "pcf" [Xml.text $ show p]

type TypedPCF = PCF.TypedProgram PCFS.Context
instance Show TypedPCF      where show p         = show $ PP.pretty p
instance Xml.Xml TypedPCF   where toXml p        = Xml.elt "typed-pcf" [Xml.text $ show p]


type RewriteSystem f = Prob.Problem f Int

instance (Eq f, PP.Pretty f) => Show (RewriteSystem f) where show p = show $ PP.pretty p
instance (Eq f, PP.Pretty f) => Xml.Xml (RewriteSystem f) where toXml p = Xml.elt "atrs" [Xml.text $ show p]

type ATRS = Prob.Problem Symbol.Symbol Int
type TRS = Prob.Problem Symbol.TRSSymbol Int
