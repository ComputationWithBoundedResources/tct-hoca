{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Tct.Hoca.Processors
       (
         HocaProcessor
         , hocaStrategy
         -- * Processor Implementation of Hoca Library
         , Desugar (..)
         , Defunctionalization (..)
         , NeededRules (..) 
         , UncurryATRS (..)
         , Inline (..)
         , InlineType (..)
         , InlineSelector (..)
         , UsableRules (..)
         , URType (..)
         , CFA (..)
         , CFARefinement (..)
         , Compression (..)
         , ToTctProblem (..)
       )
       where

import Tct.Hoca.Types

import qualified Tct.Core.Common.Xml             as Xml
import qualified Text.PrettyPrint.ANSI.Leijen    as PP

import Tct.Core.Data
import qualified Tct.Trs.Data.ProblemKind as Problem
import qualified Tct.Trs.Data.Problem as Problem
import qualified Tct.Trs.Data.Trs as Trs
import qualified Tct.Trs.Data.Symbol          as TrsSymbol
import qualified Tct.Trs.Data.DependencyGraph as DPG
import qualified Data.Rewriting.Rule          as Rule
import qualified Data.Rewriting.Term          as Term

import qualified Hoca.Problem as Hoca
import qualified Hoca.Transform as Hoca
import qualified Hoca.Transform.Inlining as Hoca.Inline
import qualified Hoca.PCF.Sugar as Hoca
import qualified Hoca.PCF.Desugar as Hoca
import qualified Hoca.PCF.Core.DMInfer as Hoca.DMInfer
import qualified Hoca.Transform.Instantiate as Hoca.Instantiate
import qualified Hoca.Problem.DFA as Hoca.Instantiate (DFAGrammar)
import qualified Hoca.Data.MLTypes as Hoca.Instantiate (Type)
import qualified Data.Set as Set

----------------------------------------------------------------------
-- * General Type class for Hoca Transformations
----------------------------------------------------------------------

-- All Hoca techniques are complexity relecting, and return precisely one
-- sub-problem. We reflect this in the following class. 

data HocaResult p = HocaAbort (HocaError p) | HocaReturn (HocaData p) (HocaOut p)

toResult :: (HocaError p ~ None, HocaData p ~ None) => Maybe (HocaOut p) -> HocaResult p
toResult = maybe (HocaAbort None) (HocaReturn None)

class (Show p, ProofData (HocaIn p), ProofData (HocaOut p)
      , Show (HocaError p), PP.Pretty (HocaError p)
      , Show (HocaData p), PP.Pretty (HocaData p)) => HocaProcessor p where
  type HocaIn p :: *
  type HocaOut p :: *
  type HocaError p :: *
  type HocaData p :: *

  transform :: p -> HocaIn p -> TctM (HocaResult p)

  type HocaError p = None
  type HocaData p = None

-- the processor instance declaration

data None = None deriving Show
instance Xml.Xml None where toXml _ = Xml.elt "none" []
instance PP.Pretty None where pretty = const PP.empty

data HocaProofObject err obj = HocaError err | HocaSuccess obj deriving Show

instance (Show err, Show obj) => Xml.Xml (HocaProofObject err obj) where
  toXml (HocaError err) = Xml.elt "hocaError" [Xml.text (show err)]
  toXml (HocaSuccess obj) = Xml.elt "hocaSuccess" [Xml.text (show obj)]

instance (PP.Pretty err, PP.Pretty obj) => PP.Pretty (HocaProofObject err obj) where
  pretty (HocaError err) = PP.pretty err
  pretty (HocaSuccess obj) = PP.pretty obj

data HocaProc p = HocaProc p deriving Show

instance HocaProcessor p => Processor (HocaProc p) where
  type I (HocaProc p) = HocaIn p
  type O (HocaProc p) = HocaOut p
  type ProofObject (HocaProc p) = HocaProofObject (HocaError p) (HocaData p)

  solve p@(HocaProc t) i = resultToTree' p i <$> toResult' <$> transform t i where
    toResult' (HocaAbort err) = Fail (HocaError err)
    toResult' (HocaReturn obj d) = Success (toId d) (HocaSuccess obj) fromId

hocaStrategy :: HocaProcessor p => p -> Strategy (HocaIn p) (HocaOut p)
hocaStrategy = Proc . HocaProc


----------------------------------------------------------------------
-- * Hoca Transformations
----------------------------------------------------------------------


-- ** Desugaring and type checking
----------------------------------------------------------------------

data Desugar = Desugar { analysedFunction :: Maybe String } deriving Show

data DesugarError =
  DesugarParseError PP.Doc
  | DesugarFailed PP.Doc
  | DesugarTypingError PP.Doc
  deriving Show

instance PP.Pretty DesugarError where
  pretty (DesugarParseError pp) =
    PP.text "The given ML program cannot be parsed:"
    PP.<//> PP.indent 2 pp
  pretty (DesugarFailed pp) = pp
  pretty (DesugarTypingError pp) =
    PP.text "The given ML program cannot be typed:"
    PP.<//> PP.indent 2 pp

instance HocaProcessor Desugar where
  type HocaIn Desugar = ML
  type HocaOut Desugar = TypedPCF
  type HocaError Desugar = DesugarError

  transform p ml = return $ toResult' $ do
    sugared <- Hoca.programFromString (file ml) (source ml) `catchErr` (DesugarParseError . PP.text)
    desugared <- Hoca.desugar (analysedFunction p) sugared `catchErr` (DesugarFailed . PP.text)
    typed <- Hoca.DMInfer.infer desugared `catchErr` (DesugarTypingError . PP.pretty)
    return typed
    where
      catchErr r toErr = either (Left . toErr) Right r
      toResult' = either HocaAbort (HocaReturn None)
    
-- ** defunctionalisation
----------------------------------------------------------------------

data Defunctionalization = Defunctionalization deriving Show

instance HocaProcessor Defunctionalization where
  type HocaIn Defunctionalization = TypedPCF
  type HocaOut Defunctionalization = ATRS
  transform _ = return . toResult . Hoca.run Hoca.defunctionalize where

-- ** needed rules
----------------------------------------------------------------------

data NeededRules = NeededRules deriving Show

instance HocaProcessor NeededRules where
  type HocaIn NeededRules = ATRS
  type HocaOut NeededRules = ATRS
  transform _ = return . toResult . Hoca.run Hoca.compress where

-- ** uncurrying
----------------------------------------------------------------------

data UncurryATRS = UncurryATRS deriving Show

instance HocaProcessor UncurryATRS where
  type HocaIn UncurryATRS = ATRS
  type HocaOut UncurryATRS = TRS
  transform _ = return . toResult . Hoca.run Hoca.uncurried where

-- ** inlining
----------------------------------------------------------------------

data InlineSelector f  = InlineSelector String (Hoca.Inline.Selector f Int)

instance Show (InlineSelector f) where show (InlineSelector n _) = "<" ++ n ++ ">"

data InlineType = InlineFull | InlineRewrite deriving Show
data Inline f = Inline { inlineType :: InlineType, inlineSelect :: InlineSelector f } deriving Show

instance (PP.Pretty f, Ord f) => HocaProcessor (Inline f) where
  type HocaIn (Inline f) = RewriteSystem f
  type HocaOut (Inline f) = RewriteSystem f
  transform t = return . toResult . Hoca.run (inliner selector) where
    inliner = case inlineType t of { InlineFull -> Hoca.inline; InlineRewrite -> Hoca.rewrite }
    selector = case inlineSelect t of { InlineSelector _ f -> f }

-- ** usable rules
----------------------------------------------------------------------

data URType = Syntactic | DFA deriving Show
data UsableRules f = UsableRules { urType :: URType } deriving Show

instance (PP.Pretty f, Ord f) => HocaProcessor (UsableRules f) where
  type HocaIn (UsableRules f) = RewriteSystem f
  type HocaOut (UsableRules f) = RewriteSystem f
  transform t = return . toResult . Hoca.run (ur (urType t)) where
    ur Syntactic = Hoca.usableRulesSyntactic
    ur DFA = Hoca.usableRulesDFA

-- ** instantiation
----------------------------------------------------------------------

data CFARefinement f = CFARefinement String (Hoca.Instantiate.RefineP f)
instance Show (CFARefinement f) where show (CFARefinement n _) = "<" ++ n ++ ">"

data CFA f = CFA { cfaRefinement :: CFARefinement f } deriving Show

instance (Show f, PP.Pretty f, Ord f) => HocaProcessor (CFA f) where
  type HocaIn (CFA f) = RewriteSystem f
  type HocaOut (CFA f) = RewriteSystem f
  type HocaData (CFA f) = Hoca.Instantiate.DFAGrammar f Int Hoca.Instantiate.Type
  transform t = return . toResult' . Hoca.run (Hoca.Instantiate.instantiate' refinement) where
    refinement = case cfaRefinement t of { CFARefinement _ f -> f }
    toResult' Nothing = HocaAbort None
    toResult' (Just (grammar,res)) = HocaReturn grammar res

-- ** closure compression
----------------------------------------------------------------------

data Compression f = Compression deriving Show

instance (PP.Pretty f, Ord f) => HocaProcessor (Compression f) where
  type HocaIn (Compression f ) = RewriteSystem f
  type HocaOut (Compression f) = RewriteSystem f
  transform _ = return . toResult . Hoca.run Hoca.compress


-- ** transform Hoca problem to Tct/Trs Problem
----------------------------------------------------------------------

data ToTctProblem f = ToTctProblem deriving Show

instance (PP.Pretty f, Ord f) => HocaProcessor (ToTctProblem f) where
  type HocaIn (ToTctProblem f ) = RewriteSystem f
  type HocaOut (ToTctProblem f) = Problem.TrsProblem
  transform _ p =
    return $ toResult $ Just $ 
      Problem.Problem { Problem.startTerms   = Problem.BasicTerms defs constrs
                      , Problem.strategy = Problem.Innermost
                      , Problem.signature = Trs.signature trs
                      , Problem.strictDPs  = Trs.empty
                      , Problem.strictTrs  = trs
                      , Problem.weakDPs    = Trs.empty
                      , Problem.weakTrs    = Trs.empty
                      , Problem.dpGraph = DPG.empty }
    where 
      defs = Set.fromList [ toTctFun f | f <- Hoca.defs (Hoca.startTerms p) ]
      constrs = Set.fromList [ toTctFun f | f <- Hoca.constrs (Hoca.startTerms p) ]
      trs = Trs.fromList [ Rule.Rule (toTctTerm l) (toTctTerm r)
                         | Rule.Rule l r <- Hoca.theRule `map` Hoca.rules p ]
      toTctTerm = Term.map toTctFun toTctVar
      toTctFun f = TrsSymbol.fun (PP.displayS (PP.renderCompact (PP.pretty f)) "")
      toTctVar v = TrsSymbol.var ("x" ++ show v)
