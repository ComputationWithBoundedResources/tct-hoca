{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Tct.Hoca.Processors
       (
         -- * Processor Implementation of Hoca Library
         Desugar (..)
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

import           Tct.Hoca.Types

import qualified Tct.Core.Common.Xml          as Xml
import           Tct.Core.Data

import qualified Data.Rewriting.Rule          as Rule
import qualified Data.Rewriting.Term          as Term
import qualified Tct.Trs.Data.DependencyGraph as DPG
import qualified Tct.Trs.Data.Problem         as P
import qualified Tct.Trs.Data.ProblemKind     as P
import qualified Tct.Trs.Data.Rules           as Trs
import qualified Tct.Trs.Data.Signature       as Sig (constructors, defineds)
import qualified Tct.Trs.Data.Symbol          as TrsSymbol

import qualified Hoca.Data.MLTypes            as Hoca.Instantiate (Type)
import qualified Hoca.PCF.Core.DMInfer        as Hoca.DMInfer
import qualified Hoca.PCF.Desugar             as Hoca
import qualified Hoca.PCF.Sugar               as Hoca
import qualified Hoca.Problem                 as Hoca
import qualified Hoca.Problem.DFA             as Hoca.Instantiate (DFAGrammar)
import           Hoca.Transform               ((:=>))
import qualified Hoca.Transform               as Hoca
import qualified Hoca.Transform.Inlining      as Hoca.Inline
import qualified Hoca.Transform.Instantiate   as Hoca.Instantiate

import qualified Data.Set                     as Set
import           Data.Typeable                (Typeable)
import qualified Text.PrettyPrint.ANSI.Leijen as PP


-- utilities

data None = None deriving Show
instance Xml.Xml None where toXml _ = Xml.elt "none" []
instance PP.Pretty None where pretty = const PP.empty


runHocaWith :: (Processor p, Forking p ~ Id) => (r -> (ProofObject p, Out p)) -> (In p :=> r) -> p -> In p -> TctM (Return p)
runHocaWith f t _ prob =
  case Hoca.run t prob of
    Nothing -> abortWith (PP.text "transformation inapplicable")
    Just r -> succeedWith1 (fst r') fromId (snd r') where r' = f r

runHoca :: (Processor p, Forking p ~ Id, ProofObject p ~ None) => (In p Hoca.:=> Out p) -> p -> In p -> TctM (Return p)
runHoca = runHocaWith ((,) None)

-- * Hoca transformations as processors
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
  pretty (DesugarParseError pp) = PP.text "The given ML program cannot be parsed:" PP.<//> PP.indent 2 pp
  pretty (DesugarFailed pp) = pp
  pretty (DesugarTypingError pp) = PP.text "The given ML program cannot be typed:" PP.<//> PP.indent 2 pp

instance Processor Desugar where
  type In Desugar          = ML
  type Out Desugar         = TypedPCF
  type ProofObject Desugar = None

  execute p ml = toResult $ do
    sugared <- Hoca.programFromString (file ml) (source ml) `catchErr` (DesugarParseError . PP.text)
    desugared <- Hoca.desugar (analysedFunction p) sugared `catchErr` (DesugarFailed . PP.text)
    typed <- Hoca.DMInfer.infer desugared `catchErr` (DesugarTypingError . PP.pretty)
    return typed
    where
      catchErr r toErr = either (Left . toErr) Right r
      toResult = either abortWith (succeedWith1 None fromId)

-- ** defunctionalisation
----------------------------------------------------------------------

data Defunctionalization = Defunctionalization deriving Show

instance Processor Defunctionalization where
  type In Defunctionalization          = TypedPCF
  type Out Defunctionalization         = ATRS
  type ProofObject Defunctionalization = None
  execute = runHoca Hoca.defunctionalize

-- ** needed rules
----------------------------------------------------------------------

data NeededRules = NeededRules deriving Show

instance Processor NeededRules where
  type In NeededRules          = ATRS
  type Out NeededRules         = ATRS
  type ProofObject NeededRules = None
  execute = runHoca Hoca.neededRules

-- ** uncurrying
----------------------------------------------------------------------

data UncurryATRS = UncurryATRS deriving Show

instance Processor UncurryATRS where
  type In UncurryATRS          = ATRS
  type Out UncurryATRS         = TRS
  type ProofObject UncurryATRS = None
  execute = runHoca Hoca.uncurried

-- ** inlining
----------------------------------------------------------------------

data InlineSelector f  = InlineSelector String (Hoca.Inline.Selector f Int)

instance Show (InlineSelector f) where show (InlineSelector n _) = "<" ++ n ++ ">"

data InlineType = InlineFull | InlineRewrite deriving Show
data Inline f = Inline { inlineType :: InlineType, inlineSelect :: InlineSelector f } deriving Show

instance (PP.Pretty f, Ord f, Typeable f) => Processor (Inline f) where
  type In (Inline f)          = RewriteSystem f
  type Out (Inline f)         = RewriteSystem f
  type ProofObject (Inline f) = None
  execute t = runHoca (inliner selector) t where
    inliner = case inlineType t of { InlineFull -> Hoca.inline; InlineRewrite -> Hoca.rewrite }
    selector = case inlineSelect t of { InlineSelector _ f -> f }

-- ** usable rules
----------------------------------------------------------------------

data URType = Syntactic | DFA deriving (Show, Eq)
data UsableRules f = UsableRules { urType :: URType } deriving Show

instance (PP.Pretty f, Ord f, Typeable f) => Processor (UsableRules f) where
  type In (UsableRules f)          = RewriteSystem f
  type Out (UsableRules f)         = RewriteSystem f
  type ProofObject (UsableRules f) = None
  execute t = runHoca usableRules t where
    usableRules | urType t == DFA = Hoca.usableRulesDFA
                | otherwise       = Hoca.usableRulesSyntactic

-- ** instantiation
----------------------------------------------------------------------

data CFARefinement f = CFARefinement String (Hoca.Instantiate.RefineP f)
type Automaton f = Hoca.Instantiate.DFAGrammar f Int Hoca.Instantiate.Type
data CFA f = CFA { cfaRefinement :: CFARefinement f } deriving Show

instance Show (CFARefinement f) where show (CFARefinement n _) = "<" ++ n ++ ">"
instance Show f => Xml.Xml (Automaton f)  where
  toXml dfa = Xml.elt "dfa" [Xml.text (show dfa)]

instance (Show f, PP.Pretty f, Ord f, Typeable f) => Processor (CFA f) where
  type In (CFA f)          = RewriteSystem f
  type Out (CFA f)         = RewriteSystem f
  type ProofObject (CFA f) = Automaton f
  execute t = runHocaWith id (Hoca.Instantiate.instantiate' refinement) t where
    refinement = case cfaRefinement t of { CFARefinement _ f -> f }

-- ** closure compression
----------------------------------------------------------------------

data Compression f = Compression deriving Show

instance (PP.Pretty f, Ord f, Typeable f) => Processor (Compression f) where
  type In (Compression f )         = RewriteSystem f
  type Out (Compression f)         = RewriteSystem f
  type ProofObject (Compression f) = None
  execute = runHoca Hoca.compress

-- ** transform Hoca problem to Tct/Trs Problem
----------------------------------------------------------------------

data ToTctProblem f = ToTctProblem deriving Show

instance (PP.Pretty f, Ord f, Typeable f) => Processor (ToTctProblem f) where
  type In (ToTctProblem f )         = RewriteSystem f
  type Out (ToTctProblem f)         = TrsProblem
  type ProofObject (ToTctProblem f) = None
  execute _ p = succeedWith1 None fromId trsProb where
    trsProb = P.Problem { P.startTerms = P.BasicTerms ds cs
                        , P.strategy   = P.Innermost
                        , P.signature  = Trs.signature trs
                        , P.strictDPs  = Trs.empty
                        , P.strictTrs  = trs
                        , P.weakDPs    = Trs.empty
                        , P.weakTrs    = Trs.empty
                        , P.dpGraph    = DPG.empty }
    trs = Trs.fromList [ Rule.Rule (toTctTerm l) (toTctTerm r)
                       | Rule.Rule l r <- Hoca.theRule `map` Hoca.rules p ]
    sig = Trs.signature trs
    ds  = toTctFuns (Hoca.defs (Hoca.startTerms p))    `Set.intersection` Sig.defineds sig
    cs  = toTctFuns (Hoca.constrs (Hoca.startTerms p)) `Set.intersection` Sig.constructors sig
    toTctTerm = Term.map toTctFun toTctVar
    toTctFuns = Set.fromList . map toTctFun
    toTctFun f = TrsSymbol.fun (PP.displayS (PP.renderCompact (PP.pretty f)) "")
    toTctVar v = TrsSymbol.var ("x" ++ show v)

