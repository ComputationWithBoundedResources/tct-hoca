module Tct.Hoca.Strategies where

import Tct.Core
import Tct.Core.Processor.Cast (close)

import Tct.Hoca.Types
import Tct.Hoca.Processors

import Tct.Trs.Data.Problem (TrsProblem)
import qualified Text.PrettyPrint.ANSI.Leijen    as PP

import qualified Hoca.Transform.Inlining as Inline
import qualified Hoca.Transform.Instantiate as Instantiate



-- * Processors from Hoca

desugar :: Maybe String -> Strategy ML TypedPCF
desugar = hocaStrategy . Desugar

defunctionalize :: Strategy TypedPCF ATRS
defunctionalize = hocaStrategy Defunctionalization  

neededRules :: Strategy ATRS ATRS
neededRules = hocaStrategy NeededRules  

uncurryATRS :: Strategy ATRS TRS
uncurryATRS = hocaStrategy UncurryATRS

inline,rewrite :: (PP.Pretty f, Ord f) => String -> Inline.Selector f Int -> Strategy (RewriteSystem f) (RewriteSystem f)
inline n f = hocaStrategy (Inline InlineFull (InlineSelector n f))
rewrite n f = hocaStrategy (Inline InlineRewrite  (InlineSelector n f))

usableRules, usableRulesFast :: (PP.Pretty f, Ord f) => Strategy (RewriteSystem f) (RewriteSystem f)
usableRules = hocaStrategy (UsableRules DFA)
usableRulesFast = hocaStrategy (UsableRules Syntactic)

dfa :: (Show f, PP.Pretty f, Ord f) => String -> Instantiate.RefineP f -> Strategy (RewriteSystem f) (RewriteSystem f)
dfa n f = hocaStrategy (CFA (CFARefinement n f))

compress :: (PP.Pretty f, Ord f) => Strategy (RewriteSystem f) (RewriteSystem f)
compress = hocaStrategy Compression  

toTctProblem :: (Show f, PP.Pretty f, Ord f) => Strategy (RewriteSystem f) TrsProblem
toTctProblem = hocaStrategy ToTctProblem

-- * Derived Strategies

mlToATRS :: Maybe String -> Strategy ML ATRS
mlToATRS mn = desugar mn >=> defunctionalize

simplifyATRS :: Strategy ATRS ATRS
simplifyATRS =
  try (exhaustively (rewrite "inline non-recursive" (Inline.withRule Inline.lambdaRule)))
  >>> try (exhaustively (inline "inline case-expression" (Inline.withRule Inline.caseRule)))
  >>> try usableRulesFast

cfa :: (Show f, PP.Pretty f, Ord f) => Strategy (RewriteSystem f) (RewriteSystem f)
cfa = dfa "control flow analysis" (Instantiate.refineND `Instantiate.orRefine` Instantiate.refineHOVars)

simplifyTRS :: (Show f, PP.Pretty f, Ord f) => Strategy (RewriteSystem f) (RewriteSystem f)
simplifyTRS = 
  try (exhaustively (inline "inline constructors" (Inline.withRule Inline.leafRule)) >>> try usableRulesFast) 
  >>> try (exhaustively ((inline "inline decreasing" Inline.decreasing <|> usableRules) >>> try usableRulesFast))
  >>> try compress

hocaDefunctionalize :: ProofData a => Maybe String -> Strategy TrsProblem a -> Strategy ML ML
hocaDefunctionalize mn solve = mlToATRS mn >=> toTctProblem >=> solve >=> close

hoca :: ProofData a => Maybe String -> Strategy TrsProblem a -> Strategy ML ML
hoca mn solve =
  mlToATRS mn
  >=> try simplifyATRS
  >=> try neededRules
  >=> try cfa
  >=> uncurryATRS
  >=> try usableRulesFast
  >=> try simplifyTRS
  >=> toTctProblem
  >=> solve >=> close



