module Tct.Hoca.Strategies where

import Tct.Core


import Tct.Hoca.Types
import Tct.Hoca.Processors

import qualified Text.PrettyPrint.ANSI.Leijen    as PP

import qualified Hoca.Transform.Inlining as Inline
import qualified Hoca.Transform.Instantiate as Instantiate

-- * Processors from Hoca

desugar :: Maybe String -> Strategy ML TypedPCF
desugar = processor . Desugar

defunctionalize :: Strategy TypedPCF ATRS
defunctionalize = processor Defunctionalization  

neededRules :: Strategy ATRS ATRS
neededRules = processor NeededRules  

uncurryATRS :: Strategy ATRS TRS
uncurryATRS = processor UncurryATRS

inline,rewrite :: (PP.Pretty f, Ord f) => String -> Inline.Selector f Int -> Strategy (RewriteSystem f) (RewriteSystem f)
inline n f = processor (Inline InlineFull (InlineSelector n f))
rewrite n f = processor (Inline InlineRewrite  (InlineSelector n f))

usableRules, usableRulesFast :: (PP.Pretty f, Ord f) => Strategy (RewriteSystem f) (RewriteSystem f)
usableRules = processor (UsableRules DFA)
usableRulesFast = processor (UsableRules Syntactic)

dfa :: (Show f, PP.Pretty f, Ord f) => String -> Instantiate.RefineP f -> Strategy (RewriteSystem f) (RewriteSystem f)
dfa n f = processor (CFA (CFARefinement n f))

compress :: (PP.Pretty f, Ord f) => Strategy (RewriteSystem f) (RewriteSystem f)
compress = processor Compression  

toTctProblem :: (Show f, PP.Pretty f, Ord f) => Strategy (RewriteSystem f) TrsProblem
toTctProblem = processor ToTctProblem

-- * Derived Strategies

mlToATRS :: Maybe String -> Strategy ML ATRS
mlToATRS maybeFunName = desugar maybeFunName .>>> defunctionalize

simplifyATRS :: Strategy ATRS ATRS
simplifyATRS =
  try (exhaustively (rewrite "inline non-recursive" (Inline.withRule Inline.lambdaRule)))
  .>>> try (exhaustively (inline "inline case-expression" (Inline.withRule Inline.caseRule)))
  .>>> try usableRulesFast
  .>>> try neededRules

cfa :: (Show f, PP.Pretty f, Ord f) => Strategy (RewriteSystem f) (RewriteSystem f)
cfa = dfa "control flow analysis" (Instantiate.refineND `Instantiate.orRefine` Instantiate.refineHOVars)

simplifyTRS :: (Show f, PP.Pretty f, Ord f) => Strategy (RewriteSystem f) (RewriteSystem f)
simplifyTRS = 
  try (exhaustively (inline "inline constructors" (Inline.withRule Inline.leafRule)) .>>> try usableRulesFast) 
  .>>> try (exhaustively ((inline "inline decreasing" Inline.decreasing .<|> usableRules) .>>> try usableRulesFast))
  .>>> try compress

hocaDefunctionalize :: Maybe String -> Strategy ML TrsProblem
hocaDefunctionalize mn = mlToATRS mn .>>> toTctProblem

hoca :: Maybe String -> Strategy ML TrsProblem
hoca maybeFunName =
  mlToATRS maybeFunName
  .>>> try simplifyATRS
  .>>> try cfa
  .>>> uncurryATRS
  .>>> try simplifyTRS
  .>>> toTctProblem



