{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}

{- | This file provides a simple wrapper to integrate 'hoca' into the 'tct3' framework.

It encapsulates following 'hoca' functionalities via 'tct' transformations:
  * parse a subset of ML (like) functional programs
  * transform ML  to Programming Computable Functions (PCF)
  * transform PCF to Applicative Term Rewrite System (ATRS)
  * simplify ATRSs
  * transform ATRSs to TRSs (TRSs)

Furthermore it incorporates the 'tct-trs' module to analyse the resulting TRS, thus providing
  * a fully automated complexity analysis of ML (like) functional programs
  * a uniform proof output of the transformation from ML to TRS and the TRS techniques applied.
-}

import           Control.Applicative
import           Control.Monad                   (foldM, forM, void)
import           Data.Either                     (either)
import qualified Data.IntMap                     as IMap
import           Data.List                       (nub)
import           Data.Maybe                      (fromMaybe)
import           Data.Maybe                      (fromJust, isJust, mapMaybe)
import           Data.Monoid                     (mempty)
import           Data.Typeable
import           Prelude                         hiding (not, (&&), (||))
import qualified Prelude
import           System.Environment              (getArgs)
import           System.Exit                     (exitFailure, exitSuccess)
import           System.IO                       (hPutStrLn, stderr)

import qualified Text.PrettyPrint.ANSI.Leijen    as PP

import           Data.Rewriting.Applicative.Rule
import           Data.Rewriting.Applicative.Term

import           Hoca.Data.MLTypes
import           Hoca.Data.Symbol
import qualified Hoca.PCF.Core                   as PCF
import qualified Hoca.PCF.Core.DMInfer           as DM
import           Hoca.PCF.Desugar                (desugar, desugarExpression)
import           Hoca.PCF.Sugar                  (Context, Exp, expressionFromString, programFromString)
import           Hoca.Problem                    hiding (Problem, TRule)
import qualified Hoca.Problem                    as P
import           Hoca.Transform
import           Hoca.Utils                      (putDocLn, render, writeDocFile)


import qualified Data.Rewriting.Problem          as R (fromString)
import qualified Tct.Core                        as T
import qualified Tct.Core.Common.Parser          as TP
import qualified Tct.Core.Common.Xml             as Xml
import qualified Tct.Core.Data                   as T
import qualified Tct.Core.Main                   as T
import qualified Tct.Core.Parse                  as TP
import qualified Tct.Core.Processor.Cast         as T (close)
import qualified Tct.Trs                         as T hiding (strategy)
import qualified Tct.Trs.Data                    as T (TrsStrategy)
import qualified Tct.Trs.Processor               as T (defaultDeclarations)

import           RC                              (runtimeSD)


--- * main -----------------------------------------------------------------------------------------------------------

main :: IO ()
main =  hm `T.setModeWith`
  T.defaultTctConfig
    { T.recompile = False }

  where
    hm = hocaMode
      `T.withStrategies`
        [ T.SD hocaSD ]
      `T.withDefaultStrategy`
        T.deflFun hocaSD (T.deflFun runtimeSD)


--- * tct integration ------------------------------------------------------------------------------------------------
-- MS: The integration is very shallow. All transformations uses the `Transform` processor from the Tct.Core module.
-- This provides only a minimal output and no means to parse/describe the transformations/simplifications applied.
-- This is usually sufficient. For example, the only thing we do with PCF is to transform it to ATRS. There is no
-- reason to provide a parsable strategy for this. We just encapsulate the transformations in a big strategy that can
-- parse a TRS strategy.

-- define necessary problem types

data ML = ML { source :: String, mname :: Maybe String }
instance Show ML      where show     = show . source
instance PP.Pretty ML where pretty   = PP.vcat . map PP.pretty . lines . source
instance Xml.Xml ML   where toXml ml = Xml.elt "ml" [Xml.text $ show ml]

newtype PCF  = PCF (PCF.Program Context)
instance PP.Pretty PCF where pretty (PCF p) = PP.pretty p
instance Show PCF      where show p         = show $ PP.pretty p
instance Xml.Xml PCF   where toXml p        = Xml.elt "pcf" [Xml.text $ show p]

newtype ATRS = ATRS Problem
instance PP.Pretty ATRS where pretty (ATRS p) = PP.pretty p
instance Show ATRS      where show p          = show $ PP.pretty p
instance Xml.Xml ATRS   where toXml p         = Xml.elt "atrs" [Xml.text $ show p]

-- set problem specific problem parser and options

type HocaMode     = T.TctMode ML ML (Maybe String)
type HocaStrategy = T.Strategy ML ML

hocaMode :: HocaMode
hocaMode = T.TctMode
  { T.modeId              = "hoca"
  , T.modeParser          = \fn -> readFile fn >>= \s -> return (Right (ML s Nothing))
  , T.modeStrategies      = []

  , T.modeDefaultStrategy = T.failing
  , T.modeOptions         =
      optional (T.option $ T.eopt
        `T.withArgLong` "mname"
        `T.withHelpDoc` PP.text "The method name.")
  , T.modeModifyer        = \mnM ml -> case mnM of
      Nothing -> ml
      mn      -> ml { mname = mn }
  , T.modeAnswer          = \_ _  -> return ()
  , T.modeProof           = \_ _ -> return () }

-- strategies

hocaSD = T.strategy "hoca" (simplifyArg `T.optional` Simplify, trsArg) hocaStrategy

hocaStrategy :: Simplify -> T.TrsStrategy -> HocaStrategy
hocaStrategy smp trs =
  ml2pcf T.>=> pcf2atrs T.>=> T.when (smp == Simplify) (T.try atrsSimplify) T.>=> atrs2trs T.>=> trs T.>=> T.close

-- transformations (processors with default minimal proof output)

noteE err = either (const $ Left err) Right
noteM err = maybe (Left err) Right

ml2pcf :: T.Strategy ML PCF
ml2pcf = T.transform "We desugar source code to PCF." $ \ml ->
  programFromString "ml" (source ml) >>= desugar (mname ml) >>= return . PCF

pcf2atrs :: T.Strategy PCF ATRS
pcf2atrs = T.transform "We defunctinoalise PCF to ATRS." $ \(PCF pcf) ->
  noteE "Could not type the PCF program" (DM.infer pcf)
  >>= noteM "Could not defunctionalise the PCF program." . run defunctionalize
  >>= return . ATRS

atrsSimplify :: T.Strategy ATRS ATRS
atrsSimplify = T.transform "We simplify the ATRS."$ \(ATRS atrs) ->
  noteM " Could not simplify the ATRS." (run simplify atrs)
  >>= return . ATRS

atrs2trs :: T.Strategy ATRS T.TrsProblem
atrs2trs = T.transform "We defunctionalise the ATRS to TRS." $ \(ATRS atrs) ->
  noteE "Could not transform the ATRS to TRS." (R.fromString . show $ prettyWST atrs)
  >>= T.fromRewriting
  >>= return . T.toInnermost

-- arguments

data Simplify = Simplify | NoSimplify deriving (Bounded, Enum, Eq, Typeable, Show)
instance T.SParsable i i Simplify where parseS = TP.enum

simplifyArg = T.arg
  `T.withName`   "simplify"
  `T.withHelp`   ["Apply simplifications."]
  `T.withDomain` fmap show [(minBound :: Simplify)..]

trsArg = T.arg
  `T.withName`   "trs"
  `T.withHelp`   ["The TRS strategy to apply."]
  `T.withDomain` ["<trs>"]

-- currently we can not parse transformations; t1 >=> t2
-- to parse a trs strategies we make a dedicated Parsable instance for the considered problem type (here pcf)
instance T.SParsable ML ML T.TrsStrategy where
  parseS = TP.withState ds TP.strategy
    where ds = T.SD runtimeSD : T.defaultDeclarations


--- * hoca configuration ---------------------------------------------------------------------------------------------
-- MS: stolen from pcf2trs executable; unmodified

type Problem = P.Problem Symbol Int
type TRule   = P.TRule Symbol Int

class Boolean a where
  (&&) :: a -> a -> a
  (||) :: a -> a -> a
  not :: a -> a

infixr 3 &&
infixr 2 ||

instance Boolean Bool where
  (&&) = (Prelude.&&)
  (||) = (Prelude.||)
  not = Prelude.not

instance Boolean b => Boolean (a -> b) where
  f && g = \ a -> f a && g a
  f || g = \ a -> f a || g a
  not f = not . f


headSymbolSatisfies :: (Symbol -> Bool) -> Problem -> ARule Symbol Int -> Bool
headSymbolSatisfies p _ rl =
  case unlabeled <$> headSymbol (lhs rl) of
   Just f -> p f
   _ -> False

anyRule, caseRule, lambdaRule, fixRule :: Problem -> ARule Symbol Int -> Bool
caseRule = headSymbolSatisfies p where
   p Cond {} = True
   p _ = False

lambdaRule = headSymbolSatisfies p where
   p Lambda {} = True
   p _ = False

fixRule = headSymbolSatisfies p where
   p Fix {} = True
   p _ = False
anyRule _ _ = True

definingRule :: String -> Problem -> TRule -> Bool
definingRule name _ (theRule -> rl) =
  case unlabeled <$> headSymbol (lhs rl) of
   Just f -> render f == name
   _ -> False

-- recursiveRule = isRecursive


-- oneOfIdx :: [Int] -> Problem -> Rule -> Bool
-- oneOfIdx is p r = maybe False (`elem` is) (indexOf p r)

leafRule :: (Eq f, Eq v) => P.Problem f v -> ARule f v -> Bool
leafRule p r = maybe True (null . cgSuccs p) (indexOf p r)


tsize :: ATerm f v -> Int
tsize = fold (const 1) (const ((+1) . sum))

type NR f v = NarrowedRule (ASym f) v v
sizeDecreasing :: P.Problem f v -> NR f v -> Bool
sizeDecreasing _ ns = all (\ n -> sz (narrowing n) < sz (narrowedRule ns)) (narrowings ns) where
  sz rl = tsize (rhs rl)

-- sizeNonIncreasing :: Problem -> NarrowedRule -> Bool
-- sizeNonIncreasing _ ns = all (\ n -> sz (N.narrowing n) <= sz (N.narrowedRule ns)) (N.narrowings ns) where
--   sz rl = size (R.lhs rl) + size (R.rhs rl)

-- branching :: Problem -> NarrowedRule -> Bool
-- branching _ ns = length (N.narrowings ns) > 1

-- selfInlining :: Problem -> NarrowedRule -> Bool
-- selfInlining _ ns = N.narrowedRule ns `elem` map N.narrowedWith (N.narrowings ns)

ruleDeleting :: (Eq f, Eq v) => P.Problem f v-> NR f v -> Bool
ruleDeleting p ns =
  case nub (concatMap (cgPreds p) nwIds) of
   [i] -> i `notElem` nwIds
   _ -> False
   where
     nwIds = mapMaybe (indexOf p . narrowedWith) (narrowings ns)

-- withRule,onRule :: (Problem -> Rule -> Bool) -> Problem -> NarrowedRule -> Bool
-- withRule p rs = all (p rs) . map N.narrowedWith . N.narrowings
-- onRule p rs = p rs . N.narrowedRule

-- narrowWith,narrowOn,rewriteWith,rewriteOn :: (Problem -> Rule -> Bool) -> Problem :~> Problem
-- narrowWith = narrow . withRule
-- narrowOn = narrow . onRule
-- rewriteWith = rewrite . withRule
-- rewriteOn = rewrite . onRule

-- ur :: Problem :~> Problem
-- ur = usableRules >=> logMsg "USABLE"

-- cfaur :: Problem :~> Problem
-- cfaur =
--   cfa >=> logMsg "CFA" >=> try ur

ur :: Ord f => P.Problem f Int :=> P.Problem f Int
ur = usableRulesSyntactic >=> logMsg "UR"

cfa :: Problem :=> Problem
cfa = instantiate abstractP >=> logMsg "CFA" where
  abstractP _ _ [_] = True
  abstractP trl v _ =
    maybe False isTArrow (lookup v (theEnv trl))
    && (var v == r || v `elem` headVars r)
    where
      r = rhs (theRule trl)

cfaUR :: Problem :=> Problem
cfaUR = instantiate abstractP >=> logMsg "CFA" where
  abstractP _ _ e = length e <= 1


simplifyATRS :: P.Problem Symbol Int :=> P.Problem Symbol Int
simplifyATRS =
  try (exhaustive (rewrite (withRule lambdaRule) >=> logMsg "lambda"))
  >=> try (exhaustive (inline (withRule caseRule) >=> logMsg "case"))
  >=> try ur

toTRS :: P.Problem Symbol Int :=> P.Problem Symbol Int
toTRS = try cfa >=> try ur >=> uncurried >=> try ur

urDFA :: P.Problem Symbol Int :=> P.Problem Symbol Int
urDFA = usableRulesDFA >=> logMsg "UR-DFA"

simplifyTRS :: P.Problem Symbol Int :=> P.Problem Symbol Int
simplifyTRS =
  try (exhaustive (inline (withRule leafRule)) >=> try ur)
  >=> try (exhaustive ((inline (sizeDecreasing || ruleDeleting) <=> cfaUR) >=> try ur))

simplify :: P.Problem Symbol Int :=> P.Problem Symbol Int
simplify = try simplifyATRS >=> toTRS >=> try simplifyTRS >=> try compress




programFromArgs :: FilePath -> Maybe String -> [String] -> IO (PCF.Program Context)
programFromArgs fname mname args = do
  r <- parseDesugared <$> readFile fname
  either (\e -> putStrLn e >> exitFailure) return r where
    parseDesugared s = do
      p <- programFromString fname s >>= desugar mname
      as <- sequence [ expressionFromString ("argument " ++ show i) ai >>= desugarExpression
                     | (i,ai) <- zip [(1::Int)..] args]
      return p { PCF.expression = foldl (PCF.App mempty) (PCF.expression p) as }

-- expressionFromArgs :: FilePath -> Maybe String -> [String] -> IO (PCF.Exp Context)
-- expressionFromArgs fn mn as = PCF.expression <$> programFromArgs fn mn as

programFromFile :: FilePath -> IO (PCF.Program Context)
programFromFile fname = programFromArgs fname Nothing []

defunctionalizedFromFile :: FilePath -> Maybe String -> [String] -> IO Problem
defunctionalizedFromFile fn m a = do
  prog <- programFromArgs fn m a
  case DM.infer prog of
    Left e -> putDocLn e >> error "Typing failed!"
    Right prog' ->
      case run defunctionalize prog' of
        Nothing -> error "Defunctionalization failed!"
        Just p -> return p

norm p = p { PCF.expression = fromJust $ PCF.nf step (PCF.expression p)} where
     step e = PCF.beta e <|> PCF.fixCBV e <|> PCF.cond e

typeProgram p =
    case DM.infer p of
      Left e -> putDocLn e >> error "program not typable"
      Right p' -> putDocLn (PCF.typeOf (PCF.expression p')) >> return p'


simp = simplifyATRS >=> cfa

