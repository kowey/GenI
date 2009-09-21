module NLP.GenI.OptimalityTheory where

import Control.Arrow ( second )
import Data.Function (on)
import Data.Char ( isSpace )
import Data.List (nub, sort, sortBy, groupBy, intersperse, (\\), unfoldr )
import Data.List.Split ( wordsBy )

import NLP.GenI.Btypes ( Macros, ptrace )
import NLP.GenI.Geni
   ( ProgState, getTraces, )

import qualified NLP.GenI.Builder as B

data OtConstraint = PositiveC String -- ^ the trace must appear
                  | NegativeC String -- ^ the trace must NOT appear
                  | NegativeConjC [String] -- ^ these traces must not appear AT THE SAME TIME
 deriving (Show, Eq)

data RankedOtConstraint = RankedOtConstraint Int OtConstraint
 deriving (Show, Eq)

instance Ord RankedOtConstraint where
 compare (RankedOtConstraint r1 _) (RankedOtConstraint r2 _) = compare r1 r2

type OtRanking = [[OtConstraint]]

-- ------------------------------------------------------------------------
-- ----------------------------------------------------------------------

otWarnings gram ranking blocks =
    addWarning neTraces neTracesW
  . addWarning nvConstraints nvConstraintsW
  $ []
 where
  addWarning xs w = if null xs then id else (w xs :)
  neTracesW xs = "these traces never appear in the grammar: " ++ unwords xs
  neTraces  = nonExistentTraces gram ranking
  nvConstraintsW xs = "these constraints are never violated: " ++ unwords (map prettyConstraint xs)
  nvConstraints = neverViolated blocks ranking

sortByViolations pst cs = sortResults . map (second getViolations)
 where
   getViolations = violations cs . lexTraces pst

-- ---------------------------------------------------------------------
-- detecting violations
-- ---------------------------------------------------------------------

-- | positive violations, negative violations which
--   are linked to one specific lexical item
type Violations = ([RankedOtConstraint],[LexViolation])
type LexViolation = (LexItem, [RankedOtConstraint])

violations :: [RankedOtConstraint] -> [LexItem] -> Violations
violations cs ls = (posVs ls, negVs ls)
 where
  negVs = map (\l -> (l, negViolations cs $ lTraces l))
  posVs  = posViolations cs     . concatMap lTraces

-- | A positive constraint is violated when a trace is NOT present
posViolations :: [RankedOtConstraint] -> [String] -> [RankedOtConstraint]
posViolations cs ss =
 [ c | c@(RankedOtConstraint _ (PositiveC s)) <- cs, not (s `elem` ss) ]

-- | A negative constraint is violated when a trace is present
--
--   Note that we will not notice if a constraint is violated more
--   than once.  If you want to count multiple violations, you'll
--   either need to partition the input strings and map this function
--   on each sublist or rewrite this code.
negViolations :: [RankedOtConstraint] -> [String] -> [RankedOtConstraint]
negViolations cs ss =
 [ c | c@(RankedOtConstraint _ (NegativeC s)) <- cs, s `elem` ss ] ++
 [ c | c@(RankedOtConstraint _ (NegativeConjC xs)) <- cs, all (`elem` ss) xs ]

-- ---------------------------------------------------------------------
-- sorting violations
-- ---------------------------------------------------------------------

concatViolations :: Violations -> [RankedOtConstraint]
concatViolations (pVs,lexVs) = pVs ++ concatMap snd lexVs

-- | Same as 'RankedOtConstraint' with the sorting inverted
newtype RankedOtConstraint2 = RankedOtConstraint2 RankedOtConstraint deriving Eq

instance Ord RankedOtConstraint2 where
 compare (RankedOtConstraint2 x) (RankedOtConstraint2 y) = compare y x

-- | Violations sorted so that the highest ranking constraint
--   (smallest number) goes first
sortedViolations :: (String, Violations) -> [RankedOtConstraint2]
sortedViolations = map RankedOtConstraint2 . sort . concatViolations . snd

-- | Sort the sentences so that the ones with the *lowest*
--   ranking violations (biggest number) go first.
--   Note that we return in groups for the sake of ties.
sortResults :: [(String, Violations)] -> [[(String, Violations)]]
sortResults = sortAndGroupByDecoration compare sortedViolations

-- ---------------------------------------------------------------------
--
-- ---------------------------------------------------------------------

data LexItem = LexItem
       { lLexeme :: String
       , lFamily :: (String,Int)
       , lTraces :: [String]
       } deriving (Ord, Eq, Show)


showViolations :: Bool -> Int -> (String, Violations) -> String
showViolations noisy rank (str, (posVs, negVs)) =
   unlines $ (show rank ++ ". " ++ str)
           :  (if null posVs then []  else [ indented 1 75 . showPosVs $ posVs ])
           ++ map showLexVs negVs2
 where
  negVs2 = if noisy then negVs else filter (not . null . snd) negVs
  --
  showPosVs  = unwords . map prettyRankedConstraint
  showLexVs (itm, vs) =
    let (f,i)   = lFamily itm
        itmName = "(" ++ lLexeme itm ++ " " ++ f ++ "-" ++ show i ++ ")"
    in (indented 2 75 . unwords $ itmName : map prettyRankedConstraint vs)
       ++ (if noisy then "\n" ++ (indented 4 75 . unwords . lTraces $ itm)
                    else "")

prettyRankedConstraint :: RankedOtConstraint -> String
prettyRankedConstraint (RankedOtConstraint r c) = prettyConstraint c ++ " " ++ prettyRank r

prettyConstraint :: OtConstraint -> String
prettyConstraint (PositiveC str) = '+' : str
prettyConstraint (NegativeC str) = '*' : str
prettyConstraint (NegativeConjC strs) = "*(" ++ (concat $ intersperse " & " strs) ++ ")"

prettyRank :: Int -> String
prettyRank r = "(r" ++ show r ++ ")"

lexTraces :: ProgState -> B.Derivation -> [LexItem]
lexTraces pst = map (toLexItem pst) . B.lexicalSelection

toLexItem :: ProgState -> String -> LexItem
toLexItem pst t =
 LexItem { lLexeme = l
         , lFamily = (f,i)
         , lTraces = getTraces pst t }
 where (l,f,i) = splitLexFam t

splitLexFam :: String -> (String, String, Int)
splitLexFam n =
 case wordsBy (== ':') n of
 [l2,_,ft,_] -> let (f,t) = spanChar '-' ft in  (l2, f, read t)
 _           -> error $ "error parsing derivation rule... has GenI changed?"

-- ---------------------------------------------------------------------
-- detecting impossible constraints or other potential errors
-- ---------------------------------------------------------------------

neverViolated :: [[[(String,Violations)]]] -> [[OtConstraint]] -> [OtConstraint]
neverViolated vs ranking = concat ranking \\ cs_used
 where
  cs_used = map noRank . nub . concatMap concatViolations . map snd . concat . concat $ vs

nonExistentTraces :: Macros -> [[OtConstraint]] -> [String]
nonExistentTraces ms vs = r_traces \\ m_traces
 where
  m_traces = nub $ concatMap ptrace ms
  r_traces = nub $ concatMap cTraces $ concat vs

cTraces :: OtConstraint -> [String]
cTraces (PositiveC c) = [c]
cTraces (NegativeConjC cs) = cs
cTraces (NegativeC c) = [c]

-- ----------------------------------------------------------------------
-- helpers
-- ----------------------------------------------------------------------

concatRank :: [[OtConstraint]] -> [RankedOtConstraint]
concatRank = concat . zipWith rank [1..]
 where
  rank x ys = map (RankedOtConstraint x) ys

noRank :: RankedOtConstraint -> OtConstraint
noRank (RankedOtConstraint _ c) = c

-- ----------------------------------------------------------------------
-- odds and ends
-- ----------------------------------------------------------------------

-- | Results are grouped so that ties can be noticed
sortAndGroupByDecoration :: Eq b => (b -> b -> Ordering) -> (a -> b) -> [a] -> [[a]]
sortAndGroupByDecoration cmp f = map (map snd)
                               . groupBy ((==) `on` fst)
                               . sortBy (cmp `on` fst)
                               . map (\x -> (f x, x))

indented :: Int -> Int -> String -> String
indented x len = concat . intersperse "\n" . map (\s -> spaces x ++ s) . unfoldr f
 where
  f ""  = Nothing
  f str = Just $ splitAtBefore len str

spaces :: Int -> String
spaces n = replicate n ' '

spanChar :: Char -> String -> (String, String)
spanChar c s = let (a,b') = span (/= c) s in (a, drop 1 b')

splitAtBefore :: Int -> String -> (String, String)
splitAtBefore len xs
  | length xs < len = (xs, "")
  | any isSpace xs  = (begin, trim $ drop (length begin) xs)
  | otherwise       = (xs, "")
 where
  begin
   | length upToSpace > len = upToSpace
   | otherwise = reverse . trim . dropWhile isNotSpace . reverse . take len $ xs
  upToSpace = takeWhile isNotSpace xs
  isNotSpace = not . isSpace
  trim = drop 1
