% GenI surface realiser
% Copyright (C) 2009 Eric Kow
%
% This program is free software; you can redistribute it and/or
% modify it under the terms of the GNU General Public License
% as published by the Free Software Foundation; either version 2
% of the License, or (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
%
% You should have received a copy of the GNU General Public License
% along with this program; if not, write to the Free Software
% Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

\chapter{Ranking output}
\label{cha:ranking}

\begin{code}
module NLP.GenI.OptimalityTheory
   ( -- * Input
     OtConstraint(..), OtRanking,
     -- * Output
     GetTraces, OtResult, OtViolation, RankedOtConstraint(..),
     rankResults, otWarnings,
     -- * Display
     prettyViolations,prettyRank
   )
 where

import Control.Applicative ( (<$>), (<*>) )
import Control.Arrow ( first )
import Data.Function (on)
import Data.Char ( isSpace )
import Data.List (nub, partition, sort, sortBy, groupBy, intersperse, (\\), unfoldr )
import Text.JSON

import NLP.GenI.Btypes ( Macros, ptrace )
import qualified NLP.GenI.Builder as B
\end{code}

If your tree schemata are annotated with traces (TODO link to traces and
metagrammars), you can re-use them as a basis for ranking the output produced
by GenI.  The basic idea is to supply a list of either positive, negative or
negative conjunction constraints.

For users familiar with Haskell, the constraints are described with the
following type:
\begin{includecodeinmanual}
\begin{code}
data OtConstraint = PositiveC String -- ^ the trace must appear
                  | NegativeC String -- ^ the trace must NOT appear
                  | NegativeConjC [String] -- ^ these traces must not appear AT THE SAME TIME
 deriving (Show, Eq)
\end{code}
\end{includecodeinmanual}

Roughly speaking the more highly ranked the constraint, the greater the impact
of a violation of that constraint will be.  See section
\ref{sec:ranking-procedure} for more details on the ranking procedure.

\begin{code}
data RankedOtConstraint = RankedOtConstraint Int OtConstraint
 deriving (Show, Eq)

instance Ord RankedOtConstraint where
 compare (RankedOtConstraint r1 _) (RankedOtConstraint r2 _) = compare r1 r2

-- | Same as 'RankedOtConstraint' with the sorting inverted
newtype RankedOtConstraint2 = RankedOtConstraint2 RankedOtConstraint deriving Eq

instance Ord RankedOtConstraint2 where
 compare (RankedOtConstraint2 x) (RankedOtConstraint2 y) = compare y x


type OtRanking = [[OtConstraint]]

data OtViolation = OtViolation { otLexName            :: String -- ^ empty for global
                               , otConstraintViolated :: RankedOtConstraint }
 deriving (Show, Eq, Ord)

data LexItem = LexItem
       { lLexname :: String
       , lTraces :: [String]
       } deriving (Ord, Eq, Show)

type GetTraces = String -> [String]
type OtResult x = (Int,x,[OtViolation])
\end{code}

\section{Input format}

Constraints are expressed in JSON as a list of \jargon{ranking levels}.  A
ranking level is a list of constraints that should be assigned the same rank.
In lieu of a formal description, we provide an example below:

\begin{verbatim}
[
 [{"neg-constraint": "dian0Vn1dePassive"},
  {"pos-constraint": "CanonicalSubject"}],

 [{"neg-conj-constraint": ["InvertedNominalSubject",
                           "CanonicalSententialObjectFinite"]}],

 [{"neg-conj-constraint": ["InvertedNominalSubject",
                           "UnboundedCleft"]},
  {"neg-constraint": "CleftSubject"}]
]
\end{verbatim}

This example constraints file has three ranking levels.  These levels contain
following constraints:

\begin{enumerate}
\item A negative constraint saying that \verb!dian0Vn1dePassive! should
      not appear, and a positive one saying that \verb!CanonicalSubject!
      \emph{should} appear.  These constraints appear together only because
      the author of the example thinks they should have the same rank,
      not because there is neccesarily any inherent relationship between
      them.
\item A single negative conjunction constraint saying that
      \verb!InvertedNominalSubject! and
      \verb!CanonicalSententialObjectFinite!
      should not appear together.
\item A negative conjunction constraint saying tat
      \verb!InvertedNominalSubject! and \verb!UnboundedCleft! should not
      appear together; and also a negative constraints saying that
      \verb!CleftSubject! should not appear.  As with the first ranking
      level, there is no relationship between these two constraints.  We
      just put them on the same level to give them the same rank
\end{enumerate}

\begin{code}
instance JSON OtConstraint where
 readJSON j =
    do jv <- fromJSObject `fmap` readJSON j
       case lookup "pos-constraint" jv of
        Just v    -> PositiveC `fmap` readJSON v
        Nothing   -> case lookup "neg-constraint" jv of
         Just v   -> NegativeC `fmap` readJSON v
         Nothing  -> case lookup "neg-conj-constraint" jv of
          Just v  -> NegativeConjC `fmap` readJSONs v
          Nothing -> fail $ "Could not read OtConstraint"
 showJSON (PositiveC c) =
     JSObject . toJSObject $ [ ("pos-constraint", showJSON c ) ]
 showJSON (NegativeC c) =
     JSObject . toJSObject $ [ ("neg-constraint", showJSON c ) ]
 showJSON (NegativeConjC cs) =
     JSObject . toJSObject $ [ ("neg-conj-constraint", showJSONs cs ) ]
\end{code}

\begin{code}
-- ---------------------------------------------------------------------
-- top level stuff
-- ---------------------------------------------------------------------
otWarnings :: Macros -> OtRanking -> [OtViolation] -> [String]
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

rankResults :: GetTraces -> OtRanking -> [(String,B.Derivation)] -> [OtResult (String,B.Derivation)]
rankResults getTraces r = squish . sortResults . map addViolations
 where
   addViolations (s,d) = ((s,d), getViolations d)
   getViolations  = violations (concatRank r) . lexTraces getTraces
   squish         = concat . zipWith applyRank [1..]
   applyRank i    = map (\(x,vs) -> (i,x,vs))
\end{code}


\begin{code}
-- ---------------------------------------------------------------------
-- detecting violations
-- ---------------------------------------------------------------------

violations :: [RankedOtConstraint] -> [LexItem] -> [OtViolation]
violations cs ls = posVs ls ++ negVs ls
 where
  negVs  = concatMap (\l -> negViolations cs (lLexname l) (lTraces l))
  posVs  = posViolations cs . concatMap lTraces

-- | A positive constraint is violated when a trace is NOT present
posViolations :: [RankedOtConstraint] -> [String] -> [OtViolation]
posViolations cs ss =
 [ OtViolation "" c | c@(RankedOtConstraint _ (PositiveC s)) <- cs, not (s `elem` ss) ]

-- | A negative constraint is violated when a trace is present
--
--   Note that we will not notice if a constraint is violated more
--   than once.  If you want to count multiple violations, you'll
--   either need to partition the input strings and map this function
--   on each sublist or rewrite this code.
negViolations :: [RankedOtConstraint]
              -> String   -- ^ lex name
              -> [String] -- ^ traces
              -> [OtViolation]
negViolations cs l ss =
 [ OtViolation l c | c@(RankedOtConstraint _ (NegativeC s)) <- cs, s `elem` ss ] ++
 [ OtViolation l c | c@(RankedOtConstraint _ (NegativeConjC xs)) <- cs, all (`elem` ss) xs ]
\end{code}

\section{Ranking procedure}
\label{sec:ranking-procedure}

Generation results are sorted according to their highest-ranking constraint
violation (moving on to the next-highest ranking violation and so forth in case
of a tie).  The best result appears first.

\begin{code}
-- | Violations sorted so that the highest ranking constraint
--   (smallest number) goes first
sortedViolations :: (a, [OtViolation]) -> [RankedOtConstraint2]
sortedViolations = map (RankedOtConstraint2 . otConstraintViolated) . sort . snd

-- | Sort the sentences so that the ones with the *lowest*
--   ranking violations (biggest number) go first.
--   Note that we return in groups for the sake of ties.
sortResults :: [(a, [OtViolation])] -> [[(a, [OtViolation])]]
sortResults = sortAndGroupByDecoration compare sortedViolations

lexTraces :: GetTraces -> B.Derivation -> [LexItem]
lexTraces getTraces = map (toLexItem getTraces) . B.lexicalSelection

toLexItem :: GetTraces -> String -> LexItem
toLexItem getTraces t =
 LexItem { lLexname = t
         , lTraces  = getTraces t }
\end{code}

\section{Output format}

Constraint violations can be outputted as JSON objects as the following example
shows

\begin{verbatim}
  { "lex-item": "discuter:n0Vn1pn2:Tn0Vn1pn2-5830:22",
  , "rank": 6,
  , "violation": {"neg-constraint": "passiveVerbMorphology"}
  }
\end{verbatim}

Positive constraint violations are not associated with any lexical items
so the lex-item field is omitted for them.

\begin{code}
-- ---------------------------------------------------------------------
-- output
-- ---------------------------------------------------------------------
instance JSON RankedOtConstraint where
 readJSON j =
    do jo <- fromJSObject `fmap` readJSON j
       let field x = maybe (fail $ "Could not find: " ++ x) readJSON
                   $ lookup x jo
       RankedOtConstraint <$> field "rank"
                          <*> field "violation"
 showJSON = JSObject . toJSObject . rankedOtConstraintToPairs

rankedOtConstraintToPairs :: RankedOtConstraint -> [ (String, JSValue) ]
rankedOtConstraintToPairs (RankedOtConstraint r c) =
  [ ("rank", showJSON r), ("violation", showJSON c) ]

instance JSON OtViolation where
 readJSON j =
    do jo <- fromJSObject `fmap` readJSON j
       case lookup "lex-item" jo of
         Nothing -> OtViolation "" <$> readJSON j
         Just l  -> OtViolation <$> readJSON l
                                <*> readJSON j

 showJSON ov = JSObject . toJSObject $ pairs
  where
   pairs = case otLexName ov of
             "" -> basicPairs
             l  -> ("lex-item", showJSON l) : basicPairs
   basicPairs = rankedOtConstraintToPairs (otConstraintViolated ov)

-- ---------------------------------------------------------------------
-- pretty printing
-- ---------------------------------------------------------------------

-- TODO: Return as a pretty Doc
prettyViolations :: GetTraces -> Bool -> [OtViolation] -> String
prettyViolations getTraces noisy vs =
   unlines $ (if null posVs then []  else [ indented 1 75 . showPosVs $ posVs ])
           ++ map showLexVs negBuckets
 where
  (posVs, negVs) = partition (null . otLexName) vs
  negBuckets = buckets otLexName negVs
  --
  showPosVs  = unwords . map (prettyRankedConstraint . otConstraintViolated)
  showLexVs (l,lvs) =
    let itmName = "(" ++ l ++ ")"
        constraints = map otConstraintViolated lvs
        allTraces = indented 4 75 . unwords . getTraces $ l
    in (indented 2 75 . unwords $ itmName : map prettyRankedConstraint constraints)
       ++ (if noisy then "\n" ++ allTraces else "")

prettyRankedConstraint :: RankedOtConstraint -> String
prettyRankedConstraint (RankedOtConstraint r c) = prettyConstraint c ++ " " ++ prettyRank r

prettyConstraint :: OtConstraint -> String
prettyConstraint (PositiveC str) = '+' : str
prettyConstraint (NegativeC str) = '*' : str
prettyConstraint (NegativeConjC strs) = "*(" ++ (concat $ intersperse " & " strs) ++ ")"

prettyRank :: Int -> String
prettyRank r = "(r" ++ show r ++ ")"

-- ---------------------------------------------------------------------
-- detecting impossible constraints or other potential errors
-- ---------------------------------------------------------------------

neverViolated :: [OtViolation] -> [[OtConstraint]] -> [OtConstraint]
neverViolated vs ranking = concat ranking \\ cs_used
 where
  cs_used = nub . map (noRank . otConstraintViolated) $ vs

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

buckets :: Ord b => (a -> b) -> [a] -> [ (b,[a]) ]
buckets f = map (first head . unzip)
          . groupBy ((==) `on` fst)
          . sortBy (compare `on` fst)
          . map (\x -> (f x, x))

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
\end{code}
