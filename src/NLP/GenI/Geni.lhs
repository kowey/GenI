% GenI surface realiser
% Copyright (C) 2005 Carlos Areces and Eric Kow
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

\chapter{Geni}
\label{cha:Geni}

Geni is the interface between the front and backends of the generator. The GUI
and the console interface both talk to this module, and in turn, this module
talks to the input file parsers and the surface realisation engine.  This
module also does lexical selection and anchoring because these processes might
involve some messy IO performance tricks.

\begin{code}
module NLP.GenI.Geni (
             -- * main interface
             ProgState(..), ProgStateRef, emptyProgState,
             initGeni,
             runGeni, runGeniWithSelector,
             GeniResult(..), ResultType(..),
             -- * helpers
             lemmaSentenceString, prettyResult,
             showRealisations, groupAndCount,
             getTraces, Selector,
             loadEverything, loadLexicon, loadGeniMacros,
             loadTestSuite, loadTargetSemStr,
             loadRanking, readRanking,
             combine,

             -- used by auxiliary tools only
             chooseLexCand,
             )
where
\end{code}

\ignore{
\begin{code}
import Control.Applicative ((<$>),(<*>))
import Control.Monad.Error
import Control.Monad (unless)

import Data.Binary (Binary, decodeFile)
import Data.IORef (IORef, readIORef, modifyIORef)
import Data.List
import Data.List.Split ( wordsBy )
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Typeable (Typeable)

import qualified System.IO.UTF8 as UTF8

import Text.JSON
-- import System.Process 

import NLP.GenI.General(
    groupAndCount,
    geniBug,
    fst3, snd3,
    ePutStr, ePutStrLn, eFlush,
    )

import NLP.GenI.Btypes
  (Macros, ILexEntry, Lexicon,
   SemInput, Sem, LitConstr, TestCase(..), sortSem,
   isemantics, ifamname, iword,
   showLexeme, showSem,
   pidname, pfamily, ptrace,
   )
import NLP.GenI.BtypesBinary ()

import NLP.GenI.Tags (TagElem,
             idname,
             tsemantics,
             ttrace,
             setTidnums) 
import NLP.GenI.TreeSchemata ( Ttree(..) )

import NLP.GenI.Configuration
  ( Params, getFlagP, hasFlagP, hasOpt, Optimisation(NoConstraints)
  , MacrosFlg(..), LexiconFlg(..), TestSuiteFlg(..), TestCaseFlg(..)
  , MorphInfoFlg(..), MorphCmdFlg(..)
  , RankingConstraintsFlg(..)
  , PartialFlg(..)
  , FromStdinFlg(..), VerboseModeFlg(..)
  , NoLoadTestSuiteFlg(..)
  , RootFeatureFlg(..)
  , TracesFlg(..)
  , grammarType
  , GrammarType(..) )

import qualified NLP.GenI.Builder as B

import NLP.GenI.GeniParsers (geniMacros, geniTagElems,
                    geniLexicon, geniTestSuite,
                    geniTestSuiteString, geniSemanticInput,
                    geniMorphInfo,
                    parseFromFile, runParser, Parser,
                    )
import NLP.GenI.LexicalSelection
        ( mapBySemKeys, chooseLexCand, combineList, compressLexCombineErrors, LexCombineError
        , missingCoanchors, combine,
        )
import NLP.GenI.Morphology
import NLP.GenI.OptimalityTheory
import NLP.GenI.Statistics (Statistics)

-- import CkyBuilder 
-- import SimpleBuilder (simpleBuilder)
\end{code}
}

% --------------------------------------------------------------------
\section{ProgState}
% --------------------------------------------------------------------

\begin{code}
data ProgState = ST{ -- | the current configuration being processed
                    pa     :: Params,
                    --
                    gr       :: Macros,
                    le       :: Lexicon,
                    morphinf :: MorphFn,
                    ts       :: SemInput, 
                    -- | names of test case to run
                    tcase    :: String, 
                    -- | name, original string (for gui), sem
                    tsuite   :: [TestCase],
                    -- | OT constraints (optional)
                    ranking  :: OtRanking,
                    -- | simplified traces (optional)
                    traces   :: [String],
                    -- | any warnings accumulated during realisation
                    --   (most recent first)
                    warnings :: [String]
               }

type ProgStateRef = IORef ProgState

-- | The program state when you start GenI for the very first time
emptyProgState :: Params -> ProgState
emptyProgState args =
 ST { pa = args
    , gr = []
    , le = Map.empty
    , morphinf = const Nothing
    , ts = ([],[],[])
    , tcase = []
    , tsuite = []
    , traces = []
    , ranking = []
    , warnings = []
    }

-- | Log another warning in our internal program state
addWarning :: ProgStateRef -> String -> IO ()
addWarning pstRef s = modifyIORef pstRef $ \p -> p { warnings = s : warnings p }
\end{code}

% --------------------------------------------------------------------
\section{Interface}
\subsection{Loading and parsing}
% --------------------------------------------------------------------

We have one master function that loads all the files GenI is expected to
use.  This just calls the sub-loaders below, some of which are exported
for use by the graphical interface.  The master function also makes sure
to complain intelligently if some of the required files are missing.

\begin{code}
loadEverything :: ProgStateRef -> IO() 
loadEverything pstRef =
  do pst <- readIORef pstRef
     --
     let config   = pa pst
         isMissing f = not $ hasFlagP f config
     -- grammar type
         isNotPreanchored = grammarType config /= PreAnchored
         isNotPrecompiled = grammarType config /= PreCompiled
         useTestSuite =  isMissing FromStdinFlg
                      && isMissing NoLoadTestSuiteFlg
     -- display 
     let errormsg =
           concat $ intersperse ", " [ msg | (con, msg) <- errorlst, con ]
         errorlst =
              [ (isMissing RootFeatureFlg,
                "a root feature [empty feature is fine if you are not using polarity filtering]")
              , (isNotPrecompiled && isMissing MacrosFlg,
                "a tree file")
              , (isNotPreanchored && isMissing LexiconFlg,
                "a lexicon file")
              , (useTestSuite && isMissing TestSuiteFlg,
                "a test suite") ]
     unless (null errormsg) $ fail ("Please specify: " ++ errormsg)
     -- we only have to read in grammars from the simple format
     case grammarType config of 
        PreAnchored -> return ()
        PreCompiled -> return ()
        _        -> loadGeniMacros pstRef
     -- we don't have to read in the lexicon if it's already pre-anchored
     when isNotPreanchored $ loadLexicon pstRef
     -- in any case, we have to...
     loadMorphInfo pstRef
     when useTestSuite $ loadTestSuite pstRef >> return ()
     -- the trace filter file
     loadTraces pstRef
     -- OT ranking
     loadRanking pstRef
\end{code}

The file loading functions all work the same way: we load the file,
and try to parse it.  If this doesn't work, we just fail in IO, and
GenI dies.  If we succeed, we update the program state passed in as
an IORef.

\begin{code}
loadLexicon :: ProgStateRef -> IO ()
loadLexicon pstRef =
    do let getSem l  = isemantics l
           sorter l  = l { isemantics = (sortSem . getSem) l }
           cleanup   = mapBySemKeys isemantics . map sorter
       xs <- loadThingOrDie LexiconFlg "lexicon" pstRef
         (parseFromFileOrFail geniLexicon)
       modifyIORef pstRef (\p -> p { le = cleanup xs })

-- | The macros are stored as a hashing function in the monad.
loadGeniMacros :: ProgStateRef -> IO ()
loadGeniMacros pstRef =
  do xs <- loadThingOrDie MacrosFlg "trees" pstRef parser
     modifyIORef pstRef (\p -> p { gr = xs })
  where parser = parseFromFileMaybeBinary geniMacros

-- | The results are stored as a lookup function in the monad.
loadMorphInfo :: ProgStateRef -> IO ()
loadMorphInfo pstRef =
 do xs <- loadThingOrIgnore MorphInfoFlg "morphological info" pstRef parser
    modifyIORef pstRef (\p -> p { morphinf = readMorph xs } )
 where parser = parseFromFileOrFail geniMorphInfo

loadTraces :: ProgStateRef -> IO ()
loadTraces pstRef =
 do xs <- loadThingOrIgnore TracesFlg "traces" pstRef
             (\f -> lines `fmap` readFile f)
    modifyIORef pstRef (\p -> p {traces = xs})

loadRanking :: ProgStateRef -> IO ()
loadRanking pstRef =
 do config <- pa `fmap` readIORef pstRef
    let verbose = hasFlagP VerboseModeFlg config
    case getFlagP RankingConstraintsFlg config of
      Nothing -> return ()
      Just f  -> do r <- readRanking verbose f
                    modifyIORef pstRef (\p -> p { ranking = r })

readRanking :: Bool -- ^ verbose
            -> FilePath -> IO OtRanking
readRanking verbose f =
 do when verbose $ do
       ePutStr $ unwords [ "Loading OT constraints", f ++ "... " ]
       eFlush
    mr <- (resultToEither . decode) `fmap` UTF8.readFile f -- utf-8?
    when verbose $ ePutStr "done"
    either fail return mr
\end{code}

\subsubsection{Target semantics}

Reading in the target semantics (or test suite) is a little more
complicated.  It follows the same general schema as above, except
that we parse the file twice: once for our internal representation,
and once to get a string representation of each test case.  The
string representation is for the graphical interface; it avoids us
figuring out how to pretty-print things because we can assume the
user will format it the way s/he wants.

\begin{code}
-- | Stores the results in the tcase and tsuite fields
loadTestSuite :: ProgStateRef -> IO [TestCase]
loadTestSuite pstRef = do
  config <- pa `fmap` readIORef pstRef
  let parser f = do
         sem   <- parseFromFileOrFail geniTestSuite f
         mStrs <- parseFromFileOrFail geniTestSuiteString f
         return $ zip sem mStrs
      updater s x =
        x { tsuite = s
          , tcase  = fromMaybe "" $ getFlagP TestCaseFlg config}
      cleanup (tc,str) =
        tc { tcSem = (sortSem sm, sort sr, lc)
           , tcSemString = str }
        where (sm, sr, lc) = tcSem tc
  xs <- map cleanup `fmap` loadThingOrDie TestSuiteFlg "test suite" pstRef parser
  modifyIORef pstRef (updater xs)
  return xs
\end{code}

Sometimes, the target semantics does not come from a file, but from
the graphical interface, so we also provide the ability to parse an
arbitrary string as the semantics.

\begin{code}
-- | Updates program state the same way as 'loadTestSuite'
loadTargetSemStr :: ProgStateRef -> String -> IO ()
loadTargetSemStr pstRef str = 
    do parseSem
    where
       parseSem = do
         let sem = runParser geniSemanticInput () "" str
         case sem of
           Left  err -> fail (show err)
           Right sr  -> modifyIORef pstRef (\x -> x{ts = smooth sr})
       smooth (s,r,l) = (sortSem s, sort r, l)
\end{code}

\subsubsection{Helpers for loading files}

\begin{code}
loadThingOrIgnore, loadThingOrDie :: forall f a . (Eq f, Show f, Typeable f)
           => (FilePath -> f) -- ^ flag
           -> String
           -> ProgStateRef
           -> (FilePath -> IO [a])
           -> IO [a]

-- | Load the file if the relevant option is set, otherwise ignore
loadThingOrIgnore flag description pstRef parser =
 do config <- pa `fmap` readIORef pstRef
    case getFlagP flag config of
      Nothing -> return []
      Just f  -> loadThing f description pstRef parser

-- | Load the file if the relevant option is set, otherwise complain and die
loadThingOrDie flag description pstRef parser =
 do config <- pa `fmap` readIORef pstRef
    case getFlagP flag config of
      Nothing -> fail $ "Please specify a " ++ description ++ "!"
      Just f  -> loadThing f description pstRef parser

loadThing :: FilePath             -- ^ file to load
          -> String               -- ^ description
          -> ProgStateRef
          -> (FilePath -> IO [a]) -- ^ parsing cmd
          -> IO [a]
loadThing filename description pstRef parser =
 do config <- pa `fmap` readIORef pstRef
    let verbose = hasFlagP VerboseModeFlg config
    when verbose $ do
       ePutStr $ unwords [ "Loading",  description, filename ++ "... " ]
       eFlush
    theTs <- parser filename
    when verbose $ ePutStr $ (show $ length theTs) ++ " entries\n"
    return theTs

parseFromFileOrFail :: Parser a -> FilePath -> IO a
parseFromFileOrFail p f = parseFromFile p f >>= either (fail.show) (return)

parseFromFileMaybeBinary :: Binary a
                         => Parser a
                         -> FilePath
                         -> IO a
parseFromFileMaybeBinary p f =
 if (".genib" `isSuffixOf` f)
    then decodeFile f
    else parseFromFileOrFail p f
\end{code}

% --------------------------------------------------------------------
\subsection{Surface realisation - entry point}
% --------------------------------------------------------------------

This is your basic entry point.  You call this if the only thing you want to do
is run the surface realiser.

\begin{enumerate}
\item It initialises the realiser (lexical selection, among other things),
      via \fnref{initGeni}
\item It runs the builder (the surface realisation engine proper)
\item It unpacks the builder results 
\item It finalises the results (morphological generation)
\end{enumerate}

\begin{code}
data GeniResult = GeniResult
 { grLemmaSentence     :: B.LemmaPlusSentence
 , grRealisations :: [String]
 , grDerivation   :: B.Derivation
 , grLexSelection :: [ GeniLexSel ]
 , grRanking      :: Int
 , grViolations   :: [ OtViolation ]
 , grResultType   :: ResultType
 , grOrigin       :: Integer -- normally a chart item id
 } deriving (Ord, Eq)

data GeniLexSel = GeniLexSel
 { nlTree  :: String
 , nlTrace :: [String]
 } deriving (Ord, Eq)

data ResultType = CompleteResult | PartialResult deriving (Ord, Eq)

-- | Returns a list of sentences, a set of Statistics, and the generator state.
--   The generator state is mostly useful for debugging via the graphical interface.
--   Note that we assumes that you have already loaded in your grammar and
--   parsed your input semantics.
runGeni :: ProgStateRef -> B.Builder st it Params -> IO ([GeniResult], Statistics, st)
runGeni pstRef builder = runGeniWithSelector pstRef defaultSelector builder

runGeniWithSelector :: ProgStateRef -> Selector -> B.Builder st it Params -> IO ([GeniResult], Statistics, st)
runGeniWithSelector pstRef  selector builder =
  do pst <- readIORef pstRef
     let config = pa pst
         run    = B.run builder
         unpack = B.unpack builder
     -- step 1: lexical selection
     initStuff <- initGeniWithSelector pstRef selector
     -- step 2: chart generation
     let (finalSt, stats) = run initStuff config
     -- step 3: unpacking
     let uninflected = unpack finalSt
         tryPartial  = null uninflected && hasFlagP PartialFlg config
         rawResults  = if tryPartial then B.partial builder finalSt else uninflected
         resultTy    = if tryPartial then PartialResult else CompleteResult
     -- step 4: post-processing
     results <- finaliseResults pstRef resultTy rawResults
     return (results, stats, finalSt)
\end{code}

% --------------------------------------------------------------------
\subsection{Surface realisation - sub steps}
% --------------------------------------------------------------------

Below are the initial and final steps of \fnreflite{runGeni}.  These functions
are seperated out so that they may be individually called from the graphical
debugger.  The middle steps (running and unpacking the builder) depend on your
builder implementation.

\begin{code}
-- | 'initGeni' performs lexical selection and strips the input semantics of
--   any morpohological literals
initGeni :: ProgStateRef -> IO (B.Input)
initGeni pstRef = initGeniWithSelector pstRef defaultSelector

initGeniWithSelector :: ProgStateRef -> Selector -> IO (B.Input)
initGeniWithSelector pstRef lexSelector =
 do -- disable constraints if the NoConstraintsFlg anti-optimisation is active
    modifyIORef pstRef
      (\p -> if hasOpt NoConstraints (pa p)
             then p { ts = (fst3 (ts p),[],[]) }
             else p)
    -- lexical selection
    (cand, lexonly) <- lexSelector pstRef
    pst <- readIORef pstRef
    -- strip morphological predicates
    let (tsem,tres,lc) = ts pst
        tsem2 = stripMorphSem (morphinf pst) tsem
            --
    let initStuff = B.Input 
          { B.inSemInput = (tsem2, tres, lc)
          , B.inLex   = lexonly 
          , B.inCands = map (\c -> (c,-1)) cand
          }
    return initStuff 

-- | 'finaliseResults' does any post-processing steps that we want to integrate
--   into mainline GenI.  So far, this consists of morphological realisation and
--   OT ranking
finaliseResults :: ProgStateRef -> ResultType -> [B.Output] -> IO [GeniResult]
finaliseResults pstRef ty os =
 do pst <- readIORef pstRef
    -- morph TODO: make this a bit safer
    mss <- case getFlagP MorphCmdFlg (pa pst) of
             Nothing  -> return $ map sansMorph sentences
             Just cmd -> map snd `fmap` inflectSentencesUsingCmd cmd sentences
    -- OT ranking
    let unranked = zipWith (sansRanking pst) os mss
        rank = rankResults (getTraces pst) grDerivation (ranking pst)
    return . map addRanking . rank $ unranked
 where
  sentences = map snd3 os
  sansRanking pst (i,l,d) rs =
    GeniResult { grLemmaSentence = l
               , grRealisations = rs
               , grDerivation   = d
               , grLexSelection = map (\x -> GeniLexSel x (getTraces pst x)) (B.lexicalSelection d)
               , grRanking = -1
               , grViolations = []
               , grResultType = ty
               , grOrigin     = i
               }
  addRanking (i,res,vs) = res { grViolations = vs, grRanking = i }
\end{code}

% --------------------------------------------------------------------
\subsection{Displaying results}
% --------------------------------------------------------------------

\begin{code}
-- | Show the sentences produced by the generator, in a relatively compact form
showRealisations :: [String] -> String
showRealisations sentences =
  let sentencesGrouped = map (\ (s,c) -> s ++ countStr c) g
                         where g = groupAndCount sentences 
      countStr c = if c > 1 then " (" ++ show c ++ " instances)"
                            else ""
  in if null sentences
     then "(none)"
     else unlines sentencesGrouped

-- | No morphology! Pretend the lemma string is a sentence
lemmaSentenceString :: GeniResult -> String
lemmaSentenceString = unwords . map lpLemma . grLemmaSentence

prettyResult :: ProgState -> GeniResult -> String
prettyResult pst nr =
  concat . intersperse "\n" . map showOne . grRealisations $ nr
 where
  showOne str = show theRanking  ++ ". " ++ str ++ "\n" ++ violations
  violations  = prettyViolations tracesFn verbose (grViolations nr)
  theRanking  = grRanking nr
  verbose  = hasFlagP VerboseModeFlg (pa pst)
  tracesFn = getTraces pst

-- | 'getTraces' is most likely useful for grammars produced by a
--   metagrammar system.  Given a tree name, we retrieve the ``trace''
--   information from the grammar for all trees that have this name.  We
--   assume the tree name was constructed by GenI; see the source code for
--   details.
getTraces :: ProgState -> String -> [String]
getTraces pst tname =
  filt $ concat [ ptrace t | t <- gr pst, pidname t == readPidname tname ]
  where
   filt = case traces pst of
          []    -> id
          theTs -> filter (`elem` theTs)

-- | We assume the name was constructed by 'combineName'
readPidname :: String -> String
readPidname n =
  case wordsBy (== ':') n of
  (_:_:p:_) -> p
  _         -> geniBug "readPidname or combineName are broken"
\end{code}

% --------------------------------------------------------------------
\section{Lexical selection}
% --------------------------------------------------------------------

\paragraph{runLexSelection} \label{fn:runLexSelection} determines which
candidates trees which will be used to generate the current target semantics.  
In addition to the anchored candidate trees, we also return the lexical items 
themselves.  This list of lexical items is useful for debugging a grammar; 
it lets us know if GenI managed to lexically select something, but did not 
succeed in anchoring it.

\begin{code}
runLexSelection :: ProgStateRef -> IO ([TagElem], [ILexEntry])
runLexSelection pstRef =
 do pst <- readIORef pstRef
    let (tsem,_,litConstrs) = ts pst
        lexicon  = le pst
        config   = pa pst
        verbose  = hasFlagP VerboseModeFlg config
        grammar = gr pst
    -- perform lexical selection
    (cand, lexCand, errs) <- case grammarType config of
                               PreAnchored -> do cs <- readPreAnchored pst
                                                 return (cs, [], [])
                               _           -> return $ initialLexSelection tsem lexicon grammar
    let candFinal = finaliseLexSelection (morphinf pst) tsem litConstrs cand
    -- status
    when verbose $
      do ePutStrLn $ "Lexical items selected:\n" ++ (unlinesIndentAnd (showLexeme.iword) lexCand)
         ePutStrLn $ "Trees anchored (family) :\n" ++ (unlinesIndentAnd idname candFinal)
    -- anchoring errors
    mapM_ (addWarning pstRef . showErr) $ concatMap compressLexCombineErrors errs
    -- more lexical selection errors
    forM_ lexCand $ \l ->
         do let familyMembers = [ p | p <- grammar, pfamily p == ifamname l ]
            -- snippets of error message
            let lexeme = showLexeme.iword $ l
                _outOfFamily n = show n ++ "/" ++ (show $ length familyMembers)
                                 ++ " instances of " ++ lexeme ++ ":" ++ ifamname l
            -- print out missing coanchors list
            case concatMap (missingCoanchors l) familyMembers of
              [] -> return ()
              cs -> mapM_ showWarning . group . sort $ cs
                    where showWarning [] = geniBug "silly error in Geni.runLexSelection"
                          showWarning xs@(x0:_) = addWarning pstRef $ "Missing co-anchor '" ++ x0 ++ "'" ++ " in " ++ _outOfFamily (length xs) ++ "."
            -- print out enrichment errors
{-
            unless (null enrichEs) $ do
                let numDiscards = length enrichEs
                    badEnrichments = [ av | av <- iequations l, hasMatch av ]
                    hasMatch (a,_) = any (== parsePathEq a) errLocs
                    errLocs = map eeLocation enrichEs
                ePutStrLn $      "Warning: Discarded "
                            ++ _outOfFamily numDiscards
                            ++ "\n         due to enrichment failure with "
                            ++ "[" ++ showPairs badEnrichments ++ "]."
            mapM (ePutStrLn.show) otherEs
-}
    -- lexical selection failures
    let missedSem  = tsem \\ (nub $ concatMap tsemantics candFinal)
        hasTree l = isJust $ find (\t -> tsemantics t == lsem) cand
          where lsem = isemantics l
        missedLex = filter (not.hasTree) lexCand
    unless (null missedSem) $ addWarning pstRef $ "no lexical selection for " ++ showSem missedSem
    unless (null missedLex) $ forM_ missedLex $ \l -> addWarning pstRef $
        "'" ++ showLex l ++ "' was lexically selected, but not anchored to any trees"
    return (candFinal, lexCand)
 where
   showLex l = (showLexeme $ iword l) ++ "-" ++ (ifamname l)
   showErr (c, e) = show e ++ " (" ++ show c ++ " times)"
   --
   indent  x = ' ' : x
   unlinesIndentAnd :: (x -> String) -> [x] -> String
   unlinesIndentAnd f = unlines . map (indent . f)

initialLexSelection :: Sem -> Lexicon -> Macros -> ([TagElem], [ILexEntry], [[LexCombineError]])
initialLexSelection tsem lexicon grammar =
  (concat cands, lexCands, errs)
 where
  (errs, cands) = unzip $ map (combineList grammar) lexCands
  lexCands      = chooseLexCand lexicon tsem

finaliseLexSelection :: MorphFn -> Sem -> [LitConstr] -> [TagElem] -> [TagElem]
finaliseLexSelection morph tsem litConstrs =
  setTidnums . considerCoherency . considerHasSem . considerLc . considerMorph
 where
   -- attach any morphological information to the candidates
   considerMorph = attachMorph morph tsem
   -- filter out candidates which do not fulfill the trace constraints
   matchesLc t = all (`elem` myTrace) constrs
         where constrs = concat [ cs | (l,cs) <- litConstrs, l `elem` mySem ]
               mySem   = tsemantics t
               myTrace = ttrace t
   considerLc = filter matchesLc
   -- filter out candidates whose semantics has bonus stuff which does
   -- not occur in the input semantics
   considerCoherency = filter (all (`elem` tsem) . tsemantics)
   considerHasSem    = filter (not . null . tsemantics)
\end{code}

% --------------------------------------------------------------------
\subsection{Pre-selection and pre-anchoring}
\label{sec:pre-anchor}
% --------------------------------------------------------------------

For testing purposes, we can perform lexical selection ahead of time and store
it somewhere else.

\begin{code}
-- | Only used for instances of GenI where the grammar is compiled
--   directly into GenI.
type Selector = ProgStateRef -> IO ([TagElem],[ILexEntry])

defaultSelector :: Selector
defaultSelector = runLexSelection
\end{code}

For debugging purposes, it is often useful to perform lexical selection and
surface realisation separately.  Pre-anchored mode allows the user to just
pass the lexical selection in as a file of anchored trees associated with a
semantics.

\begin{code}
readPreAnchored :: ProgState -> IO [TagElem]
readPreAnchored pst =
 case getFlagP MacrosFlg (pa pst) of
 Nothing   -> fail "No macros file specified (preanchored mode)"
 Just file -> parseFromFileOrFail geniTagElems file
\end{code}

% --------------------------------------------------------------------
% Boring utility code
% --------------------------------------------------------------------

\ignore{
\begin{code}
instance JSON GeniResult where
 readJSON j =
    do jo <- fromJSObject `fmap` readJSON j
       let field x = maybe (fail $ "Could not find: " ++ x) readJSON
                   $ lookup x jo
       GeniResult <$> field "raw"
                  <*> field "realisations"
                  <*> field "derivation"
                  <*> field "lexical-selection"
                  <*> field "ranking"
                  <*> field "violations"
                  <*> field "result-type"
                  <*> field "chart-item"
 showJSON nr =
     JSObject . toJSObject $ [ ("raw", showJSON $ grLemmaSentence nr)
                             , ("realisations", showJSONs $ grRealisations nr)
                             , ("derivation", showJSONs $ grDerivation nr)
                             , ("lexical-selection", showJSONs $ grLexSelection nr)
                             , ("ranking", showJSON $ grRanking nr)
                             , ("violations", showJSONs $ grViolations nr)
                             , ("result-type", showJSON $ grResultType nr)
                             , ("chart-item", showJSON $ grOrigin nr)
                             ]

instance JSON ResultType where
  readJSON j =
    do js <- fromJSString `fmap` readJSON j
       case js of
         "partial"   -> return PartialResult
         "complete"  -> return CompleteResult
         ty          -> fail $ "unknown result type: " ++ ty
  showJSON CompleteResult = JSString $ toJSString "complete"
  showJSON PartialResult  = JSString $ toJSString "partial"

instance JSON GeniLexSel where
 readJSON j =
    do jo <- fromJSObject `fmap` readJSON j
       let field x = maybe (fail $ "Could not find: " ++ x) readJSON
                   $ lookup x jo
       GeniLexSel <$> field "lex-item"
                  <*> field "trace"
 showJSON x =
     JSObject . toJSObject $ [ ("lex-item", showJSON  $ nlTree x)
                             , ("trace",    showJSONs $ nlTrace x)
                             ]
\end{code}
}

