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

\chapter{Builder}
\label{cha:Builder}

The heavy lifting of GenI, the whole chart/agenda mechanism, can be
implemented in many ways.  To make it easier to write different
algorithms for GenI and compare them, we provide a single interface
for what we call Builders.

This interface is then used called by the Geni module and by the
graphical interface.  Note that each builder has its own graphical
interface and that we do a similar thing in the graphical interface
code to make it possible to use these GUIs.  Maybe a little dose of
UML might help.  See figure \ref{fig:builderUml}.

\begin{figure}
\begin{center}
\includegraphics[scale=0.5]{images/builderUml.pdf}
\label{fig:builderUml}
\caption{Essentially what the Builder interface provides}
\end{center}
\end{figure}

\ignore{
\begin{code}
module NLP.GenI.Builder
where

import Control.Applicative ( (<$>), (<*>) )
import Control.Monad.State.Strict
import Data.Bits ( (.&.), (.|.), bit )
import Data.List ( maximum, delete, sort, nub )
import qualified Data.Map as Map
import Data.Maybe ( mapMaybe, fromMaybe  )
import qualified Data.Set as Set
import Data.Tree ( flatten )
import Prelude hiding ( init )
import Text.JSON

import Data.Generics.PlateDirect
import Data.Generics ( Data )
import Data.Typeable ( Typeable )

import NLP.GenI.Automaton (NFA, automatonPaths, automatonPathSets, numStates, numTransitions)
import NLP.GenI.Configuration
  ( getListFlagP, getFlagP, Params,
    DetectPolaritiesFlg(..),
    ExtraPolaritiesFlg(..), MetricsFlg(..),
    RootFeatureFlg(..),
    Optimisation(..), hasOpt,
  )
import NLP.GenI.General (geniBug, BitVector, snd3, thd3)
import NLP.GenI.Btypes
  ( ILexEntry, SemInput, Sem, Pred, showPred,
    gtype, GType(Subs, Foot),
    DescendGeniVal(..), Collectable(collect), alphaConvertById,
    GeniVal
  )
import NLP.GenI.FeatureStructures ( Flist, showFlist, sortFlist )
import NLP.GenI.GeniParsers ( geniFeats, runParser, CharParser )
import NLP.GenI.Polarity  (PolResult(..), buildAutomaton, detectPolPaths)
import NLP.GenI.Statistics (Statistics, incrIntMetric,
                   Metric(IntMetric), updateMetrics,
                   queryMetrics, queryIntMetric,
                   addMetric, emptyStats,
                   )
import NLP.GenI.Tags ( TagElem(idname,tsemantics,ttree), setTidnums, TagDerivation, DerivationStep(..) )
\end{code}
}

\section{The interface}

All backends provide the same essential functionality:
\begin{description}
\item [run]       calls init and stepAll and potentially wraps it with some
                  other functionality.  
\item [init]      initialise the machine from the semantics and lexical selection 
\item [step]      run a realisation step
\item [stepAll]   run all realisations steps until completion
\item [finished]  determine if realisation is finished
\item [stats]     extract various statistics from it
\item [setStats]  set the statistical information 
\item [unpack]    unpack chart results into a list of sentences
\end{description}

FIXME: need to update this comment

\begin{code}
data Builder st it pa = Builder
  { init     :: Input -> pa -> (st, Statistics)
  --
  , step     :: BuilderState st ()
  , stepAll  :: BuilderState st ()
  --
  , finished :: st -> Bool
  , unpack   :: st -> [Output]
  , partial  :: st -> [Output] }

type Output = (Integer, LemmaPlusSentence, Derivation)
type Derivation = TagDerivation
\end{code}

To simplify interaction with the backend, we provide a single data
structure which represents all the inputs a backend could take.

\begin{code}
data Input = 
  Input { inSemInput :: SemInput
        , inLex      :: [ILexEntry] -- ^ for the debugger
        , inCands    :: [(TagElem, BitVector)]   -- ^ tag tree
        }
\end{code}

\section{Uninflected words and sentences}

Each word of an uninflected sentence consists of a lemma and some
feature structures.

\paragraph 
A SentenceAut represents a set of sentences in the form of an automaton.
The labels of the automaton are the words of the sentence.  But note! 
``word'' in the sentence is in fact a tuple (lemma, inflectional feature
structures).  Normally, the states are defined as integers, with the
only requirement being that each one, naturally enough, is unique.

\begin{code}
type SentenceAut            = NFA Int LemmaPlus

data UninflectedDisjunction = UninflectedDisjunction [String] (Flist GeniVal) deriving (Show, Data, Typeable)

instance Biplate UninflectedDisjunction GeniVal where
  biplate (UninflectedDisjunction a v) = plate UninflectedDisjunction |- a ||+ v

instance DescendGeniVal UninflectedDisjunction where
  descendGeniVal s (UninflectedDisjunction a v) = {-# SCC "descendGeniVal" #-} UninflectedDisjunction a (descendGeniVal s v)

instance Collectable UninflectedDisjunction where
  collect (UninflectedDisjunction _ b) = collect b
\end{code}

\section{BuilderState}

To cleanly seperate the tracking of statistics from the core functionality of a
builder, we use a State transformer to thread a Statistics state monad inside of
our main monad.

\begin{code}
type BuilderState s a = StateT s (State Statistics) a
\end{code}

\section{Helper functions for Builders}

\subsection{Initialisation}
\label{fn:Builder:preInit}

There's a few things that need to be run before even initialising the builder.
One of these is running some of the optimisations (namely the polarity stuff),
which is made complicated by the fact that they are optional.  Another of these
to assign each of the trees with a unique ID.  Note that this has to be done
after the polarity optimisation because this optimisation may introduce new
items into the lexical selection.  Finally, we must also make sure we perform
alpha conversion so that unification does not do the wrong thing when two trees
have the same variables.

\begin{code}
preInit :: Input -> Params -> (Input, PolResult)
preInit input config =
 let (cand,_) = unzip $ inCands input
     seminput = inSemInput input
     --
     extraPol = fromMaybe (Map.empty) $ getFlagP ExtraPolaritiesFlg config
     polsToDetect = fromMaybe (error "there should be a default for --detect-pols")
                  $ getFlagP DetectPolaritiesFlg config
     rootFeat = sortFlist $ getListFlagP RootFeatureFlg config
     -- do any optimisations
     isPol = hasOpt Polarised config
     -- polarity optimisation (if enabled)
     autstuff = buildAutomaton polsToDetect rootFeat extraPol seminput cand
     autpaths = map concat . automatonPathSets . prFinal $ autstuff
     combosPol = if isPol then autpaths else [cand]
     -- chart sharing optimisation
     (cands2, pathIds) = unzip $ detectPolPaths combosPol
     --
     fixate ts ps = zip (map alphaConvertById $ setTidnums ts) ps
     input2 = input { inCands    = fixate cands2 pathIds
                    , inSemInput = (prSem autstuff, snd3 seminput, thd3 seminput) }
     -- note: autstuff is only useful for the graphical debugger
  in (input2, autstuff)
\end{code}

\begin{code}
-- | Equivalent to 'id' unless the input contains an empty or uninstatiated
--   semantics
unlessEmptySem :: Input -> Params -> a -> a
unlessEmptySem input _ =
 let (cands,_) = unzip $ inCands input
     nullSemCands   = [ idname t | t <- cands, (null.tsemantics) t ]
     unInstSemCands = [ idname t | t <- cands, not $ Set.null $ collect (tsemantics t) Set.empty ]
     nullSemErr     = "The following trees have a null semantics: " ++ (unwords nullSemCands)
     unInstSemErr   = "The following trees have an uninstantiated semantics: " ++ (unwords unInstSemCands)
     semanticsErr   = (if null nullSemCands then "" else nullSemErr ++ "\n") ++
                      (if null unInstSemCands then "" else unInstSemErr)
  in if null semanticsErr
     then id
     else error semanticsErr
\end{code}

\subsection{Running a surface realiser}

\begin{code}
-- | Performs surface realisation from an input semantics and a lexical selection.
--
--   Statistics tracked
--
--    * pol_used_bundles - number of bundled paths through the polarity automaton.
--                         see 'NLP.GenI.Automaton.automatonPathSets'
--
--    * pol_used_paths - number of paths through the final automaton
--
--    * pol_seed_paths - number of paths through the seed automaton (i.e. with no polarities).
--                       This is normally just 1, unless you have multi-literal semantics
--
--    * pol_total_states - combined number of states in the all the polarity automata
--
--    * pol_total_tras - combined number of transitions in all polarity automata
--
--    * pol_max_states - number of states in the polarity automaton with the most states
--
--    * pol_total_tras - number of transitions in the polarity automata with the most transitions
--
--    * sem_literals    - number of literals in the input semantics
--
--    * lex_trees       - total number of lexically selected trees

--    * lex_foot_nodes  - total number of nodes of any sort in lexically selected trees
--
--    * lex_subst_nodes - total number of sustitution nodes in lexically selected trees
--
--    * lex_foot_nodes  - total number of foot nodes in lexically selected trees
--
--    * plex_...        - same as the lex_ equivalent, but after polarity filtering
run :: Builder st it Params -> Input -> Params -> (st, Statistics)
run builder input config_ =
  let -- 0 normalise the config
      config = modifyFlagP RootFeatureFlg sortFlist config_
      -- 1 run the setup stuff
      (input2, autstuff) = preInit input config
      auts = map snd3 (prIntermediate autstuff)
      -- 2 call the init stuff
      (iSt, iStats) = init builder input2 config
      -- 2b extra statistics
      autpaths = map concat . automatonPathSets . prFinal $ autstuff
      countsFor ts = (length ts, length nodes, length sn, length an)
        where nodes = concatMap (flatten.ttree) ts
              sn = [ n | n <- nodes, gtype n == Subs  ]
              an = [ n | n <- nodes, gtype n == Foot  ]
      (tsem,_,_) = inSemInput input
      cands = nub . map fst $ inCands input
      cands2 = nub . concatMap concat . automatonPathSets . prFinal $ autstuff
      countUp = do incrCounter "sem_literals"  $ length tsem
                   --
                   incrCounter "lex_subst_nodes" snl
                   incrCounter "lex_foot_nodes"  anl
                   incrCounter "lex_nodes"        nl
                   incrCounter "lex_trees"        tl
                   -- node count after polarities are taken into account
                   incrCounter "plex_subst_nodes" snl2
                   incrCounter "plex_foot_nodes"  anl2
                   incrCounter "plex_nodes"        nl2
                   incrCounter "plex_trees"        tl2
                where (tl , nl , snl , anl ) = countsFor cands
                      (tl2, nl2, snl2, anl2) = countsFor cands2
      -- 3 step through the whole thing
      stepAll_ = do countUp
                    incrCounter "pol_used_bundles" $ length autpaths
                    incrCounter "pol_used_paths"   $ length . automatonPaths . prFinal   $ autstuff
                    incrCounter "pol_seed_paths"   $ length . automatonPaths . prInitial $ autstuff
                    incrCounter "pol_total_states" $ sum $ map numStates auts
                    incrCounter "pol_total_trans"  $ sum $ map numTransitions auts
                    incrCounter "pol_max_states"   $ maximum $ map numStates auts
                    incrCounter "pol_max_trans"    $ maximum $ map numTransitions auts
                    stepAll builder
  in runState (execStateT stepAll_ iSt) iStats
\end{code}

\subsection{Semantics and bit vectors}

\begin{code}
type SemBitMap = Map.Map Pred BitVector

-- | assign a bit vector value to each literal in the semantics
-- the resulting map can then be used to construct a bit vector
-- representation of the semantics
defineSemanticBits :: Sem -> SemBitMap
defineSemanticBits sem = Map.fromList $ zip sem bits
  where
   bits = map bit [0..] -- 0001, 0010, 0100...

semToBitVector :: SemBitMap -> Sem -> BitVector
semToBitVector bmap sem = foldr (.|.) 0 $ map doLookup sem
  where doLookup p =
         case Map.lookup p bmap of
         Nothing -> geniBug $ "predicate " ++ showPred p ++ " not found in semanticBit map"
         Just b  -> b

bitVectorToSem :: SemBitMap -> BitVector -> Sem
bitVectorToSem bmap vector =
  mapMaybe tryKey $ Map.toList bmap
  where tryKey (p,k) = if (k .&. vector == k) then Just p else Nothing
\end{code}

\subsection{Generate step}

\begin{code}
-- | Default implementation for the 'stepAll' function in 'Builder'
defaultStepAll :: Builder st it pa -> BuilderState st ()
defaultStepAll b =
 do s <- get
    unless (finished b s) $
      do step b
         defaultStepAll b
\end{code}

\subsection{Dispatching new chart items}
\label{sec:dispatching}

Dispatching consists of assigning a chart item to the right part of the
chart (agenda, trash, results list, etc).  This is implemented as a
series of filters which can either fail or succeed.

Counter-intuitively, success is defined as returning \verb!Nothing!.
Failure is defined as return \verb!Just!, because if a filter fails, it
has the right to modify the item for the next filter.  For example, the
top and bottom unification filter succeeds if it \emph{cannot} unify
the top and bottom features of a node.  It suceeds by putting the item
into the trash and returning Nothing.  If it \emph{can} perform top and
bottom unification, we want to return the item where the top and bottom
nodes are unified.  Failure is success, war is peace, freedom is
slavery, erase is backspace.

\begin{code}
type DispatchFilter s a = a -> s (Maybe a)

-- | Sequence two dispatch filters.
(>-->) :: (Monad s) => DispatchFilter s a -> DispatchFilter s a -> DispatchFilter s a
f >--> f2 = \x -> f x >>= maybe (return Nothing) f2

-- | A filter that always fails (i.e. no filtering)
nullFilter :: (Monad s) => DispatchFilter s a
nullFilter = return.Just

-- | If the item meets some condition, use the first filter, otherwise
--   use the second one.
condFilter :: (Monad s) => (a -> Bool)
           -> DispatchFilter s a -> DispatchFilter s a
           -> DispatchFilter s a
condFilter cond f1 f2 = \x -> if cond x then f1 x else f2 x
\end{code}

\subsection{Statistics}

\begin{code}
modifyStats :: (Metric -> Metric) -> BuilderState st ()
modifyStats fn = lift $ modify $ updateMetrics fn

incrCounter :: String -> Int -> BuilderState st ()
incrCounter key n = modifyStats (incrIntMetric key n)

queryCounter :: String -> Statistics -> Maybe Int
queryCounter key s =
  case queryMetrics (queryIntMetric key) s of
  []  -> Nothing
  [c] -> Just c
  _   -> geniBug $ "More than one instance of the metric: " ++ key
\end{code}

\subsection{Command line configuration}

\begin{code}
initStats :: Params -> Statistics
initStats pa =
 let mdefault ms = if "default" `elem` ms then defaultMetricNames else []
     identifyMs :: [String] -> [Metric]
     identifyMs ms = map namedMetric $ mdefault ms ++ delete "default" ms
     metrics = identifyMs $ fromMaybe [] $ getFlagP MetricsFlg pa
 in execState (mapM addMetric metrics) emptyStats

namedMetric :: String -> Metric
-- the default case is that it's an int metric
namedMetric n = IntMetric n 0

-- Note that the strings here are command-line strings, not metric names!
defaultMetricNames :: [ String ]
defaultMetricNames = [ num_iterations, chart_size, num_comparisons ]
\end{code}

\subsection{Common counters}

These numbers allow us to keep track of how efficient our generator is
and where we are in the process (how many steps we've taken, etc)

\begin{code}
num_iterations, chart_size, num_comparisons :: String

num_iterations  = "iterations"
chart_size      = "chart_size"
num_comparisons = "comparisons"
\end{code}

% ----------------------------------------------------------------------
% strictly API-ish bits
% ----------------------------------------------------------------------

\ignore{
\begin{code}
-- | The names of lexically selected chart items used in a derivation
lexicalSelection :: Derivation -> [String]
lexicalSelection = sort . nub . concatMap (\d -> [dsChild d, dsParent d])

-- | A lemma plus its morphological features
data LemmaPlus = LemmaPlus { lpLemma :: String
                           , lpFeats :: Flist GeniVal }
 deriving (Show, Eq, Ord)

-- | A sentence composed of 'LemmaPlus' instead of plain old words
type LemmaPlusSentence = [LemmaPlus]

instance JSON LemmaPlus where
 readJSON j =
    do jo <- fromJSObject `fmap` readJSON j
       let field x = maybe (fail $ "Could not find: " ++ x) readJSON
                   $ lookup x jo
       LemmaPlus <$> field "lemma"
                 <*> (parsecToJSON "lemma-features" geniFeats =<< field "lemma-features")
 showJSON (LemmaPlus l fs) =
     JSObject . toJSObject $ [ ("lemma", showJSON l)
                             , ("lemma-features", showJSON $ showFlist fs)
                             ]
parsecToJSON :: Monad m => String -> CharParser () b -> String -> m b
parsecToJSON description p str =
 case runParser p () "" str of
   Left  err -> fail $ "Couldn't parse " ++ description ++ " because " ++ show err
   Right res -> return res
\end{code}
}
