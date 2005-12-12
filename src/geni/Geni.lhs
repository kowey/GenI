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
module Geni (ProgState(..), ProgStateRef, 
             showRealisations, groupAndCount,
             initGeni, runGeni, 
             loadGrammar, loadLexicon, 
             loadTestSuite, loadTargetSemStr,
             combine, testGeni)
where
\end{code}

\ignore{
\begin{code}
import qualified Data.Map as Map
import Data.IORef (IORef, readIORef, modifyIORef)
import Data.List (intersperse, sort, nub)
import Data.Tree

import System.IO(hPutStrLn, hClose, hGetContents)
import System.IO.Unsafe(unsafePerformIO)
import Text.ParserCombinators.Parsec 
-- import System.Process 

import Control.Monad (when)
import General(groupAndCount, multiGroupByFM, ePutStr, ePutStrLn, eFlush)

import Btypes (Macros, MTtree, ILexEntry, Lexicon, 
               Replacable(..),
               Sem, SemInput, 
               fromGVar, 
               GNode, GType(Subs), Flist,
               isemantics, ifamname, iword, iparams, 
               ipfeat, ifilters,
               isempols, 
               gnname, gtype, gaconstr, gup, gdown, toKeys,
               sortSem, subsumeSem, params, 
               showLexeme,
               pidname, pfamily, pfeat, ptype, 
               setLexeme, tree, unifyFeat)

import Tags (Tags, TagElem, emptyTE, TagSite, 
             idname, 
             derivation, ttype, tsemantics, ttree, tsempols,
             tinterface, tpolarities, substnodes, adjnodes, 
             setTidnums) 

import Configuration
  ( Params
  , grammarType, testCase, morphCmd, ignoreSemantics, selectCmd,
  , GrammarType(..),
  , tsFile, macrosFile, lexiconFile, morphFile)

import qualified Builder as B

import GeniParsers (geniMacros, 
                    geniLexicon, geniTestSuite, geniSemanticInput, 
                    geniMorphInfo)
import Morphology

-- import CkyBuilder 
-- import SimpleBuilder (simpleBuilder)

-- Not for windows
-- FIXME: even better would be to really figure out all this
-- Posix stuff so that I don't need to use my SysGeni hack
#ifndef mingw32_BUILD_OS 
import SysGeni 
#endif
\end{code}
}

\begin{code}
myEMPTY :: String
myEMPTY = "MYEMPTY" 

testGeni = True
\end{code}

% --------------------------------------------------------------------
\section{ProgState}
% --------------------------------------------------------------------

Data types for keeping track of the program state.  

\begin{description}
\item[pa] the current configuration being processed
\item[batchPa] the list of possible configurations.  This list is
               never empty.  If we are doing batch processing, 
               then its only item is pa
\item 
\end{description}

\begin{code}
data ProgState = ST{pa     :: Params,
                    --
                    gr       :: Macros,
                    le       :: Lexicon,
                    morphinf :: MorphFn,
                    ts       :: SemInput, 
                    -- names of test case to run
                    tcase    :: String, 
                    --name, sem
                    tsuite   :: [(String,SemInput)]
               }

type ProgStateRef = IORef ProgState
\end{code}

% --------------------------------------------------------------------
\section{Interface}
\subsection{Loading and parsing}
% --------------------------------------------------------------------

\fnlabel{loadGrammar} Given the pointer to the monadic state pstRef it reads
and parses everything it needs to get going, that is, the macros file, 
lexicon file and test suite.

\begin{code}
loadGrammar :: ProgStateRef -> IO() 
loadGrammar pstRef =
  do pst <- readIORef pstRef
     --
     let config    = pa pst
         gfilename = macrosFile config 
         lfilename = lexiconFile config 
         sfilename = tsFile config
     -- display 
     let errorlst  = filter (not.null) $ map errfn src 
           where errfn (err,msg) = if err then msg else ""
                 src = [ (null gfilename, "a tree file")
                       , (null lfilename, "a lexicon file") 
                       , (null sfilename, "a test suite") ]
         errormsg = "Please specify: " ++ (concat $ intersperse ", " errorlst)
     when (not $ null errorlst) $ fail errormsg 
     -- we don't have to read in grammars from the simple format
     case grammarType config of 
        XMGTools -> return ()
        _           -> loadGeniMacros pstRef config
     -- in any case, we have to...
     loadLexicon   pstRef config 
     loadMorphInfo pstRef config 
     loadTestSuite pstRef 
\end{code}

\fnlabel{loadLexicon} Given the pointer to the monadic state pstRef and
the parameters from a grammar index file parameters; it reads and parses
the lexicon file.   

\begin{code}
loadLexicon :: ProgStateRef -> Params -> IO ()
loadLexicon pstRef config = do
       let lfilename = lexiconFile config
       when (null lfilename) $ fail "Please specify a lexicon!"
       ePutStr $ "Loading Lexicon " ++ lfilename ++ "..."
       eFlush
       pst <- readIORef pstRef
       let params = pa pst
           --       
           getSem l  = if (ignoreSemantics params) then [] else isemantics l 
           sorter l  = l { isemantics = (sortSem . getSem) l }
           cleanup   = (mapBySemKeys isemantics) . (map sorter)
           --
       prelex <- parseFromFile geniLexicon lfilename
       let lex = case prelex of 
                   Left err -> error (show err)
                   Right x  -> cleanup x 
       --
       ePutStr ((show $ length $ Map.keys lex) ++ " entries\n")
       -- combine the two lexicons
       modifyIORef pstRef (\x -> x{le = lex})

       return ()
\end{code}

\fnlabel{loadGeniMacros} Given the pointer to the monadic state pstRef and
the parameters from a grammar index file parameters; it reads and parses
macros file.  The macros are stored as a hashing function in the monad.

\begin{code}
loadGeniMacros :: ProgStateRef -> Params -> IO ()
loadGeniMacros pstRef config = 
  do let filename = macrosFile config
     --
     ePutStr $ "Loading Macros " ++ filename ++ "..."
     eFlush
     when (null filename) $ fail "Please specify a trees file!"
     parsed <- parseFromFile geniMacros filename
     case parsed of 
       Left  err -> fail (show err)
       Right g   -> setGram g
  where
    setGram g = 
      do let sizeg = length g
         ePutStr $ show sizeg ++ " trees in " 
         ePutStr $ (show $ length g) ++ " families\n"
         modifyIORef pstRef (\x -> x{gr = g})
\end{code}

\fnlabel{loadMorphInfo} Given the pointer to the monadic state pstRef and
the parameters from a grammar index file parameters; it reads and parses
the morphological information file, if available.  The results are stored
as a lookup function in the monad.

\begin{code}
loadMorphInfo :: ProgStateRef -> Params -> IO ()
loadMorphInfo pstRef config = 
  do let filename = morphFile config
     when (not $ null filename ) $ do --
        ePutStr $ "Loading Morphological Info " ++ filename ++ "..."
        eFlush
        parsed <- parseFromFile geniMorphInfo filename
        let g = case parsed of 
                  Left err -> fail (show err)
                  Right  x -> x
            sizeg  = length g
        ePutStr $ show sizeg ++ " entries\n" 
        modifyIORef pstRef (\x -> x{morphinf = readMorph g})
\end{code}

\subsubsection{Target semantics}

\fnlabel{loadTestSuite} 
given a pointer pstRef to the general state st, it access the parameters and the
name of the file for the target semantics from params.  It parses the file as a
test suite, and assigns it to the tsuite field of st.

\begin{code}
loadTestSuite :: ProgStateRef -> IO ()
loadTestSuite pstRef = do
  pst <- readIORef pstRef
  let config   = pa pst
      filename = (tsFile.pa) pst
      useSem   = not $ ignoreSemantics config
  when (null filename) $ fail "Please specify a test suite!"
  when useSem $ do
    ePutStr $ "Loading Test Suite " ++ filename ++ "...\n"
    eFlush
    -- helper functions for test suite stuff
    let cleanup (id, (sm,sr), _) = (id, newsmsr)
          where newsmsr = (sortSem sm, sort sr)
        updateTsuite s x = x { tsuite = map cleanup s   
                             , tcase  = testCase config}
    sem <- parseFromFile geniTestSuite filename 
    case sem of 
      Left err -> fail (show err)
      Right s  -> modifyIORef pstRef $ updateTsuite s 
    -- in the end we just say we're done
    --ePutStr "done\n"
\end{code}

\fnlabel{loadTargetSemStr} Given a string with some semantics, it
parses the string and assigns the assigns the target semantics to the 
ts field of the ProgState 

\begin{code}
loadTargetSemStr :: ProgStateRef -> String -> IO ()
loadTargetSemStr pstRef str = 
    do pst <- readIORef pstRef
       let params = pa pst
       if (ignoreSemantics params) then return () else parseSem 
    where
       parseSem = do
         let sem = runParser geniSemanticInput () "" str
         case sem of
           Left  err -> fail (show err)
           Right sr  -> modifyIORef pstRef (\x -> x{ts = smooth sr})
       smooth (s,r) = (sortSem s, sort r)
\end{code}

% --------------------------------------------------------------------
\subsection{Surface realisation - entry point}
% --------------------------------------------------------------------

\fnlabel{runGeni} is what you call if the only thing you want to do is run the
surface realiser.  It assumes that you have already loaded in your grammar and
parsed your input semantics, and performs the following four steps:

\begin{enumerate}
\item It initialises the realiser (lexical selection, among other things),
      via \fnref{initGeni}
\item It runs the builder (the surface realisation engine proper)
\item It unpacks the builder results 
\item It finalises the results (morphological generation)
\end{enumerate}

It returns a list of sentences as a well as the final builder state.  This 
final state is useful for displaying debug information.

\begin{code}
runGeni :: ProgStateRef -> B.Builder st it Params -> IO ([String], st)
runGeni pstRef builder = 
  do let run    = B.run builder
         unpack = B.unpack builder
     -- step 1
     initStuff <- initGeni pstRef
     --
     pst <- readIORef pstRef
     let config  = pa pst
         -- step 2 
         finalSt = run initStuff config
         -- step 3
         uninflected = unpack finalSt
     -- step 4
     sentences <- finaliseResults pstRef uninflected 
     return (sentences, finalSt)
\end{code}

% --------------------------------------------------------------------
\subsection{Surface realisation - sub steps}
% --------------------------------------------------------------------

Below are the initial and final steps of \fnreflite{runGeni}.  These functions
are seperated out so that they may be individually called from the graphical
debugger.  The middle steps (running and unpacking the builder) depend on your
builder implementation.

\fnlabel{initGeni} performs lexical selection and strips the input semantics of
any morpohological literals

\begin{code}
initGeni :: ProgStateRef -> IO (B.Input)
initGeni pstRef = 
 do -- lexical selection
    pstLex   <- readIORef pstRef
    (cand, lexonly) <- runLexSelection pstLex
    -- strip morphological predicates
    let (tsem,tres) = ts pstLex
        tsem2       = stripMorphSem (morphinf pstLex) tsem
    --
    let initStuff = B.Input 
          { B.inSemInput = (tsem2, tres)
          , B.inLex   = lexonly 
          , B.inCands = cand
          }
    return initStuff 
\end{code}

\fnlabel{finaliseResults} for the moment consists only of running the
morphological generator, but there could conceivably be more involved.

\begin{code}
finaliseResults :: ProgStateRef -> [B.UninflectedSentence] -> IO [String]
finaliseResults = runMorph 
\end{code}

% --------------------------------------------------------------------
\subsection{Displaying results}
% --------------------------------------------------------------------

\paragraph{showRealisations} shows the sentences produced by the
generator in a relatively compact form 

\begin{code}
showRealisations :: [String] -> String
showRealisations sentences =
  let sentencesGrouped = map (\ (s,c) -> s ++ count c) g
                         where g = groupAndCount sentences 
      count c = if (c > 1) 
                then " (" ++ show c ++ " instances)"
                else ""
  in if null sentences
     then "(none)"
     else concat $ intersperse "\n" $ sentencesGrouped
\end{code}

%\paragraph{describeOpts} concisely describes the optimisations used
%by GenI in a short string.  Each optimisation is described as a one
%to three letter code:
%
%\begin{code}
%describeOpts :: Params -> String
%describeOpts config =
%  let polkey = "pol"
%      polplus  = ""
%       ++ "A" -- always detects polarities
%       ++ (if polsig  config then "S" else "")
%       ++ (if chartsharing config then "C" else "")
%      --
%      adjkey = "adj"
%      adjplus  = ""   
%       ++ (if semfiltered config then "S" else "")
%       ++ "O" -- always uses ordered adjunction
%       ++ (if footconstr  config then "F" else "")
%      --
%      optPol = if polarised config then polkey ++ polStuff else ""
%        where polStuff = if null polplus then "" else ":" ++ polplus
%      optAdj = if null adjplus then "" else " " ++ adjkey ++ ":" ++ adjplus
%      optAll = optPol ++ optAdj
% in if null optAll then "none" else optAll
%\end{code}

% --------------------------------------------------------------------
\section{Lexical selection}
\label{sec:candidate_selection} \label{sec:lexical_selecetion} \label{par:lexSelection}
% --------------------------------------------------------------------

\paragraph{runLexSelection} \label{fn:runLexSelection} determines which
candidates trees which will be used to generate the current target semantics.  
In addition to the anchored candidate trees, we also return the lexical items 
themselves.  This list of lexical items is useful for debugging a grammar; 
it lets us know if GenI managed to lexically select something, but did not 
succeed in anchoring it.

\begin{code}
runLexSelection :: ProgState -> IO ([TagElem], [ILexEntry])
runLexSelection pst = 
  case (grammarType $ pa pst) of  
        XMGTools -> runXMGLexSelection pst
        _        -> runBasicLexSelection pst

runBasicLexSelection :: ProgState -> IO ([TagElem], [ILexEntry])
runBasicLexSelection pst = do
  let (tsem,_) = ts pst
      lexicon  = le pst
      -- select lexical items first 
      lexCand   = chooseLexCand lexicon tsem
      -- then anchor these lexical items to trees
      combiner = combineList (gr pst) 
      cand     = concatMap combiner lexCand
      -- attach any morphological information to the candidates
      morphfn  = morphinf pst
      cand2    = attachMorph morphfn tsem cand 
  return (setTidnums cand2, lexCand)
\end{code}

% --------------------------------------------------------------------
\subsection{Basic selection}
\subsubsection{Combine}
\label{sec:combine_macros}
% --------------------------------------------------------------------

combine: Given 
- the Macros and 
- a list of ILexEntry (read from the Lexicon.in file) 

it creates the Tags repository combining lexical entries and
un-anchored trees from the grammar. It also unifies the parameters
used to specialize un-anchored trees and propagates additional features
given in the ILexEntry. 

\begin{code}
combine :: Macros -> Lexicon -> Tags
\end{code}

We start by collecting all the features and parameters we want to combine.

\begin{code}
combine gram lexicon =
  let helper li = map (combineOne li) macs 
       where tn   = ifamname li
             macs = [ t | t <- gram, pfamily t == tn ]
  in Map.map (\e -> concatMap helper e) lexicon 
\end{code}

\paragraph{combineList} takes a lexical item; it looks up the tree
families for that item, and anchors the item to the trees.  A simple
list of trees is returned.

\begin{code}
combineList :: Macros -> ILexEntry -> [TagElem]
combineList gram lexitem = 
  let tn      = ifamname lexitem
      macs    = [ t | t <- gram, pfamily t == tn ]
      result  = map (combineOne lexitem) macs
      warning = "Warning: family " ++ tn ++ " not found in Macros"
  in unsafePerformIO $ do 
       when (null macs) $ ePutStrLn warning 
       return result 
\end{code}

\paragraph{combineOne} \label{fn:combineOne} combines a single tree with its
lexical item to form a bonafide TagElem

\begin{code}
combineOne :: ILexEntry -> MTtree -> TagElem
combineOne lexitem e = 
   let wt = "(Word: "++ (showLexeme $ iword lexitem) ++
            ", Family:" ++ (ifamname lexitem) ++ ")\n"
       -- lexitem stuff
       sem  = isemantics lexitem
       p    = iparams lexitem
       pf   = ipfeat lexitem
       -- tree stuff
       tp   = map fromGVar $ params e
       tpf  = pfeat e
       -- unify the parameters
       psubst = zip tp p
       paramsUnified = replace psubst (Btypes.tree e)
       -- unify the features
       pf2  = replace psubst pf  
       tpf2 = replace psubst tpf 
       (fsucc, funif, fsubst) = unifyFeat pf2 tpf2
       featsUnified = replace fsubst paramsUnified 
       -- detect subst and adj nodes
       unified = featsUnified
       (snodes,anodes) = detectSites unified 
       -- the final result
       showid i = if null i then "" else ("-" ++ i)
       sol = emptyTE {
                idname = (head $ iword lexitem) ++ "_" 
                         ++ pfamily e ++ showid (pidname e),
                derivation = (0,[]),
                ttype = ptype e,
                ttree = setLexeme (iword lexitem) unified,
                substnodes = snodes,
                adjnodes   = anodes,
                tsemantics = replace fsubst sem,
                tsempols    = isempols lexitem,
                tinterface  = funif
                -- tpredictors = combinePredictors e lexitem
               }        
       -- well... with error checking
       wterror s = error (s ++ " " ++ wt)
       result 
         -- if the parameters are of different length
         | (length p) /= (length tp) = wterror "Wrong number of parameters."
{-
  This check prevents us from using large families of trees with
  very different interfaces

         -- if the lex item features are not a subset of the tree features
         | intersect fpf ftpf /= fpf = error $
             "Lex entry " ++ iword lexitem 
             ++ "'s features are not a subset of"
             ++ " tree's features: "  
             ++ "\nlex:  " ++ show fpf  
             ++ "\ntree: " ++ show ftpf
-}
         -- if fs unification fails for any reason (probably a bug)
         | not fsucc = wterror "Feature unification failed."
         -- success! 
         | otherwise = sol
   in result
\end{code}

\paragraph{detectSites}: Given a tree(GNode) 
returns a list of substitution or adjunction nodes

\begin{code}
detectSites :: Tree GNode -> ([TagSite], [TagSite])
detectSites (Node a lt) =
  let next = map detectSites lt
      (snodes', anodes') = unzip next
      --
      site = [(gnname a, gup a, gdown a)]
      snodes = if (gtype a == Subs) then (site:snodes') else snodes' 
      anodes = if (gaconstr a)      then anodes' else (site:anodes')
  in (concat snodes, concat anodes)
\end{code}

% --------------------------------------------------------------------
\subsubsection{The selection process}
% --------------------------------------------------------------------

\paragraph{chooseLexCand} selects and returns the set of entries from
the lexicon whose semantics subsumes the input semantics. 

\begin{code}
chooseLexCand :: Lexicon -> Sem -> [ILexEntry]
chooseLexCand slex tsem = 
  let -- the initial "MYEMPTY" takes care of items with empty semantics
      keys = myEMPTY:(toKeys tsem)   
      -- we choose candidates that match keys
      lookuplex t = Map.findWithDefault [] t slex
      cand  = concatMap lookuplex keys
      -- and refine the selection... 
      cand2 = chooseCandI tsem cand
      -- treat synonyms as a single lexical entry
      cand3 = cand2
  in cand3
\end{code}

With a helper function, we refine the candidate selection by
instatiating the semantics, at the same time filtering those which
do not stay within the target semantics, and finally eliminating 
the duplicates.

\begin{code}
chooseCandI :: Sem -> [ILexEntry] -> [ILexEntry]
chooseCandI tsem cand =
  let replaceLex i (sem,sub) = 
        (replace sub i) { isemantics = sem }
      --
      helper :: ILexEntry -> [ILexEntry]
      helper le = if (null sem) then [le] 
                  else map (replaceLex le) psubsem 
        where psubsem = subsumeSem tsem sem
              sem = isemantics le
      --
  in nub $ concatMap helper cand 
\end{code}

\paragraph{mapBySemKeys} organises items by their semantic key.  A
semantic key is a semantic literal boiled down to predicate plus arity
(see section \ref{btypes_semantics}).  Given \texttt{xs} a list of items
and \texttt{fn} a function which retrieves the item's semantics, we
return a Map from semantic key to a list of items with that key.
An item may have multiple keys.

This is used to organise the lexicon by its semantics.

\begin{code}
mapBySemKeys :: (a -> Sem) -> [a] -> Map.Map String [a]
mapBySemKeys semfn xs = 
  let gfn t = if (null s) then [myEMPTY] else toKeys s 
              where s = semfn t
  in multiGroupByFM gfn xs
\end{code}

\fnlabel{mergeSynonyms} is a factorisation technique that uses
atomic disjunction to merge all synonyms into a single lexical
entry.  Two lexical entries are considered synonyms if their
semantics match and they point to the same tree families.

\begin{code}
mergeSynonyms :: [ILexEntry] -> [ILexEntry]
mergeSynonyms lex =
  let mergeFn l1 l2 = l1 { iword = (iword l1) ++ (iword l2) }
      keyFn l = (ifamname l, isemantics l)   
      synMap = foldr helper Map.empty lex  
        where helper x acc = Map.insertWith mergeFn (keyFn x) x acc 
  in Map.elems synMap
\end{code}

% --------------------------------------------------------------------
\subsection{XMG selection}
\label{sec:xmg_selection}
% --------------------------------------------------------------------

XMG is a metagrammar compiler.  GenI supports XMG grammars which obey
the LORIA common grammar manifesto \cite{kow05CGM}.  This is an
attempt to build a reversible TAG grammar and lexicon, that is, one
that can be used for parsing and generation.  

When you are using XMG stuff, three things are different:
\begin{enumerate}
\item The grammar format is different (.rec pickles produced by XMG)
\item The lexicon format is different (See the Common Grammar Manifesto site) 
\item Tree anchoring is farmed out to a third party tool we call the
the Selector.  This module handles most of the XMG-specific bits of 
the lexical selection process via a XMG lexicon.  
\end{enumerate}

Hopefully one day, we will be able to refactor some of the differences
between GenI reading its own grammar and using XMG tools.

\paragraph{runXMGLexSelection} is the front end to the XMG lexical
selection process.  

\begin{code}
runXMGLexSelection :: ProgState -> IO ([TagElem], [ILexEntry])
runXMGLexSelection pst = 
  do let (tsem,_) = ts pst
         lexicon  = le pst
     -- figure out what grammar file to use
     let gparams  = pa pst
         gramfile = macrosFile gparams
     -- select lexical items 
         lexCand = chooseLexCand lexicon tsem
     -- run the selector module
     let idxs = [1..]
         fil  = concat $ zipWith lexEntryToFil lexCand idxs 
     selected <- runSelector pst gramfile fil
     let parsed = runParser geniMacros () "" selected 
         g = case parsed of Left err -> error (show err) 
                            Right gr -> gr
     --
     let lexMap = Map.fromList $ zip (map show idxs) lexCand
         fixate :: MTtree -> IO TagElem 
         fixate ts = 
           case (Map.lookup id lexMap) of
             Nothing  -> fail ("no such lexical entry " ++ id)
             Just lex -> return $ combineXMG lex ts 
           where id = takeWhile (/= 'x') $ tail $ pfamily ts --FIXME: hack!
     cand <- mapM fixate g
     -- attach any morphological information to the candidates
     let morphfn  = morphinf pst
         cand2    = attachMorph morphfn tsem cand 
     return (setTidnums cand2, lexCand)
  -- FIXME: determine if we can nix this error handler
  --`catch` \e -> do ePutStrLn (show e)
  --                 return ([], []) 
\end{code}

\subsubsection{Calling the Selector}

\paragraph{runSelector} calls the Selector, passing it the
grammar file (argument \fnparam{gfile}) and the
lexical selection (argument \fnparam{l}).
It returns a list of anchored trees.

The selector is expected to read cgm filter stuff 
(see \cite{kow05CGM} and lexEntryToFil below) and output 
a set of geni formatted trees\footnote{What actually happens
is that it outputs a set of XML trees; and we use the XML
converter in \ref{cha:xml} to convert that to geni format}.

\begin{code}
runSelector :: ProgState -> String -> String -> IO String 
runSelector pst gfile fil = do
#ifdef mingw32_BUILD_OS
     ePutStr $ "Selector not available under Windows until Eric"
              ++ " figures out all this Posix stuff.\n"
     return ""
#else
      -- run the selector
     let theCmd  = selectCmd (pa pst)
         selectArgs = [gfile]
     when (null theCmd) $ fail "Please specify a tree selection command!"
     ePutStr $ "Selector started.\n"
     eFlush
     -- (toP, fromP, _, pid) <- runInteractiveProcess theCmd selectArgs Nothing Nothing
     (pid, fromP, toP) <- runPiped theCmd selectArgs Nothing Nothing
     hPutStrLn toP fil
     hClose toP -- so that process gets EOF and knows it can stop
     res    <- hGetContents fromP 
     -- waitForProcess pid
     awaitProcess pid 
     {- exCode <- awaitProcess pid 
       case exCode of 
       Just ExitSuccess -> return ()
       _ -> fail "There was a problem running the selector. Check your terminal." -}
     return res
#endif
\end{code}

\paragraph{lexEntryToFil} converts a lexical entry to a XMG filter for
use by the selection module.  The selection module is a third-party
program which selects the trees from a grammar that correspond to each
lexical item and which performs enrichement and anchoring.  The
arguments are \fnparam{lex}, the lexical item to convert; and
\fnparam{n}, the numerical id you wish to associate to that item.  

Note: One weird thing we do in this function is to add some stuff to
the path equations for enrichment.  This stuff corresponds to semantic
arguments.  For example, if the semantics of the lexical item is
\semexpr{hate(m,j)}, we add the equations \verb!interface.arg1=m! and
\verb!interface.arg2=j!.  In order to do this, we assume that there is
only one literal in the lexical item semantics.

\begin{code}
lexEntryToFil :: ILexEntry -> Int -> String
lexEntryToFil lex n = 
  let filters   = ifilters lex
      enrichers = ipfeat lex 
      --
      showFil (a,v) = a ++ ":" ++ show v
      showEnr (a,v) = a ++ "=" ++ show v
      concatSperse x y = concat $ intersperse x y
  in show n 
    ++ " " ++ (showLexeme $ iword lex) ++ " "
    ++ "[" 
    ++ (concatSperse ","   $ map showFil filters)
    ++ "]\n(" 
    ++ (concatSperse ",\n" $ map showEnr enrichers)
    ++ ")\n\n"
\end{code}

\paragraph{combineXMG} is similar to \fnref{combineOne} except that we assume
the tree is completely anchored and instatiated and that thus there no boring
unification or checks to worry about.

\begin{code}
combineXMG :: ILexEntry -> MTtree -> TagElem
combineXMG lexitem e = 
   let tree_ = Btypes.tree e
       (snodes,anodes) = detectSites tree_
       sol = emptyTE {
                idname = (showLexeme $ iword lexitem) ++ "_" ++ pidname e,
                derivation = (0,[]),
                ttype  = ptype e,
                ttree  = tree_,
                substnodes = snodes,
                adjnodes   = anodes,
                -- FIXME: we completely ignore semantic info from the tree
                -- this allows us to handle multi-literal semantics
                tsemantics = isemantics lexitem,
                tpolarities = Map.empty,
                tsempols    = isempols lexitem,
                tinterface  = pfeat e
                -- tpredictors = combinePredictors e lexitem
               }        
   in sol 
\end{code}

% --------------------------------------------------------------------
\section{Morphology} 
% --------------------------------------------------------------------

\paragraph{runMorph} inflects a list of sentences if a morphlogical generator
has been specified.  If not, it returns the sentences as lemmas.

\begin{code}
runMorph :: ProgStateRef -> [[(String,Flist)]] -> IO [String]
runMorph pstRef sentences = 
  do pst <- readIORef pstRef
     let mcmd = morphCmd (pa pst)
     if null mcmd
        then return (map sansMorph sentences)
        else inflectSentences mcmd sentences 
\end{code}


