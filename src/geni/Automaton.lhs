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

\chapter{Automaton}
\label{cha:Automaton}

\begin{code}
module Automaton 
  ( NFA(..), 
    finalSt,
    addTrans, lookupTrans,
    automatonPaths, automatonPathSets,
    numStates, numTransitions )
where

import qualified Data.Map as Map
import Data.Maybe (catMaybes)

import General (combinations)
\end{code}

This module provides a simple, naive implementation of nondeterministic
finite automata (NFA).  The transition function consists of a Map, but 
there are also accessor function which help you query the automaton 
without worrying about how it's implemented.

\begin{enumerate}
\item The states are a list of lists, not just a simple flat list as 
  you might expect.  This allows you to optionally group your 
  states into ``columns'' (which is something we use in the 
  GenI polarity automaton optimisation).  If you don't want 
  columns, you can just make one big group out of all your states.
\item We model the empty an empty transition as the transition on
  Nothing.  All other transitions are Just something.
\item I'd love to reuse some other library out there, but Leon P. Smith's
  Automata library requires us to know before-hand the size of our alphabet,
  which is highly unacceptable for this task.  
\end{enumerate}

\begin{code}
-- | Note: there are two ways to define the final states.
-- 1. you may define them as a list of states in finalStList
-- 2. you may define them via the isFinalSt function
-- The state list is ignored if you define 'isFinalSt'
data NFA st ab = NFA 
  { startSt     :: st
  , isFinalSt   :: Maybe (st -> Bool)
  , finalStList :: [st]
  -- 
  , transitions :: Map.Map st (Map.Map st [Maybe ab])
  -- see chapter comments about list of list 
  , states    :: [[st]] 
  }
\end{code}

% ----------------------------------------------------------------------
\section{Building automata}
% ----------------------------------------------------------------------

\fnlabel{finalSt} returns all the final states of an automaton

\begin{code}
finalSt :: NFA st ab -> [st]
finalSt aut =
  case isFinalSt aut of
  Nothing -> finalStList aut
  Just fn -> concatMap (filter fn) (states aut)
\end{code}

\fnlabel{lookupTrans} takes an automaton, a state \fnparam{st1} and an
element \fnparam{ab} of the alphabet; and returns the state that 
\fnparam{st1} transitions to via \fnparam{a}, if possible. 

\begin{code}
lookupTrans :: (Ord ab, Ord st) => NFA st ab -> st -> (Maybe ab) -> [st]
lookupTrans aut st ab = Map.keys $ Map.filter (elem ab) subT
  where subT = Map.findWithDefault Map.empty st (transitions aut) 
\end{code}

\begin{code}
addTrans :: (Ord ab, Ord st) => NFA st ab -> st -> Maybe ab -> st -> NFA st ab 
addTrans aut st1 ab st2 = 
  aut { transitions = Map.insert st1 newSubT oldT }
  where oldT     = transitions aut
        oldSubT  = Map.findWithDefault Map.empty st1 oldT 
        newSubT  = Map.insertWith (++) st2 [ab] oldSubT
\end{code}

% ----------------------------------------------------------------------
\section{Exploiting automata}
% ----------------------------------------------------------------------

\fnlabel{automatonPaths} returns all possible paths through an
automaton.  Each path is represented as a list of labels.

We assume that the automaton does not have any loops
in it.  Maybe it would still work if there were loops, with lazy
evaluation, but I haven't had time to think this through, so only
try it unless you're feeling adventurous.

FIXME: we should write some unit tests and quickchecks for this
\begin{code}
automatonPaths :: (Ord st, Ord ab) => (NFA st ab) -> [[ab]]
automatonPaths aut = concatMap combinations $ map (filter (not.null)) $ automatonPathSets aut

-- | Not quite the set of all paths, but the sets of all transitions
---  FIXME: explain later
automatonPathSets :: (Ord st, Ord ab) => (NFA st ab) -> [[ [ab] ]]
automatonPathSets aut = helper (startSt aut)
 where
  transFor st = Map.lookup st (transitions aut)
  helper st = case transFor st of
              Nothing   -> []
              Just subT -> concat [ (next (catMaybes tr) st2) | (st2, tr) <- Map.toList subT ]
  next tr st2 = case helper st2 of
                []  -> [[tr]]
                res -> map (tr :) res
\end{code}

\begin{code}
numStates, numTransitions :: NFA st ab ->  Int
numStates = sum . (map length) . states
numTransitions = sum . (map subTotal) . (Map.elems) . transitions
  where subTotal = sum . (map length) . (Map.elems)
\end{code}

