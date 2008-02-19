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

\section{GraphvizShow}

Outputting core GenI data to graphviz.

\begin{code}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module NLP.GenI.GraphvizShow
where
\end{code}

\ignore{
\begin{code}
import Data.List(intersperse,nub)

import NLP.GenI.Tags
 ( TagElem, TagDerivation, idname,
   tsemantics, ttree,
 )
import NLP.GenI.Btypes (GeniVal(GConst), AvPair,
               GNode(..), GType(..), Flist,
               isConst,
               showSem,
               )
import NLP.GenI.General (wordsBy)
import NLP.GenI.Graphviz
  ( gvUnlines, gvNewline
  , GraphvizShow(graphvizShowAsSubgraph, graphvizLabel, graphvizParams)
  , GraphvizShowNode(graphvizShowNode)
  , GraphvizShowString(graphvizShow)
  , gvNode, gvEdge, gvShowTree
  )

\end{code}
}

% ----------------------------------------------------------------------
\section{For GraphViz}
% ----------------------------------------------------------------------

\begin{code}
type GvHighlighter a = a -> (a, Maybe String)

nullHighlighter :: GvHighlighter GNode
nullHighlighter a = (a,Nothing)

instance GraphvizShow Bool TagElem where
 graphvizShowAsSubgraph sf = graphvizShowAsSubgraph (sf, nullHighlighter)
 graphvizLabel  sf = graphvizLabel (sf, nullHighlighter )
 graphvizParams sf = graphvizParams (sf, nullHighlighter)


instance GraphvizShow (Bool, GvHighlighter GNode) TagElem where
 graphvizShowAsSubgraph (sf,hfn) prefix te =
    (gvShowTree (\_->[]) sf (prefix ++ "DerivedTree0") $
     fmap hfn $ ttree te)

 graphvizLabel _ te =
  -- we display the tree semantics as the graph label
  let treename   = "name: " ++ (idname te)
      semlist    = "semantics: " ++ (showSem $ tsemantics te)
  in gvUnlines [ treename, semlist ]

 graphvizParams _ _ =
  [ "fontsize = 10", "ranksep = 0.3"
  , "node [fontsize=10]"
  , "edge [fontsize=10 arrowhead=none]" ]
\end{code}

Helper functions for the TagElem GraphvizShow instance

\section{GNode - GraphvizShow}

\begin{code}
instance GraphvizShowNode (Bool) (GNode, Maybe String) where
 -- compact -> (node, mcolour) -> String
 graphvizShowNode detailed prefix (gn, mcolour) =
   let -- attributes
       filledParam         = ("style", "filled")
       fillcolorParam      = ("fillcolor", "lemonchiffon")
       shapeRecordParam    = ("shape", "record")
       shapePlaintextParam = ("shape", "plaintext")
       --
       colorParams = case mcolour of
                     Nothing -> []
                     Just c  -> [ ("fontcolor", c) ]
       shapeParams = if detailed
                     then [ shapeRecordParam, filledParam, fillcolorParam ]
                     else [ shapePlaintextParam ]
       -- content
       stub  = showGnStub gn
       extra = showGnDecorations gn
       summary = if null extra then stub
                 else "{" ++ stub ++ "|" ++ extra ++ "}"
       --
       body = if not detailed then graphvizShow_ gn
              else    "{" ++ summary
                   ++ (barAnd.showFs $ gup gn)
                   ++ (maybeShow (barAnd.showFs) $ gdown gn)
                   ++ "}"
        where barAnd x = "|" ++ x
              showFs = gvUnlines . (map graphvizShow_)
   in gvNode prefix body (shapeParams ++ colorParams)
\end{code}

\begin{code}
instance GraphvizShowString () GNode where
  graphvizShow () gn =
    let stub  = showGnStub gn
        extra = showGnDecorations gn
    in stub ++ maybeShow_ " " extra

instance GraphvizShowString () AvPair where
  graphvizShow () (a,v) = a ++ ":" ++ (graphvizShow_ v)

instance GraphvizShowString () GeniVal where
  graphvizShow () (GConst x) = concat $ intersperse " ! " x
  graphvizShow () x = show x

showGnDecorations :: GNode -> String
showGnDecorations gn =
  case gtype gn of
  Subs -> "!"
  Foot -> "*"
  _    -> if (gaconstr gn) then "#"   else ""

showGnStub :: GNode -> String
showGnStub gn =
 let cat = case getGnVal gup "cat" gn of
           Nothing -> ""
           Just v  -> graphvizShow_ v
     --
     getIdx f =
       case getGnVal f "idx" gn of
       Nothing -> ""
       Just v  -> if isConst v then graphvizShow_ v else ""
     idxT = getIdx gup
     idxB = getIdx gdown
     idx  = idxT ++ (maybeShow_ "." idxB)
     --
     lexeme  = concat $ intersperse "!" $ glexeme gn
 in concat $ intersperse ":" $ filter (not.null) [ cat, idx, lexeme ]

getGnVal :: (GNode -> Flist) -> String -> GNode -> Maybe GeniVal
getGnVal getFeat attr gn =
  case [ av | av <- getFeat gn, fst av == attr ] of
  []     -> Nothing
  (av:_) -> Just (snd av)

-- | Apply fn to s if s is not null
maybeShow :: ([a] -> String) -> [a] -> String
maybeShow fn s = if null s then "" else fn s
-- | Prefix a string if it is not null
maybeShow_ :: String -> String -> String
maybeShow_ prefix s = maybeShow (prefix++) s

graphvizShow_ :: (GraphvizShowString () a) => a -> String
graphvizShow_ = graphvizShow ()
\end{code}

% ----------------------------------------------------------------------
\section{Derivation tree}
% ----------------------------------------------------------------------

\begin{code}
graphvizShowDerivation :: TagDerivation -> String
graphvizShowDerivation deriv =
  if (null histNodes)
     then ""
     else " node [ shape = plaintext ];\n"
          ++ (concatMap showHistNode histNodes)
          ++ (concatMap graphvizShowDerivation' deriv)
  where showHistNode n  = gvNode (gvDerivationLab n) (label n) []
        label n = case wordsBy ':' n of
                  name:fam:tree:_ -> name ++ ":" ++ fam ++ gvNewline ++ tree
                  _               -> n ++ " (geni/gv ERROR)"
        histNodes       = reverse $ nub $ concatMap (\ (_,c,(p,_)) -> [c,p]) deriv
\end{code}

\begin{code}
graphvizShowDerivation' :: (Char, String, (String, String)) -> String
graphvizShowDerivation' (substadj, child, (parent,_)) =
  gvEdge (gvDerivationLab parent) (gvDerivationLab child) "" p
  where p = if substadj == 'a' then [("style","dashed")] else []
\end{code}

We have a couple of functions to help massage our data into Graphviz input
format: node names can't have hyphens in them and newlines within the node
labels should be represented literally as \verb$\n$.

\begin{code}
gvDerivationLab :: String -> String
gvDerivationLab xs = "Derivation" ++ gvMunge xs

newlineToSlashN :: Char -> String
newlineToSlashN '\n' = gvNewline
newlineToSlashN x = [x]

gvMunge :: String -> String
gvMunge = map dot2x . filter (/= ':') . filter (/= '-')

dot2x :: Char -> Char
dot2x '.' = 'x'
dot2x c   = c
\end{code}
