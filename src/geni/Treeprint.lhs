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

\chapter{Treeprint}

The Treeprint module provides specialised functions for visualising tree data.

\begin{code}
module Treeprint 
where
\end{code}

\ignore{
\begin{code}
import Data.Tree
import Data.List(intersperse,nub)

import General (mapTree)
import Tags
 ( TagElem, idname,
   tsemantics, ttree, tinterface, ttype, ttreename,
 )
import Btypes (GeniVal, Ptype(..),
               GNode(..), GType(..), Flist,
               showLexeme,
               Sem, showSem, showPairs, showAv)

import Graphviz 
  ( gvUnlines, gvNewline
  , GraphvizShow(graphvizShowAsSubgraph, graphvizLabel, graphvizParams)
  , GraphvizShowNode(graphvizShowNode)
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
  -- the interface
  (graphvizShowInterface sf $ tinterface te)
  -- then the tree itself
  ++ (gvShowTree (\_->[]) sf (prefix ++ "DerivedTree0") $
     mapTree hfn $ ttree te)

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

\begin{code}
-- | shows the contents of the TAG tree's interface
-- Used for debugging grammars when surface realisaiton goes wrong.
graphvizShowInterface :: Bool -> Flist -> String
graphvizShowInterface sf iface 
  | sf == False = ""
  | null iface  = ""
  | otherwise =
      let showFs   = gvUnlines $ map showAv iface 
          theLabel = "{ interface | " ++ showFs ++ "}"
      in gvNode "interface" theLabel [ ("shape", "record") ]
\end{code}

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
       cat  = if null c then "" else (show.snd.head) c
              where c = [ av | av <- gup gn, fst av == "cat" ]
       lex  = concat $ intersperse "!" $ glexeme gn
       stub = if (not (null cat || null lex))
              then cat ++ ":" ++ lex 
              else cat ++ lex 
       extra = case (gtype gn) of         
               Subs -> "!"
               Foot -> "*"
               _    -> if (gaconstr gn) then "#"   else ""
       summary = if null extra then stub 
                 else "{" ++ stub ++ "|" ++ extra ++ "}"
       --
       body = if not detailed then show gn 
              else    "{" ++ summary
                   ++ "|" ++ (showFs $ gup gn) 
                   ++ (if (null $ gdown gn) then "" else "|" ++ (showFs $ gdown gn)) 
                   ++ "}"
        where showFs = gvUnlines . (map showAv) 
   in gvNode prefix body (shapeParams ++ colorParams)
\end{code}

% ----------------------------------------------------------------------
\section{Derivation tree}
% ----------------------------------------------------------------------

\paragraph{graphvizShowDerivation} displays the derivation tree.
This is actually trickier than it looks: one thing we need to prevent 
for is the potential for loops in the graph (not infinite loops, but
diagrams with loops in them).  For example if I am a NomRel, I can 
both attach to a det and have a det attached to me.  Oops.  The basic
trick is to treat each derivation item as a node in the derivation tree.

\begin{code}
graphvizShowDerivation :: [(Char, String, String)] -> String
graphvizShowDerivation deriv =  
  if (null histNodes) 
     then "" 
     else " node [ shape = plaintext ];\n" 
          ++ (concatMap showHistNode histNodes) 
          ++ (concatMap graphvizShowDerivation' deriv)
  where showHistNode n  = gvNode (gvDerivationLab n) (lastSegmentOf n) []
        lastSegmentOf   = reverse . takeWhile (/= '.') . reverse 
        histNodes       = reverse $ nub $ concatMap (\ (_,c,p) -> [c,p]) deriv
\end{code}

\begin{code}
graphvizShowDerivation' :: (Char, String, String) -> String
graphvizShowDerivation' (substadj, child, parent) = 
  gvEdge (gvDerivationLab parent) (gvDerivationLab child) "" params
  where params = if substadj == 'a' then [("style","dashed")] else []
\end{code}

We have a couple of functions to help massage our data into Graphviz input 
format: node names can't have hyphens in them and newlines within the node
labels should be represented literally as \verb$\n$.

\begin{code}
gvDerivationLab :: String -> String
gvDerivationLab xs = "Derivation" ++ (map dot2x $ dehyphen xs)

dehyphen :: String -> String
dehyphen = filter (/= '-')

newlineToSlashN :: Char -> String
newlineToSlashN '\n' = gvNewline
newlineToSlashN x = [x]

dot2x :: Char -> Char 
dot2x '.' = 'x'
dot2x c   = c
\end{code}   

% ----------------------------------------------------------------------
\section{GeniHand}
% ----------------------------------------------------------------------

We need to be able to dump some of GenI's data structures into a simple
text format we call GeniHand.

There are at leaste two uses for this, one is that it allows us to
interrupt the debugging process, dump everything to file, muck around
with the trees and then pick up where we left off.

The other use is to make large grammars faster to load.  We don't actually do
this anymore, mind you, but it's nice to have the option.  The idea is to take
a massive XML grammar, parse it to a set of TagElems and then write these back
in the lighter syntax.  It's not that XML is inherently less efficient to parse
than the handwritten syntax, just that writing an efficient parser for XML
based format is more annoying, so I stuck with HaXml to make my life easy.
Unfortunately, HaXml seems to have some kind of space leak.

\begin{code}
class GeniHandShow a where
  toGeniHand :: a -> String

instance GeniHandShow Sem where
 toGeniHand = showSem

instance GeniHandShow Flist where
 toGeniHand = showPairs

instance GeniHandShow Ptype where
 toGeniHand Initial  = "initial"
 toGeniHand Auxiliar = "auxiliary"
 toGeniHand _        = ""

instance GeniHandShow [GeniVal] where
 toGeniHand = unwords . (map show)

instance GeniHandShow GNode where
 toGeniHand n =
  let gtypestr n = case (gtype n) of
                     Subs -> "type:subst"
                     Foot -> "type:foot"
                     Lex  -> if (ganchor n) then "type:anchor" else "type:lex"
                     _    -> ""
      glexstr  n = if null gl then "" else "\"" ++ gl ++ "\""
                   where gl = showLexeme $ glexeme n
  in "n" ++ gnname n
         ++ " " ++ gtypestr n ++ " " ++ glexstr n ++ " "
         ++ (squares.toGeniHand $ gup n) ++ "!"
         ++ (squares.toGeniHand $ gdown n)

instance (GeniHandShow a) => GeniHandShow (Tree a) where
 toGeniHand t =
  let treestr i (Node a l) =
        spaces i ++ toGeniHand a ++
        case (l,i) of
        ([], 0) -> "{}"
        ([], _) -> ""
        (l,  i) -> "{\n" ++ concatMap next l
                         ++ spaces i ++ "}\n"
        where next = treestr (i+1)
      --
      spaces i = take i $ repeat ' '
  in treestr 0 t

instance GeniHandShow TagElem where
 toGeniHand te =
  ttreename te
  ++ " "  ++ (squares.toGeniHand.tinterface $ te)
  ++ " "  ++ (toGeniHand.ttype $ te)
  ++ "\n" ++ (toGeniHand.ttree $ te)
  ++ "\n" ++ "semantics:" ++ (squares.toGeniHand.tsemantics $ te)
  ++ "\n"

squares s = "[" ++ s ++ "]"
\end{code}
