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
import Data.List(delete,intersperse,nub)

import Tags (TagElem, idname, tdiagnostic, 
             tsemantics, ttree, thighlight, tinterface, 
             derivation)
import Btypes (MTtree, Ttree(..), Ptype(..), 
               GNode(..), GType(..), Flist,
               showLexeme,
               showSem, showPairs, showAv)
\end{code}
}

% ----------------------------------------------------------------------
\section{For GraphViz}
% ----------------------------------------------------------------------

\paragraph{graphVizShowTagElem} converts a TAG tree into Graphviz's
\textit{dot} format.

\begin{code}
graphvizShowTagElem :: Bool -> TagElem -> String
graphvizShowTagElem sf te =
     -- we want a directed graph (but without arrows)
     "digraph " ++ (dehyphen $ idname te) ++ " {\n" 
     ++ " fontsize = 10\n"
     ++ " ranksep = \"0.3\"\n"
     ++ " node [ fontsize=10 ]\n"
     ++ " edge [ arrowhead = none ];\n"
     -- we display the tree semantics as the graph label
     ++ " label = \"" ++ label ++ "\";\n" 
     -- derived tree nodes' labels are their feature structures 
     ++ " node [ shape = plaintext ];\n" 
     ++ (graphvizShowInterface sf (tinterface te))
     ++ (graphvizShow' sf (thighlight te) (ttree te) "DerivedTree0") 
     -- derivation tree is displayed without any decoration
     ++ (graphvizShowDerivation $ snd $ derivation te)
     --
     ++ "}\n" 
   where treename = "name: " ++ (idname te)
         semlist  = "semantics: " ++ (showSem $ tsemantics te)
         tdiag = if null s then "" else "\\n" ++ s 
           where s = concat $ intersperse "\\n" (tdiagnostic te)
         label    = treename ++ "\\n" ++ semlist ++ tdiag
\end{code}

\paragraph{graphvizShow'} invokes a helper function which walks the actual
tree, converts the nodes and connects parent nodes to child nodes in the 
output representation.

The label argument is the name of the parent node.  This is used to
build node names for Graphviz.  We use simple Gorn addresses 
(e.g. n0x2x3 means 3rd child of the 2nd child of the root) to keep
them distinct.  Note : We use the letter `x' as seperator because 
graphviz will choke on `.' or `-', even underscore.

\begin{code}
graphvizShow' :: Bool -> [String] -> Tree GNode -> String -> String
graphvizShow' sf hl (Node node l) label =
   let showFn   = if sf then showGNodeAll else show 
       shapeStr = if sf then "shape = record, " else ""
       -- highlight any specially marked nodes
       (hlStr, hl2) = if (n `elem` hl)
                      then ("color = red, fontcolor = red, ", delete n hl)
                      else ("", hl)
                      where n = gnname node 
       shownode = " " ++ label ++ " [ " ++ hlStr ++ shapeStr 
                  ++ "label = \"" ++ showFn node ++ "\"];\n"       
       kidname index = (label ++ "x" ++ (show index))
       -- we show the kid and the fact that the node is connected
       -- to the kid
       showkid (index,kid) = (graphvizShow' sf hl2 kid (kidname index)) ++
                             " " ++ label ++ " -> " ++ (kidname index) ++ ";\n"
       -- now let's run this thing!
   in shownode ++ (concatMap showkid $ zip [0..] l)
\end{code}

\paragraph{showGNodeAll} shows everything you would want to know about a
gnode, probably more than you want to know

\begin{code}
showGNodeAll gn = 
  let showFs l = (concat $ intersperse "\\n" $ map showAv l)
      sgup = if (null $ gup gn) 
             then "" 
             else showFs (gup gn) 
      sgdown = if (null $ gdown gn)
               then ""
               else "|" ++ showFs (gdown gn) 
      label  = show gn
  in "{" ++ label ++ "|" ++ sgup ++ sgdown ++ "}"-- (show gn ++ "\n" ++)
\end{code}

\paragraph{graphvizShowInterface} displays the contents of a TagElem's
interface.  This is only used for debugging.

\begin{code}
graphvizShowInterface :: Bool -> Flist -> String
graphvizShowInterface sf iface 
  | sf == False = ""
  | null iface  = ""
  | otherwise =
      let showFs = concat $ intersperse "\\n" $ map showAv iface 
          str    = " interface [ shape = record, " 
                   ++ "label = \"{ interface | " ++ showFs ++ "}\"];\n"       
      in str
\end{code}

% ----------------------------------------------------------------------
\section{GeniHand}
% ----------------------------------------------------------------------

To make large grammars faster to load, we include a mechanism for
writing a TagElem to the GeniHand format.  The idea is to take a massive
XML grammar, parse it to a set of TagElems and then write these back in
the lighter syntax.  It's not that XML is inherently less efficient to
parse than the handwritten syntax, just that writing an efficient parser
for XML based format is more annoying, so I stuck with HaXml to make my
life easy.  It would be useful eventually to see if I could just use
some kind of SAX based method.

\begin{code}
toGeniHand :: MTtree -> String
toGeniHand tr = 
  let showid t = dashtobar fam ++ (if null id then "" else ":" ++ id)
        where fam = pfamily t
              id  = pidname t
      --
      ptypestr t = case t of 
                     Initial  -> "initial" 
                     Auxiliar -> "auxiliary" 
                     _        -> ""
      --
      gtypestr n = case (gtype n) of 
                     Subs -> "type:subst"
                     Foot -> "type:foot"
                     Lex  -> if (ganchor n) then "type:anchor" else "type:lex" 
                     _    -> ""
      glexstr  n = if null gl then "" else "\"" ++ gl ++ "\""
                   where gl = showLexeme $ glexeme n
      --
      nodestr :: GNode -> String
      nodestr n = "n" ++ gnname n 
                  ++ " " ++ gtypestr n ++ " " ++ glexstr n ++ " "
                  ++ "[" ++ showflist (gup n) ++ "]!"
                  ++ "[" ++ showflist (gdown n) ++ "]"
      --
      treestr :: Int -> Tree GNode -> String
      treestr i (Node a []) = spaces i ++ nodestr a ++  
                              (if (i == 0) then "{}" else "") ++ "\n"
      treestr i (Node a l)  = spaces i ++ nodestr a ++ "{\n"
                              ++ concatMap nextfn l ++ spaces i ++ "}\n"
                              where nextfn = treestr (i+1)
      -- misc helpers
      spaces :: Int -> String 
      spaces i = take i $ repeat ' '
      -- helpers to account for shortcomings in genihand lexer 
      dashtobar :: String -> String 
      dashtobar s = map (\c -> if ('-' == c) then '_' else c) s
      substf (a,v) = case (a,v) of _ -> (dashtobar a, v)
      --
      showflist  = showPairs . (map substf)
      showparams = concat $ intersperse " " $ map show (params tr)
      --
  in showid tr 
     ++ " ("  ++ showparams 
     ++ " ! " ++ showflist (pfeat tr) ++ ")"
     ++ " " ++ (ptypestr.ptype) tr 
     ++ "\n" ++ ((treestr 0).tree) tr ++ "\n"
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
  where showHistNode n  = gvDerivationLab n 
                        ++ " [label=\"" ++ (dropTillDot n) ++ "\"];\n"
        histNodes       = reverse $ nub $ concatMap (\ (_,c,p) -> [c,p]) deriv
\end{code}

\begin{code}
graphvizShowDerivation' :: (Char, String, String) -> String
graphvizShowDerivation' (substadj, child, parent) = 
  (gvDerivationLab parent) ++ " -> " ++ (gvDerivationLab child) ++         
  (if (substadj == 'a') then " [style=dashed]" else "") ++
  ";\n"
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
newlineToSlashN x = if ('\n' == x) then "\\n" else [x] 

dot2x :: Char -> Char 
dot2x c = if ('.' == c) then 'x' else c 

dropTillDot :: String -> String
dropTillDot l = f [] l 
  where f acc []     = reverse acc
        f acc (x:xs) = if (x == '.') then f [] xs else f (x:acc) xs
\end{code}   


