\chapter{Treeprint}

The Treeprint module provides specialised functions for visualising tree data.

\begin{code}
module Treeprint 
where
\end{code}

\ignore{
\begin{code}
import Data.Char(toUpper)
import Data.Tree
import Data.List(intersperse,nub)

import Tags (TagElem, idname, tsemantics, ttree, derivation, showfeats)
import Bfuncs (MTtree, Ttree(..), Ptype(..), 
               GNode(..), GType(..),
               showSem, showGNodeAll, showPairs)
import Graphviz (GraphvizShow(..))
-- import Debug.Trace 
\end{code}
}

% ----------------------------------------------------------------------
\section{For GraphViz}
% ----------------------------------------------------------------------

graphVizShow converts a TAG tree into Graphviz's \textit{dot} format.

\begin{code}
instance GraphvizShow TagElem where
  graphvizShow te =
     -- we want a directed graph (but without arrows)
     "digraph " ++ (dehyphen $ idname te) ++ " {\n" 
     ++ " fontsize = 10\n"
     ++ " ranksep = \"0.3\"\n"
     ++ " node [ fontsize=10 ]\n"
     ++ " edge [ arrowhead = none ];\n"
     -- we display the tree semantics as the graph label
     ++ " label = \"" ++ label ++ "\";\n" 
     -- derivation tree is displayed without any decoration
     ++ (graphvizShowDerivation $ snd $ derivation te)
     -- derived tree nodes' labels are their feature structures 
     ++ " node [ shape = plaintext ];\n" 
     ++ (graphvizShow' (showfeats te) (ttree te) "DerivedTree0") ++ "}\n" 
   where treename = "name: " ++ (idname te)
         semlist  = "semantics: " ++ (showSem $ tsemantics te)
         label    = treename ++ "\\n" ++ semlist 
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
graphvizShow' :: Bool -> Tree GNode -> String -> String
graphvizShow' sf (Node node l) label =
   let showFn   = if sf then showGNodeAll else show 
       node'    = concatMap newlineToSlashN (showFn node)
       shownode = " " ++ label ++ " [ label = \"" ++ node' ++ "\"];\n"       
       pairs    = zip [0..] l
       kidname index = (label ++ "x" ++ (show index))
       -- we show the kid and the fact that the node is connected
       -- to the kid
       showkid (index,kid) = (graphvizShow' sf kid (kidname index)) ++
                             " " ++ label ++ " -> " ++ (kidname index) ++ ";\n"
       -- now let's run this thing!
       in shownode ++ (concat (map showkid pairs))
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
  let ptypestr t = case t of 
                     Initial  -> "initial" 
                     Auxiliar -> "auxiliary" 
                     _        -> ""
      --
      gtypestr n = case (gtype n) of 
                     Subs -> "type:subst"
                     Foot -> "type:foot"
                     Lex  -> if (ganchor n) then "type:anchor" else "type:lex" 
                     _    -> ""
      --
      nodestr :: GNode -> String
      nodestr n = "n" ++ gnname n 
                  ++ " " ++ gtypestr n ++ " "
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
      -- helpers to account for shortcomings in genihand parser 
      dashtobar :: Char -> Char
      dashtobar c = if ('-' == c) then '_' else c 
      substf (a,v) = case (a,v) of (_,"+") -> (a, "minus")
                                   (_,"-") -> (a, "minus")
                                   (_,"3") -> (a, "third")
                                   (_,"2") -> (a, "second")
                                   (_,"1") -> (a, "first")
                                   _       -> (a,v)
      showflist = showPairs . (map substf)
      --
  in ((map dashtobar).pidname) tr 
     ++ " (" ++ (concat $ intersperse " " (params tr)) ++ ")"
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

% ----------------------------------------------------------------------
\section{Printing sentences}
% ----------------------------------------------------------------------

\paragraph{showLeaves} returns the phrase that the TAG tree represents.
Any leaf nodes which are not words are returned as the node name.

\begin{code}
showLeaves :: TagElem -> String
showLeaves te = concat $ intersperse " " w
                where w = showLeaves' (ttree te) 
\end{code}

\begin{code}
showLeaves' :: Tree GNode -> [ String ]
showLeaves' (Node node []) = 
  let lexeme = (glexeme node)
      cat' = filter (\ (f,_) -> f == "cat") $ gup node 
      cat  = if (null cat') 
             then gnname node     
             else snd $ head cat'
      name   = map toUpper cat 
      output = if (null lexeme) then name else lexeme
  in [ output ]            
\end{code}
\begin{code}
showLeaves' (Node _ l) = concatMap showLeaves' l 
\end{code}

