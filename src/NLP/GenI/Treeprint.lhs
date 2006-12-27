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
module NLP.GenI.Treeprint
where
\end{code}

\ignore{
\begin{code}
import Data.Tree
import Data.List(intersperse,nub)
import qualified Data.Map

import NLP.GenI.General (mapTree)
import NLP.GenI.Tags
 ( TagElem(TE), idname,
   tsemantics, ttree, tinterface, ttype, ttreename,
 )
import NLP.GenI.Btypes (GeniVal(GConst, GVar, GAnon), AvPair, Ptype(..),
               Ttree(TT, params, pidname, pfamily, pinterface, ptype, tree, psemantics, ptrace),
               GNode(..), GType(..), Flist,
               isConst,
               Pred, showSem)

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
    in stub ++ (maybeShow_ " " extra)

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
  gvEdge (gvDerivationLab parent) (gvDerivationLab child) "" p
  where p = if substadj == 'a' then [("style","dashed")] else []
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
class GeniShow a where
  geniShow :: a -> String

instance GeniShow Ptype where
 geniShow Initial  = "initial"
 geniShow Auxiliar = "auxiliary"
 geniShow _        = ""

instance GeniShow AvPair where
 geniShow (a,v) = a ++ ":" ++ geniShow v

instance GeniShow GeniVal where
 geniShow (GConst xs) = concat $ intersperse "|" xs
 geniShow x = show  x

instance GeniShow Pred where
 geniShow (h, p, l) = (geniShow h) ++ ":" ++ (geniShow p) ++ "(" ++ unwords (map geniShow l) ++ ")"

instance GeniShow GNode where
 geniShow x =
  let gtypestr n = case (gtype n) of
                     Subs -> "type:subst"
                     Foot -> "type:foot"
                     Lex  -> if ganchor n && (null.glexeme) n
                             then "type:anchor" else "type:lex"
                     _    -> ""
      glexstr n =
        if null ls then ""
        else concat $ intersperse "|" $ map quote ls
        where quote s = "\"" ++ s ++ "\""
              ls = glexeme n
      tbFeats n = (geniShow $ gup n) ++ "!" ++ (geniShow $ gdown n)
  in unwords $ filter (not.null) $ [ gnname x, gtypestr x, glexstr x, tbFeats x ]

instance (GeniShow a) => GeniShow [a] where
 geniShow = squares . unwords . (map geniShow)

instance (GeniShow a) => GeniShow (Tree a) where
 geniShow t =
  let treestr i (Node a l) =
        spaces i ++ geniShow a ++
        case (l,i) of
        ([], 0)  -> "{}"
        ([], _)  -> ""
        (_, _)   -> "{\n" ++ (unlines $ map next l) ++ spaces i ++ "}"
        where next = treestr (i+1)
      --
      spaces i = take i $ repeat ' '
  in treestr 0 t

instance GeniShow TagElem where
 geniShow te =
  "\n% ------------------------- " ++ idname te
  ++ "\n" ++ (ttreename te) ++ ":" ++ (idname te)
  ++ " "  ++ (geniShow.tinterface $ te)
  ++ " "  ++ (geniShow.ttype $ te)
  ++ "\n" ++ (geniShow.ttree $ te)
  ++ "\n" ++ "semantics:" ++ (geniShow.tsemantics $ te)

instance (GeniShow a) => GeniShow (Ttree a) where
 geniShow tt =
  "\n% ------------------------- " ++ pidname tt
  ++ "\n" ++ (pfamily tt) ++ ":" ++ (pidname tt)
  ++ " "  ++ (parens $    (unwords $ map geniShow $ params tt)
                       ++ " ! "
                       ++ (unwords $ map geniShow $ pinterface tt))
  ++ " "  ++ (geniShow.ptype $ tt)
  ++ "\n" ++ (geniShow.tree $ tt)
  ++ (case psemantics tt of
      Nothing   -> ""
      Just psem -> "\n" ++ "semantics:" ++ (geniShow psem))
  ++ "\ntrace:" ++ (squares $ unwords $ ptrace tt)

parens, squares :: String -> String
parens s  = "(" ++ s ++ ")"
squares s = "[" ++ s ++ "]"
\end{code}

\include{src/NLP/GenI/HsShowable.lhs}
