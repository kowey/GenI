\chapter{Morphology}

This module handles mostly everything to do with morphology in Geni.
There are two basic tasks: morphological input and output.  
GenI farms out morphology to whatever third party program you
specify in the configuration file.

\begin{code}
module Morphology where
\end{code}

\ignore{
\begin{code}
import Data.List (lines, unlines, unwords, nub)
import Data.Tree
import Data.FiniteMap
import System.IO

import Graphviz -- FIXME: temporary, just for the process stuff 
import Bfuncs
import Tags

import Debug.Trace
\end{code}
}

\begin{code}
type MorphFn = Pred -> Maybe Flist
\end{code}

\section{Input}

Morphological input means attaching morphological features on trees.  The
user specifies morphological input through the input semantics.  Our job
is to identify morphological predicates like \semexpr{plural(x)} and 
apply features like \fs{\it num:pl} on the relevant trees.

\paragraph{readMorph} converts information from a morphological
information file into GenI's internal format.

\begin{code}
readMorph :: [(String,[AvPair])] -> MorphFn
readMorph minfo pred = lookupFM fm key
  where fm = listToFM minfo
        key = snd3 pred 
\end{code}

\paragraph{attachMorph} does the bulk of the morphological input. We use
\fnparam{morphfn} to determine which literals in \fnparam{sem} contain
morphological information and what information they contain.  Then we
attach this morphological information to the relevant trees in
\fnparam{cand}.  A tree is considered relevant w.r.t to a morphological
literal if its semantics contains at least one literal whose first index
is the same as the first index of the morphological literal.

\begin{code}
attachMorph :: MorphFn -> Sem -> [TagElem] -> [TagElem]
attachMorph morphfn sem cands = 
  let -- relevance of a tree wrt to an index
      relTree i = not.null.relfilt.tsemantics
        where relfilt = filter (relLit i)  
      relLit i l = if null args then False else (head args == i)
        where args = thd3 l
      -- perform the attachment for a tree if it is relevant
      attachHelper :: String -> Flist -> TagElem -> TagElem  
      attachHelper i mfs t = 
        if relTree i t then attachMorphHelper mfs t else t 
      -- perform all attachments for a literal
      attach :: Pred -> [TagElem] -> [TagElem]
      attach l cs = 
        case morphfn l of 
          Nothing  -> cs
          Just mfs -> map (attachHelper i mfs) cs
        where i = if null args then "" else head args
              args = thd3 l 
  in foldr attach cands sem 
\end{code}

\paragraph{attachMorphHelper} performs the morphological attachment
proper.  

FIXME: we'll need to make sure this still works as promised 
       when we implement co-anchors.

\begin{code}
attachMorphHelper :: Flist -> TagElem -> TagElem
attachMorphHelper mfs te = 
  let -- unification with anchor
      tt     = ttree te 
      anchor = head $ filterTree fn tt
               where fn a = (ganchor a && gtype a == Lex)
      (succ, unf, subst) = unifyFeat mfs (gup anchor) 
      -- perform substTagElem
      te2 = substTagElem te subst
      tt2 = ttree te2
      -- replace the anchor with the unification results
      newgdown = substFlist (gdown anchor) subst
      te3 = te2 { ttree = setAnchor newa tt2 }
            where newa = anchor { gup   = unf,
                                  gdown = newgdown }
  in if succ 
     then te3
     else error ("Morphological unification failure on " ++ idname te)

setAnchor :: GNode -> Tree GNode -> Tree GNode
setAnchor n t =
  let filt (Node a _) = (gtype a == Lex && ganchor a)
      fn (Node _ l)   = Node n l
  in (head.fst) $ listRepNode fn filt [t]
\end{code}

\section{Output}

Output (\jargon{morphological generation}) refers to the actual process
of converting lemmas and morphological information into inflected forms.
We do this by calling some third party software specified by the user.

The morphological software must accept on stdin a newline delimited list
of lemmas and features like so:

\begin{verbatim}
the  [num:sg]
girl [num:sg]
hate [tense:past num:sg]
that [num:pl]
boy  [num:pl]
\end{verbatim}

It must return inflected forms on stdout, delimited by newlines

\begin{verbatim}
the
girl
hated
those
boys
\end{verbatim}

If your morphological software does not do this, you could wrap it
with a simple shell or Perl script.

\paragraph{sansMorph} extracts the lemmas from a list of uninflected
sentences.  This is used when the morphological generator is 
unavailable, doesn't work, etc.

\begin{code}
sansMorph :: [(String,Flist)] -> String
sansMorph s = (unwords . (map fst)) s
\end{code}

\paragraph{inflectSentences} converts a list of uninflected sentences
into inflected ones by calling the third party software.  Since we're
using system calls, we're stuck with an IO monad. 

Note: we try to avoid system calls by remembering the inflected forms.

\begin{code}
inflectSentences :: String -> [[(String,Flist)]] -> IO [String]
inflectSentences morphcmd sentences =  
  do -- determine what inflections to perform 
     let morphlst = (nub.concat) sentences
     -- perform the inflections
     inflections <- callInflector morphcmd morphlst 
     let morphmem = listToFM (zip morphlst inflections)
     -- apply the inflections to the sentences 
     let lookupfn w = lookupWithDefaultFM morphmem (fst w) w
         sentences2 = map (unwords . (map lookupfn)) sentences
     return sentences2
  `catch` \_ -> do putStrLn "Error calling morphological generator"
                   return $ map sansMorph sentences
\end{code}

\paragraph{callInflector} performs the Unix stuff which calls the
morphological generator

\begin{code}
callInflector :: String -> [(String,Flist)] -> IO [String]
callInflector morphcmd request = 
  do -- format the stuff as input to the inflector
     let fn (lem,fs) = lem ++ " [" ++ showPairs fs ++ "]"
         order = unlines $ map fn request
     -- run the inflector
     (pid, fromP, toP) <- runPiped morphcmd [] Nothing Nothing
     hPutStrLn toP order
     hClose toP 
     awaitProcess pid 
     -- read the inflector output back as a list of strings
     resRaw <- hGetContents fromP 
     let res = lines resRaw
     return res
\end{code}


