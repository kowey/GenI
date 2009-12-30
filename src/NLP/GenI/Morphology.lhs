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

\chapter{Morphology}
\label{cha:Morphology}

This module handles mostly everything to do with morphology in Geni.
There are two basic tasks: morphological input and output.
GenI farms out morphology to whatever third party program you
specify on the command line.  Note that a simple and stupid
``sillymorph'' realiser is provided either in the GenI repository
or on hackage.

\begin{code}
module NLP.GenI.Morphology
 (
 MorphFn
 -- re-export
 , LemmaPlus(..), LemmaPlusSentence
 -- * Morphological predicates
 , readMorph, stripMorphSem, attachMorph, setMorphAnchor
 -- * Morphological realisation
 , inflectSentencesUsingCmd, sansMorph
 ) where
\end{code}

\ignore{
\begin{code}
import Data.Maybe (isNothing)
import Data.Tree
import qualified Data.Map as Map
import System.IO
import System.Process
import Text.JSON
import Text.JSON.Pretty

import NLP.GenI.Btypes
import NLP.GenI.General
import NLP.GenI.Tags
import NLP.GenI.Builder
\end{code}
}

\begin{code}
type MorphFn = Pred -> Maybe Flist
\end{code}

\section{Morphological input}

Morphological input means attaching morphological features on trees.  The
user specifies morphological input through the input semantics.  Our job
is to identify morphological predicates like \semexpr{plural(x)} and 
apply features like \fs{\it num:pl} on the relevant trees.

\begin{code}
-- | Converts information from a morphological information file into GenI's
--   internal format.
readMorph :: [(String,[AvPair])] -> MorphFn
readMorph minfo pred_ = Map.lookup key fm
  where fm = Map.fromList minfo
        key = show $ snd3 pred_

-- | Filters away from an input semantics any literals whose realisation is
--   strictly morphological.  The first argument tells us helps identify the
--   morphological literals -- it associates literals with morphological stuff;
--   if it returns 'Nothing', then it is non-morphological
stripMorphSem :: MorphFn -> Sem -> Sem
stripMorphSem morphfn tsem = 
  [ l | l <- tsem, (isNothing.morphfn) l ]

-- | 'attachMorph' @morphfn sem cands@ does the bulk of the morphological
--   input processing.  We use @morphfn@ to determine which literals in
--   @sem@ contain morphological information and what information they contain.
--   Then we attach this morphological information to the relevant trees in
--   @cand@.  A tree is considered relevant w.r.t to a morphological
--   literal if its semantics contains at least one literal whose first index
--   is the same as the first index of the morphological literal.
attachMorph :: MorphFn -> Sem -> [TagElem] -> [TagElem]
attachMorph morphfn sem cands = 
  let -- relevance of a tree wrt to an index
      relTree i = not.null.relfilt.tsemantics
        where relfilt = filter (relLit i)  
      relLit i l = if null args then False else (head args == i)
        where args = thd3 l
      -- perform the attachment for a tree if it is relevant
      attachHelper :: GeniVal -> Flist -> TagElem -> TagElem  
      attachHelper i mfs t = 
        if relTree i t then attachMorphHelper mfs t else t 
      -- perform all attachments for a literal
      attach :: Pred -> [TagElem] -> [TagElem]
      attach l cs = 
        case morphfn l of 
          Nothing  -> cs
          Just mfs -> map (attachHelper i mfs) cs
        where i = if null args then GAnon else head args
              args = thd3 l 
  in foldr attach cands sem 

-- | Actually unify the morphological features into the anchor node
--
--   FIXME: we'll need to make sure this still works as promised 
--   when we implement co-anchors.
attachMorphHelper :: Flist -> TagElem -> TagElem
attachMorphHelper mfs te = 
  let -- unification with anchor
      tt     = ttree te 
      anchor = head $ filterTree fn tt
               where fn a = (ganchor a && gtype a == Lex)
  in case unifyFeat mfs (gup anchor) of
     Nothing -> error ("Morphological unification failure on " ++ idname te)
     Just (unf,subst) ->
      let -- perform replacements
          te2 = replace subst te 
          tt2 = ttree te2
          -- replace the anchor with the unification results
          newgdown = replace subst (gdown anchor) 
          newa = anchor { gup = unf, gdown = newgdown }
      in te2 { ttree = setMorphAnchor newa tt2 }

setMorphAnchor :: GNode -> Tree GNode -> Tree GNode
setMorphAnchor n t =
  let filt (Node a _) = (gtype a == Lex && ganchor a)
      fn (Node _ l)   = Node n l
  in (head.fst) $ listRepNode fn filt [t]
\end{code}

\section{Morphological realisation}

\jargon{Morphological realisation} refers to the actual process
of converting lemmas and morphological information into inflected forms.
We do this by calling some third party software specified by the user.

The morphological software must accept a JSON list of \jargon{lemma sentences}
where each lemma sentence is itself a list of objects containing a lemma and
a feature structure.

\begin{verbatim}
[
 [{"lemma": "le",       "lemma-features": "[num:sg gen:f]"},
  {"lemma": "fille",    "lemma-features": "[num:sg]"},
  {"lemma": "detester", "lemma-features": "[num:sg tense:past]"},
  {"lemma": "le",       "lemma-features": "[num:pl gen:m]"},
  {"lemma": "garcon",   "lemma-features": "[num:pl]"}
 ],

 [{"lemma": "ce",       "lemma-features": "[]"},
  {"lemma": "etre",     "lemma-features": "[]"},
  {"lemma": "le",       "lemma-features": "[]"},
  {"lemma": "garcon",   "lemma-features": "[]"},
  {"lemma": "que",      "lemma-features": "[]"},
  {"lemma": "le",       "lemma-features": "[num:sg gen:f]"},
  {"lemma": "fille",    "lemma-features": "[num:sg]"},
  {"lemma": "detester", "lemma-features": "[num:sg tense:past]"}
 ]
]
\end{verbatim}

NB: I recommend using a JSON library instead of parsing and writing this by
hand.

The morphological realiser may return more than one output per sentence.
Indeed, we expect a JSON-formatted list (a) of lists (b), where each (b)
provides a number of candidate morphological realisations for a sentence in
(a).  The list (a) must have the same length as the input because each item in
(a) is expected to correspond to a sentence from the input.

Notice that the morphological generator can choose to delete spaces or do other
orthographical tricks in between words:

\begin{verbatim}
[
 ["la fille detestait les garcons"],

 ["c'est le garcon que la fille detestait"
 ,"c'est les garcons que la fille detestait"]
]
\end{verbatim}

If your morphological software does not do this, you could wrap it with a
simple script.

\begin{code}
-- | Extracts the lemmas from a list of uninflected sentences.  This is used
--   when the morphological generator is unavailable, doesn't work, etc.
sansMorph :: LemmaPlusSentence -> [String]
sansMorph = singleton . unwords . map lem
 where
  lem (LemmaPlus l _) = l

-- | Converts a list of uninflected sentences into inflected ones by calling
---  the third party software.
-- FIXME: this doesn't actually support lists-of-results per input
-- will need to work it out
inflectSentencesUsingCmd :: String -> [LemmaPlusSentence] -> IO [(LemmaPlusSentence,[String])]
inflectSentencesUsingCmd morphcmd sentences =
  do -- run the inflector
     (toP, fromP, errP, _) <- runInteractiveCommand morphcmd
     hPutStrLn toP . render . pp_value . showJSON $ sentences
     hClose toP
     -- read the inflector output back as a list of strings
     mResults <- (resultToEither . decode) `fmap` hGetContents fromP
     hGetContents errP >>= ePutStrLn
     case mResults of
       Left err  -> fallback $ "Could not parse morphological generator output: " ++ err
       Right res -> do let lenResults   = length res
                           lenSentences = length sentences
                       if lenResults == lenSentences
                          then return $ zip sentences res
                          else fallback $ "Morphological generator returned "
                                          ++ show lenResults ++ " results for "
                                          ++ show lenSentences ++ " inputs"
                    `catch` \e -> fallback $ "Error calling morphological generator:\n" ++ show e
 where
  fallback err =
    do ePutStrLn err
       return $ map (\x -> (x, sansMorph x)) sentences

singleton :: a -> [a]
singleton x = [x]
\end{code}
