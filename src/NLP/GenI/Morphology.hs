-- GenI surface realiser
-- Copyright (C) 2005 Carlos Areces and Eric Kow
--
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation; either version 2
-- of the License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

{-|
This module handles mostly everything to do with morphology in Geni.
There are two basic tasks: morphological input and output.
GenI farms out morphology to whatever third party program you
specify on the command line.  Note that a simple and stupid
``sillymorph'' realiser is provided either in the GenI repository
or on hackage.
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
module NLP.GenI.Morphology
 (
 module NLP.GenI.Morphology.Types
 -- * Morphological predicates
 , readMorph, stripMorphSem, attachMorph, setMorphAnchor
 -- * Morphological realisation
 , inflectSentencesUsingCmd, sansMorph
 ) where

import Control.Concurrent (forkIO)
import Control.Exception (catch, bracket, evaluate, IOException)
import Data.Maybe (isNothing, fromMaybe)
import Data.Text ( Text )
import Data.Tree
import Data.Typeable
import System.Exit
import System.IO
import System.Process
import Prelude hiding (catch)
import qualified Data.Map as Map
import qualified Data.Text as T

import System.Log.Logger
import Text.JSON
import Text.JSON.Pretty hiding ( (<>), (<+>) )

import NLP.GenI.FeatureStructure
import NLP.GenI.GeniVal ( mkGAnon, GeniVal, replace )
import NLP.GenI.General
import NLP.GenI.Morphology.Types
import NLP.GenI.Pretty
import NLP.GenI.Semantics ( Literal(..), Sem )
import NLP.GenI.Tag
import NLP.GenI.TreeSchema ( GNode(..), GType(..) )

-- ----------------------------------------------------------------------
-- Morphological input
-- ----------------------------------------------------------------------

-- | Converts information from a morphological information file into GenI's
--   internal format.
readMorph :: [(Text,[AvPair GeniVal])] -> MorphInputFn
readMorph minfo lit =
    Map.lookup key fm
  where
    fm = Map.fromList minfo
    key = pretty (lPredicate lit)

-- | Filters away from an input semantics any literals whose realisation is
--   strictly morphological.  The first argument tells us helps identify the
--   morphological literals -- it associates literals with morphological stuff;
--   if it returns 'Nothing', then it is non-morphological
stripMorphSem :: MorphInputFn -> Sem -> Sem
stripMorphSem morphfn tsem = 
  [ l | l <- tsem, (isNothing.morphfn) l ]

-- | 'attachMorph' @morphfn sem cands@ does the bulk of the morphological
--   input processing.  We use @morphfn@ to determine which literals in
--   @sem@ contain morphological information and what information they contain.
--   Then we attach this morphological information to the relevant trees in
--   @cand@.  A tree is considered relevant w.r.t to a morphological
--   literal if its semantics contains at least one literal whose first index
--   is the same as the first index of the morphological literal.
attachMorph :: MorphInputFn -> Sem -> [TagElem] -> [TagElem]
attachMorph morphfn sem cands = 
  let -- relevance of a tree wrt to an index
      relTree i = not.null.relfilt.tsemantics
        where relfilt = filter (relLit i)  
      relLit i l = case lArgs l of
                     []    -> False
                     (x:_) -> x == i
      -- perform the attachment for a tree if it is relevant
      attachHelper :: GeniVal -> Flist GeniVal -> TagElem -> TagElem
      attachHelper i mfs t = 
        if relTree i t then attachMorphHelper mfs t else t 
      -- perform all attachments for a literal
      attach :: Literal GeniVal -> [TagElem] -> [TagElem]
      attach l cs = 
        case morphfn l of 
          Nothing  -> cs
          Just mfs -> map (attachHelper i mfs) cs
        where i = case lArgs l of
                    []    -> mkGAnon
                    (x:_) -> x
  in foldr attach cands sem 

-- | Actually unify the morphological features into the anchor node
--
--   FIXME: we'll need to make sure this still works as promised 
--   when we implement co-anchors.
attachMorphHelper :: Flist GeniVal -> TagElem -> TagElem
attachMorphHelper mfs te = 
  let -- unification with anchor
      tt     = ttree te 
      anchor = head $ filterTree fn tt
               where fn a = (ganchor a && gtype a == Lex)
  in case unifyFeat mfs (gup anchor) of
     Left err -> error . T.unpack $
       "Morphological unification failure on" <+> idname te <> ":" <+> err
     Right (unf,subst) ->
      let -- perform replacements
          te2 = replace subst te 
          tt2 = ttree te2
          -- replace the anchor with the unification results
          newgdown = replace subst (gdown anchor) 
          newa = anchor { gup = unf, gdown = newgdown }
      in te2 { ttree = setMorphAnchor newa tt2 }

-- | @setMorphAnchor n t@ replaces the anchor node of a tree with @n@
--
--   We assume the tree has exactly one anchor node.  If it has none,
--   this explodes; if it has more than one, they all get replaced.
setMorphAnchor :: GNode GeniVal -> Tree (GNode GeniVal) -> Tree (GNode GeniVal)
setMorphAnchor n t =
    fromMaybe (error oops) $ repNode fn filt t
  where
    filt (Node a _) = gtype a == Lex && ganchor a
    fn (Node _ l)   = Node n l
    oops = "NLP.GenI.Morphology.setMorphAnchor did not anticipate failure was possible"

-- ----------------------------------------------------------------------
-- Morphological realisation
-- ----------------------------------------------------------------------

-- | Extracts the lemmas from a list of uninflected sentences.  This is used
--   when the morphological generator is unavailable, doesn't work, etc.
sansMorph :: LemmaPlusSentence -> MorphOutput
sansMorph =
    MorphOutput [] . singleton . T.unwords . map lem
  where
    lem (LemmaPlus l _) = l

-- | Converts a list of uninflected sentences into inflected ones by calling
---  the third party software.
-- FIXME: this doesn't actually support lists-of-results per input
-- will need to work it out
-- HUH? What makes me say that?
inflectSentencesUsingCmd :: String -> [LemmaPlusSentence] -> IO [(LemmaPlusSentence,MorphOutput)]
inflectSentencesUsingCmd morphcmd sentences =
  doit `catch` \e -> let _ = e :: IOException in (fallback (show e))
 where
  hCloseSloppy h = hClose h `catch` \err -> let _ = err :: IOException in warningM logname (show err)
  doit = bracket
   (do debugM logname $ "Starting morph generator: " ++ morphcmd 
       runInteractiveCommand morphcmd)
   (\(inh,outh,errh,_) -> do
      debugM logname $ "Closing output handles from morph generator"
      mapM hCloseSloppy [inh, outh, errh])
    $ \(toP,fromP,errP,pid) -> do
     debugM logname $ "Sending " ++ show (length sentences) ++ " sentences to morph generator"
     hPutStrLn toP . render . pp_value . showJSON $ sentences

     debugM logname $ "Closing input handle to morph generator"
     hClose toP
     -- see http://www.haskell.org/pipermail/haskell-cafe/2008-May/042994.html
     -- fork off a thread to pull on the stderr
     -- so if the process writes to stderr we do not block.
     -- NB. do the hGetContents synchronously, otherwise the outer
     -- bracket can exit before this thread has run, and hGetContents
     -- will fail.
     err <- hGetContents errP
     _ <- forkIO (evaluate (length err) >> warningM logname err)

     -- wait for all the output
     output <- hGetContents fromP
     _ <- evaluate (length output)

     -- wait for the program to terminate
     exitcode <- waitForProcess pid
     debugM logname $ "Morph command exited"
     -- on failure, throw the exit code as an exception
     if exitcode == ExitSuccess
        then case resultToEither (decode output) of
               Left jerr  -> fallback $ "Could not parse morphological generator output: " ++ jerr
               Right res -> do let lenResults   = length res
                                   lenSentences = length sentences
                               if lenResults == lenSentences
                                  then return $ zip sentences res
                                  else fallback $ "Morphological generator returned "
                                                  ++ show lenResults ++ " results for "
                                                  ++ show lenSentences ++ " inputs"
                            `catch` \e -> let _ = e :: IOException
                                          in  fallback ("Error calling morphological generator:\n" ++ show e)
        else fallback "Morph generator failed"
  fallback err = do
    errorM logname err
    return $ map (\x -> (x, sansMorph x)) sentences

-- ----------------------------------------------------------------------
-- odds and ends
-- ----------------------------------------------------------------------

singleton :: a -> [a]
singleton x = [x]

data MNAME = MNAME deriving Typeable
logname :: String
logname = mkLogname MNAME
