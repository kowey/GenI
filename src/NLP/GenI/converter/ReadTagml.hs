{-
GenI surface realiser
Copyright (C) 2006 Carlos Areces and Eric Kow

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
-}

module NLP.GenI.Converter.ReadTagml where

import Data.Char
import Data.List (sort,delete,find)
import Data.Tree
import Control.Monad.State (State, runState, get, put)
import Text.XML.HaXml.XmlContent


import NLP.GenI.Btypes
  ( AvPair, Flist
  , GType(Subs,Foot,Lex,Other)
  , GNode(..), Macros, Ttree(..)
  , GeniVal(GConst, GVar, GAnon), fromGConst, lexemeAttributes
  , emptyMacro, Ptype(..), Pred, Sem)
-- import NLP.GenI.Tags(emptyTE,TagElem(..),Tags,TagSite,addToTags)
import qualified NLP.GenI.Converter.XmgTagml as X 

-- ======================================================================
-- Macros
-- ======================================================================

type MTree = Ttree GNode

readTagmlMacros :: String -> Either String Macros
readTagmlMacros g = 
 case readXml g of
 Right (X.GrammarEntry es) -> Right $ map translateEntry es
 _ -> Left "Not a TAGML grammar entry"

translateEntry :: X.Entry -> MTree
translateEntry (X.Entry (X.Entry_Attrs tname) (X.Family family) (X.Trace trace) t sem (X.Interface iface)) =
 let ifeats = case iface of
              Nothing  -> []
              Just ifs -> translateFlatFs ifs
     fromClass (X.Class s) = s
 in (translateTree t) { pfamily = family
                      , pidname = tname
                      , params  = []
                      , pinterface = ifeats
                      , ptrace  = map fromClass trace
                      , psemantics = Just $ translateSemantics sem }

-- ----------------------------------------------------------------------
-- Syntax
-- ----------------------------------------------------------------------

data TreeInfo = TI {
  tiNum     :: Int,
  tiHasFoot :: Bool
} 

-- Tree parsing consists of building up the tree with the recursive
-- function translateNode, and then extracting (from the State monad)
-- some information which is global to the tree.
translateTree :: X.Tree -> MTree
translateTree (X.Tree _ root) =
  let initTi = TI { tiNum = 0, tiHasFoot = False }
      (tr,info) = runState (translateNode root) initTi
  in emptyMacro { tree = tr
                , ptype = if tiHasFoot info then Auxiliar else Initial
                }

-- We recurse through a basic tree structure to build the TAG tree.
-- The annoying thing is the features.  The MG builds recursive feature
-- structures, where the top and bottom features are substructures of
-- the global fs.  GenI, on the other hand, assumes two flat feature
-- lists, top and bottom.   We work around this by simply assuming that
-- the fs recursion never goes further than that one level, and that
-- global features belong to both the top and bottom lists.
translateNode :: X.Node -> State TreeInfo (Tree GNode)
translateNode node@(X.Node _ _ kids) = do
  st <- get
  -- update the monadic state
  let idnum = tiNum st
      gn    = translateNodeHelper idnum node
  put $ st { tiNum = idnum + 1,
             -- tiLex = if ntype == Lex then lex else (tiLex st),
             tiHasFoot = tiHasFoot st || gtype gn == Foot }
  -- recursion to the kids  
  kidsOut <- mapM translateNode kids
  -- output the node
  return $! Node gn kidsOut

translateNodeHelper :: Int -> X.Node -> GNode
translateNodeHelper idnum (X.Node nattrs mnargs _) = gn where
      allFs = case mnargs of
              Nothing         -> []
              Just (X.Narg n) -> translateFs n
      --
      recursiveNamed x (Node (Left n) _) = n == x
      recursiveNamed _ _ = False
      topF_ = find (recursiveNamed "top") allFs
      botF_ = find (recursiveNamed "bot") allFs
      gloF_ = case topF_ of
              Nothing -> gloF_'
              Just t  -> delete t gloF_'
       where gloF_' = case botF_ of
                      Nothing -> allFs
                      Just b  -> delete b allFs
      --
      mflattenFs mfs = case mfs of Nothing -> []
                                   Just fs -> flattenFs fs
      topF = mflattenFs topF_
      botF = mflattenFs botF_
      gloF = concatMap flattenFs gloF_
      -- reading the node type
      ntype = case X.nodeType nattrs of
                 X.Node_type_subst    -> Subs
                 X.Node_type_foot     -> Foot
                 X.Node_type_anchor   -> Lex
                 X.Node_type_coanchor -> Lex
                 X.Node_type_lex      -> Lex
                 X.Node_type_std      -> Other
                 X.Node_type_nadj     -> Other
      -- figuring out what the lexeme might be
      -- (we a list of distinguished attributes, in order of preference)
      lexL = concat $ map (\la -> [ v | (a,v) <- gloF, a == la ])
                      lexemeAttributes
      lexeme = case (ntype,lexL) of
               (Lex,h:_) -> fromGConst h
               _         -> []
      anchor = X.nodeType nattrs == X.Node_type_anchor
      aconstr = case X.nodeType nattrs of
                X.Node_type_subst -> True
                X.Node_type_foot  -> True
                X.Node_type_nadj  -> True
                _                 -> False
      -- the node name is either the counter or something hardset
      name = case X.nodeName nattrs of
             Nothing     -> 'n' : (show idnum)
             Just n      -> n
      -- saving the results in a Gnode
      gn = GN { gnname  = name,
                gup     = sort $ topF ++ gloF,
                gdown   = sort $ botF ++ gloF,
                ganchor = anchor,
                glexeme = if anchor then [] else lexeme,
                gtype   = ntype,
                gaconstr = aconstr,
                gorigin  = "" }

-- ----------------------------------------------------------------------
-- Semantics
-- ----------------------------------------------------------------------

translateSemantics :: X.Semantics -> Sem
translateSemantics (X.Semantics ls) = map translateSemantics_ ls

translateSemantics_ :: X.Semantics_ -> Pred
translateSemantics_ (X.Semantics_Literal l) = translateLiteral l
translateSemantics_ _ = geniXmlError "fancy semantics not supported"

translateLiteral :: X.Literal -> Pred
translateLiteral (X.Literal _ mlabel (X.Predicate pr) args) =
 let label = case mlabel of
             Nothing          -> GAnon
             Just (X.Label s) -> translateSym s
 in (label, translateSym pr, map translateArg args)

translateArg :: X.Arg -> GeniVal
translateArg (X.ArgSym s) = translateSym s
translateArg (X.ArgFs _) = geniXmlError "complex semantic arguments not supported"

-- ----------------------------------------------------------------------
-- Features
-- ----------------------------------------------------------------------

type FsTreeNode = Either String AvPair

translateFs :: X.Fs -> Forest FsTreeNode 
translateFs (X.Fs _ fs) = map translateF fs

translateF :: X.F -> Tree FsTreeNode
translateF (X.FFs attr fs)  = Node (Left (X.fName attr)) (translateFs fs)
translateF (X.FSym attr s)  = Node (Right (X.fName attr, translateSym s))  []
translateF (X.FVAlt attr d) = Node (Right (X.fName attr, translateVAlt d)) []

flattenFs :: Tree FsTreeNode -> [AvPair]
flattenFs = catMaybes . (map fromRight) . flatten
 where fromRight (Right r) = Just r
       fromRight _         = Nothing

translateFlatFs :: X.Fs -> Flist
translateFlatFs (X.Fs _ fs) = map translateFlatF fs

translateFlatF :: X.F -> AvPair
translateFlatF (X.FFs _ _)      = error "translateFlatF called on recursive F"
translateFlatF (X.FSym attr s)  = (X.fName attr, translateSym s)
translateFlatF (X.FVAlt attr d) = (X.fName attr, translateVAlt d)

translateSym :: X.Sym -> GeniVal
translateSym s = 
 case X.symValue s of 
 Just c  -> GConst [c]
 Nothing -> 
   case X.symVarname s of
   Just v  -> GVar $ drop 1 v -- drop the '@'
   Nothing -> geniXmlError "translateSym on sym which is neither value nor varname"

-- atomic disjunction
translateVAlt :: X.VAlt -> GeniVal
translateVAlt (X.VAlt _ (NonEmpty ss)) = GConst $ map (fromJ.(X.symValue)) ss 
  where fromJ (Just n) = n
        fromJ _ = geniXmlError "atomic disjunction with non atomic component"

geniXmlError :: String -> a
geniXmlError s = error $ "GenI XML error: " ++ s
