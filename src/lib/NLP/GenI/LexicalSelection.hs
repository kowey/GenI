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

-- | This module performs the core of lexical selection and anchoring.
{-# LANGUAGE OverloadedStrings #-}
module NLP.GenI.LexicalSelection
where

import Control.Arrow ((***))
import Control.Monad.Maybe
import Control.Monad.Writer

import Data.Function ( on )
import Data.List
import Data.List.Split ( wordsBy )
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing)
import Data.Poset
import Data.Tree (Tree(Node))
import qualified Data.Text as T
import Data.Text ( Text )

import Data.FullList hiding ( head, tail, (++) )
import qualified Data.FullList as FL

import NLP.GenI.General(filterTree, repAllNode,
    showWithCount,
    geniBug,
    repNodeByNode,
    )
import NLP.GenI.Btypes
  (Macros, ILexEntry, Lexicon,
   replace,
   GNode(ganchor, gnname, gup, gdown, gaconstr, gtype, gorigin),
   GType(Subs, Other),
   isemantics, ifamname, iword, iparams, iequations,
   iinterface, ifilters,
   isempols,
   pidname, pfamily, pinterface, ptype, psemantics, ptrace,
   finaliseVars,
   )
import NLP.GenI.FeatureStructures (Flist, AvPair(..), unifyFeat)
import NLP.GenI.GeniVal( unify, GeniVal(gConstraints), isConst, Subst )

import NLP.GenI.Semantics ( subsumeSem, unifySem, Sem )
import NLP.GenI.Tags (TagElem(..),
             idname,
             )
import NLP.GenI.TreeSchemata ( Ttree(..), SchemaTree, SchemaNode, crushTreeGNode
                             , setAnchor, setLexeme, tree
                             )

-- ----------------------------------------------------------------------
-- Selecting candidate lemmas
-- ----------------------------------------------------------------------

-- | Select and returns the set of entries from the lexicon whose semantics
--   subsumes the input semantics.
chooseLexCand :: Lexicon -> Sem -> [ILexEntry]
chooseLexCand slex tsem = chooseCandI tsem slex

-- | 'chooseCandI' @sem l@ attempts to unify the semantics of @l@ with @sem@
--   If this succeeds, we use return the result(s); if it fails, we reject
--   @l@ as a lexical selection candidate.
chooseCandI :: Sem -> [ILexEntry] -> [ILexEntry]
chooseCandI tsem cand =
  let replaceLex i (sem,sub) =
        (replace sub i) { isemantics = sem }
      --
      helper :: ILexEntry -> [ILexEntry]
      helper l = if null sem then [l]
                 else map (replaceLex l) psubsem
        where psubsem = sem `subsumeSem` tsem
              sem = isemantics l
      --
  in nub $ concatMap helper cand

-- | `mergeSynonyms' is a factorisation technique that uses
--   atomic disjunction to merge all synonyms into a single lexical
--   entry.  Two lexical entries are considered synonyms if their
--   semantics match and they point to the same tree families.
-- 
--  FIXME: 2006-10-11 - note that this is no longer being used,
--  because it breaks the case where two lexical entries differ
--  only by their use of path equations.  Perhaps it's worthwhile
--  just to add a check that the path equations match exactly.
mergeSynonyms :: [ILexEntry] -> [ILexEntry]
mergeSynonyms lexEntry =
  let mergeFn l1 l2 = l1 { iword = (FL.++) (iword l1) (iword l2) }
      keyFn l = (ifamname l, isemantics l)
      synMap = foldr helper Map.empty lexEntry
        where helper x acc = Map.insertWith mergeFn (keyFn x) x acc
  in Map.elems synMap

-- --------------------------------------------------------------------
-- Anchoring
-- --------------------------------------------------------------------

-- | The 'LexCombine' supports warnings during lexical selection
--   and also failure via Maybe
type LexCombine a = MaybeT (Writer [LexCombineError]) a

lexTell :: LexCombineError -> LexCombine ()
lexTell x = lift (tell [x])

data LexCombineError =
       BoringError String
     | FamilyNotFoundError String
     | SchemaError [SchemaTree] LexCombineError2
 deriving Eq

data LexCombineError2 = EnrichError PathEqLhs
                      | StringError String
 deriving (Eq, Ord)

instance Poset LexCombineError where
 leq (BoringError _) _                                 = True
 leq (SchemaError _ e1) (SchemaError _ e2)             = leq e1 e2
 leq (FamilyNotFoundError x1) (FamilyNotFoundError x2) = leq x1 x2
 leq (FamilyNotFoundError _)  (SchemaError _ _)        = True
 leq _ _ = False

instance Poset LexCombineError2 where
 leq (EnrichError _)  (EnrichError _ ) = False
 leq (EnrichError _ ) (StringError _ ) = True
 leq (StringError s1) (StringError s2) = leq s1 s2
 leq _ _ = False


instance Show LexCombineError where
 show e = body ++ suffix
  where
   (body, suffix) = showLexCombineError e

showLexCombineError :: LexCombineError -> (String, String)
showLexCombineError (SchemaError xs x) = (show x, showWithCount (const "") "trees" ((),length xs))
showLexCombineError (BoringError s)    = (s, "")
showLexCombineError (FamilyNotFoundError f) = ("Family " ++ f ++ " not found in tree schema file", "")

instance Show LexCombineError2 where
 show (EnrichError p) = "Some trees discarded due enrichment error on " ++ showPathEqLhs p
 show (StringError s) = s

compressLexCombineErrors :: [LexCombineError] -> [LexCombineError]
compressLexCombineErrors errs = schema2 ++ normal
 where
  isSchema (SchemaError _ _) = True
  isSchema _ = False
  (schema, normal) = partition isSchema errs
  schema2 = map (uncurry (flip SchemaError))
          . Map.toList
          $ Map.fromListWith (++) [ (l,ts) | SchemaError ts l <- schema ]

-- | Given a lexical item, looks up the tree families for that item, and
--   anchor the item to the trees.
combineList :: Sem -> Macros -> ILexEntry
            -> ([LexCombineError],[TagElem]) -- ^ any warnings, plus the results
combineList tsem gram lexitem =
  case [ t | t <- gram, pfamily t == tn ] of
       []   -> ([FamilyNotFoundError tn],[])
       macs -> squish . swap . unzip $ map (\m -> runWriter . runMaybeT $ combineOne tsem lexitem m) macs
  where
   tn = ifamname lexitem
   swap (x,y) = (y,x)
   squish = (compressLexCombineErrors . concat) *** (concat . catMaybes)

-- | Combine a single tree with its lexical item to form a bonafide TagElem.
--   This process can fail, however, because of filtering or enrichement
combineOne :: Sem -> ILexEntry -> SchemaTree -> LexCombine [TagElem]
combineOne tsem lexRaw eRaw = -- Maybe monad
 -- trace ("\n" ++ (show wt)) $
 do let l1 = finaliseVars "-l" lexRaw
        e1 = finaliseVars "-t" eRaw
    (l,e) <- unifyParamsWithWarning (l1,e1)
             >>= unifyInterfaceUsing iinterface
             >>= unifyInterfaceUsing ifilters -- filtering
             >>= enrichWithWarning -- enrichment
    tree2 <- case crushTreeGNode (tree e) of
               Nothing -> do lexTell (SchemaError [e] (StringError "Could not flatten disjunction"))
                             fail ""
               Just x  -> return x
    let name = concat $ intersperse ":" $ filter (not.null)
                 [ FL.head (iword l) , pfamily e , pidname e ]
        template = TE
              { idname = name
              , ttreename = pfamily e
              , tidnum    = -1 -- provisional id
              , ttype = ptype e
              , ttree = setOrigin name . setLemAnchors . setAnchor (iword l) $ tree2
              , tsemantics  = []
              , tsempols    = isempols l
              , tpolarities = Map.empty
              , tinterface  = pinterface e
              , ttrace      = ptrace e
              }
    semUnifications <- case unifySem (isemantics l) (fromMaybe [] $ psemantics e) of
                         [] -> do lexTell (SchemaError [e] (StringError "could not unify lemma and schema semantics"))
                                  fail ""
                         xs -> return xs
    return $ concatMap (finaliseSemantics template) semUnifications
 where
  finaliseSemantics template (sem,sub) =
    do (sem2,sub2) <- sem `subsumeSem` replace sub tsem
       return $ replace sub2 $ template { tsemantics = sem2 }
  unifyParamsWithWarning (l,t) =
   -- trace ("unify params " ++ wt) $
   let lp = iparams l
       tp = params t
   in if length lp /= length tp
      then do lexTell (SchemaError [t] (StringError "Parameter length mismatch"))
              fail ""
      else case unify lp tp of
             Nothing -> do lexTell (SchemaError [t] (StringError "Parameter unification error"))
                           fail ""
             Just (ps2, subst) -> return (replace subst l, t2)
                                  where t2 = (replace subst t) { params = ps2 }
  unifyInterfaceUsing ifn (l,e) =
    -- trace ("unify interface" ++ wt) $
    case unifyFeat (ifn l) (pinterface e) of
    Nothing             -> do lexTell (SchemaError [e] (StringError "Interface unification error"))
                              fail ""
    Just (int2, fsubst) -> return (replace fsubst l, e2)
                           where e2 = (replace fsubst e) { pinterface = int2 }
  --
  enrichWithWarning (l,e) =
    -- trace ("enrich" ++ wt) $
    do e2 <- enrich l e
       return (l,e2)

-- Enrichment

data PathEqLhs = PeqInterface   Text
               | PeqJust        NodePathEqLhs
               | PeqUnknown     Text
  deriving (Eq, Ord)

data NodePathEqLhs = PeqNode String TopBottom Text
  deriving (Eq, Ord)

data TopBottom = Top | Bottom
  deriving (Eq, Ord)

type PathEqPair = (NodePathEqLhs, GeniVal)

enrich :: ILexEntry -> SchemaTree -> LexCombine SchemaTree
enrich l t =
 do -- separate into interface/anchor/named
    (intE, namedE) <- lift $ lexEquations l
    -- enrich the interface and everything else
    t2 <- foldM enrichInterface t intE
    -- enrich everything else
    foldM enrichBy t2 namedE
 where
  enrichInterface tx en =
    case unifyFeat [en] (pinterface tx) of
      Nothing -> lexTell (ifaceEnrichErr en) >> fail ""
      Just (i2, isubs) -> return $ (replace isubs tx) { pinterface = i2 }
  ifaceEnrichErr (AvPair loc _) = SchemaError [t] (EnrichError (PeqInterface loc))

-- | Helper for 'enrich' (enrich by single path equation)
enrichBy :: SchemaTree
         -> PathEqPair
         -> LexCombine SchemaTree
enrichBy t eq@(eqLhs, _) =
  case maybeEnrichBy t eq of
    Nothing -> lexTell enrichErr >> return t
    Just t2 -> return t2
 where
  enrichErr = SchemaError [t] (EnrichError (PeqJust eqLhs))

-- | Helper for 'enrichBy'
maybeEnrichBy :: SchemaTree
              -> PathEqPair
              -> Maybe SchemaTree
maybeEnrichBy t (eqLhs, eqVal) = do
  node      <- seekCoanchor eqLhs t
  (fs, sub) <- enrichFeat (AvPair eqAtt eqVal) (get node)
  return $ fixNode (set node fs) (replace sub t)
 where
  (get, set) = case eqTop of
                 Top     -> (gup,   \n x -> n { gup = x })
                 Bottom  -> (gdown, \n x -> n { gdown = x})
  (PeqNode _ eqTop eqAtt) = eqLhs
  fixNode n mt = mt { tree = repNodeByNode (matchNodeName eqLhs) n (tree mt) }

enrichFeat :: AvPair GeniVal -> Flist [GeniVal] -> Maybe (Flist [GeniVal], Subst)
enrichFeat (AvPair a v) fs =
  case span (\x -> avAtt x < a) fs of
    (before,here:after) | avMatch here ->
      do let (AvPair _ fv) = here
         (v2,sub) <- unify fv (replicate (length fv) v)
         let av2 = AvPair a v2
             fs2 = replace sub before ++ (av2 : replace sub after)
         return (fs2, sub)
    (before,after) ->
      let av2 = AvPair a [v]
          fs2 = before ++ (av2 : after) in  Just (fs2, Map.empty)
  where
   avMatch (AvPair fa _) = fa == a

missingCoanchors :: ILexEntry -> SchemaTree -> [String]
missingCoanchors lexEntry t =
   [ name eqLhs | eqLhs <- nubBy ((==) `on` name) equations, missing eqLhs ]
 where
   equations = map fst . snd . fst . runWriter $ lexEquations lexEntry
   name (PeqNode n _ _) = n
   missing eqLhs = isNothing (seekCoanchor eqLhs t)

-- | Split a lex entry's path equations into interface enrichement equations
--   or (co-)anchor modifiers
lexEquations :: ILexEntry -> Writer [LexCombineError] ([AvPair GeniVal],[PathEqPair])
lexEquations =
  fmap myPartition . mapM parseAv . iequations
  where
   myPartition xs = ( [ AvPair a v | (PeqInterface a, v) <- xs ]
                    , [ (n,v)      | (PeqJust n, v)      <- xs ] )
   parseAv (AvPair a v) = fmap (\a2 -> (a2,v)) (parsePathEq a)

seekCoanchor :: NodePathEqLhs -> SchemaTree -> Maybe SchemaNode
seekCoanchor eqLhs t =
 case filterTree (matchNodeName eqLhs) (tree t) of
 [a] -> Just a
 []  -> Nothing
 _   -> geniBug $ "Tree with multiple matches in enrichBy. " ++
                  "\nTree: " ++ pidname t ++ "\nFamily: " ++ pfamily t ++
                  "\nMatching on: " ++ showPathEqLhs (PeqJust eqLhs)

matchNodeName :: NodePathEqLhs -> SchemaNode -> Bool
matchNodeName (PeqNode "anchor" _ _) = ganchor
matchNodeName (PeqNode n _ _)        = (== n) . gnname

-- | Parse a path equation using the GenI conventions
--   This always succeeds, but can return @Just warning@
--   if anything anomalous comes up
--   FIXME : make more efficient
parsePathEq :: Text -> Writer [LexCombineError] PathEqLhs
parsePathEq e =
  case wordsBy (== '.') (T.unpack e) of
  (n:"top":r)     -> return (node n Top    r)
  (n:"bot":r)     -> return (node n Bottom r)
  ("top":r)       -> return (node "anchor" Top r)
  ("bot":r)       -> return (node "anchor" Bottom r)
  ("anchor":r)    -> return (node "anchor" Bottom r)
  ("interface":r) -> return (PeqInterface     (rejoin r))
  ("anc":r)       -> parsePathEq $ rejoin ("anchor":r)
  (n:r)           -> tell [ BoringError (tMsg n) ] >> return (node n Top r)
  _               -> tell [ BoringError iMsg     ] >> return (PeqUnknown e)
 where
  node n tb r = PeqJust $ PeqNode n tb (rejoin r)
  rejoin = T.pack . concat . intersperse "."
  tMsg n = "Interpreting path equation " ++ T.unpack e ++ " as applying to top of " ++ n ++ "."
  iMsg   = "Could not interpret path equation " ++ T.unpack e 

showPathEqLhs :: PathEqLhs -> String
showPathEqLhs p =
  case p of
   PeqJust (PeqNode n tb att) -> squish [ n       , fromTb tb, T.unpack att ]
   PeqInterface att -> squish [ "interface", T.unpack att ]
   PeqUnknown e     -> T.unpack e
 where
  fromTb Top    = "top"
  fromTb Bottom = "bot"
  squish = intercalate "."

-- Lemanchor mechanism

setLemAnchors :: Tree (GNode GeniVal) -> Tree (GNode GeniVal)
setLemAnchors t =
 repAllNode fn filt t
 where
  filt (Node a []) = gtype a == Subs && (isJust. lemAnchor) a
  filt _ = False
  fn (Node x k) = setLexeme (lemAnchorMaybeFake x) $
                    Node (x { gtype = Other, gaconstr = False }) k
  --
  lemAnchorMaybeFake :: GNode GeniVal -> [String]
  lemAnchorMaybeFake n =
    case lemAnchor n of
    Nothing -> ["ERR_UNSET_LEMMANCHOR"]
    Just l  -> map T.unpack l
  lemAnchor :: GNode GeniVal -> Maybe [Text]
  lemAnchor n =
    case [ v | AvPair a v <- gdown n, a == _lemanchor ] of
    [l] | isConst l -> fromFL `fmap` (gConstraints l)
    _               -> Nothing

_lemanchor :: Text
_lemanchor = "lemanchor"

-- Node origins

setOrigin :: String -> Tree (GNode v) -> Tree (GNode v)
setOrigin t = fmap (\g -> g { gorigin = t })

-- ----------------------------------------------------------------------
-- Helper functions
-- ----------------------------------------------------------------------

myEMPTY :: String
myEMPTY = "MYEMPTY"
