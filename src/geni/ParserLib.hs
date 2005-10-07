module ParserLib 
where

import Btypes (
               Ptype(Initial,Auxiliar,Unspecified),
               Ttree(..), Flist, AvPair,
               GType(Foot, Lex, Subs, Other),
               GNode(..), MTtree, 

               emptyLE, ILexEntry(..), Ptype(..), 

               Sem, Pred, readGeniVal, GeniVal(..))

import qualified Data.Map as Map 

import Data.Char (isLower)
import Data.List (sort)
import qualified Data.Tree 


import System.IO.Unsafe(unsafePerformIO)

type PosToken = (Token, Int, Int)

data Token =
    Grammar | Entry | Syntax | Features | Avm | Var | 
    Id | Sem | Label |
    Const | Predicate | Argument |
    Mark | Name | Negated | EqTok | Feature |
    Node | Literal | Lt | Gt | BarTok |
    Init | Aux | FamilyTok |  
    Anchor | Lexeme | Type | LSubst | LFoot | Aconstr | Noadj |
    Comma | Colon | Bang | 
    OC | CC | OP | CP | OB | CB | 
    Str String | ID String | Num Int |
    Semantics | RestrictorsTok |  SentenceTok |
    Polarities | Predictors |
    Begin | End |
    PlusTok | MinusTok 
  deriving (Eq, Show, Read)

-- -------------------------------------------------------------------
-- error recovery
-- the monad below is from the Happy manual
-- -------------------------------------------------------------------

data E a = Ok a | Failed String

thenE :: E a -> (a -> E b) -> E b
m `thenE` k = 
   case m of 
       Ok a -> k a
       Failed e -> Failed e

returnE :: a -> E a
returnE a = Ok a

failE :: String -> E a
failE err = Failed err

catchE :: E a -> (String -> E a) -> E a
catchE m k = 
   case m of
      Ok a -> Ok a
      Failed e -> k e

parserError :: [PosToken] -> E a
parserError [] = failE $ "Parser error: input ended unexpectantly: " ++ 
                        "maybe your parentheses do not match?"
parserError ((t, l, c):cs) = 
    let mess = if (l == 0) 
               then "Parse error in arguments near col. " ++ show c
               else "Parse error in input file near line " ++ show l 
                    ++ ", col. " ++ show c
    in failE mess

simpleParserError :: [PosToken] -> a
simpleParserError [] = error "Parser error because input file ended unexpectantly."
simpleParserError ((t, l, c):cs) = 
    let {
	mess = if (l == 0) 
	       then "Parse error in arguments near col. " ++ show c
               else "Parse error in input file near line " ++ 
				   show l ++ 
				   ", col. " ++
				   show c
    } in 
    error (mess)


-- -------------------------------------------------------------------
-- lexicon helper functions 
-- -------------------------------------------------------------------

type LpSemPols    = [Int]
type LpParam     = (GeniVal,Int)
type LpRawPred   = ((LpParam,String), [LpParam])
type LpSemFam    = ([Int],[String])

createHandleVars :: [LpRawPred] -> Sem 
createHandleVars predargs = 
  let fn ((_,p),a) = (GAnon, p, map fst a)
  in  map fn predargs 

{- this is if you want to deal with handles -- i don't
createHandleVars :: [LpRawPred] -> Btypes.Sem 
createHandleVars predargs = 
  let handle newh oldh = if null oldh 
                         then "H" ++ show newh 
                         else oldh
      fn (nh,((oh,p),a)) = (han, p, map fst a)
                           where han = handle nh (fst oh)
      zpa = zip [1..] predargs
   in map fn zpa
-}

extractSemPolarities :: [LpRawPred] -> [LpSemPols]
extractSemPolarities predargs =
  map fn predargs
  where fn ((h,p),a) = (snd h) : (map snd a)

-- -------------------------------------------------------------------
-- macro helper functions
-- -------------------------------------------------------------------
 
type TrTree   = Data.Tree.Tree
type MpStuff = (String,String,(Flist,Flist)) 
type MpPredictors = [(AvPair,Int)]
type MpPolarities = Map.Map String Int

emptyPolPred = (Map.empty, [])

nullpair :: ([a],[a])
nullpair = ([],[])

buildTreeNode :: String -> MpStuff -> [TrTree GNode] -> TrTree GNode
buildTreeNode name (ntype, lex, (rtop,rbot)) kids =
  let top = sort rtop 
      bot = sort rbot 
      (a,l,t,ac) = case ntype of 
         "anchor" ->  (True,  "" , Lex,   True)
         "lexeme" ->  (False, lex, Lex,   True)
         "subst"  ->  (False, "" , Subs,  True)
         "foot"   ->  (False, "" , Foot,  True)
         "aconstr" -> (False, "" , Other, True)
         "" ->        (False, "" , Other, False) 
      node = GN{gnname=name, gtype=t,
                gup=top,   gdown=bot,
                ganchor=a, glexeme=l,
                gaconstr=ac}
  in Data.Tree.Node node kids

buildTree :: Ptype -> (String,String) -> ([GeniVal],Flist) -> (MpPolarities, MpPredictors) -> TrTree GNode -> MTtree
buildTree ttype (fam,id) (params,feats) (pol,pred) t = 
       TT{params = params, 
          pfamily = fam,
          pidname = id,
          pfeat = feats, 
          ptype = ttype, 
          tree = t, 
          ptpolarities = pol,
          ptpredictors = pred} 

-- -------------------------------------------------------------------
-- test suite helpers 
-- -------------------------------------------------------------------

type TpCase = ( String,            -- name
                (TpSem, [AvPair]), -- sem/restrictors
                [String])          -- sentences 
type TpSem  = Btypes.Sem
-- type TpSem  = [Tree (String,String)]
-- type TpPred = (String,String)

-- -------------------------------------------------------------------
-- general helper functions 
-- -------------------------------------------------------------------

rejectNonGConst :: String -> String 
rejectNonGConst str =
  let h = head str
  in  if null str then error "empty string in rejectNonGConst"
      else if isLower h then str 
      else error "only constants are allowed in a disjunction!"
