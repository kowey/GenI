{
module Mparser
 
where 

import ParserLib(Token(..),PosToken,parserError,
                 E(..), thenE, returnE, failE)

import Btypes (Ptype(Initial,Auxiliar,Unspecified),
               Ttree(..), Flist, AvPair,
               GType(Foot, Lex, Subs, Other),
               GNode(..), MTtree)

import Data.FiniteMap (FiniteMap, 
                       addToFM,
                       addToFM_C,
                       emptyFM,
                       plusFM)

import Data.List (sort)
import qualified Data.Tree 

}

%name mParser    Input 
%name polParserE PolList

%tokentype { PosToken }
%monad { E } { thenE } { returnE }

%token 
    ':'      {(Colon,    _, _)} 
    anchor   {(Anchor,   _, _)} 
    type     {(Type,     _, _)}
    subst    {(LSubst,   _, _)}
    lexeme   {(Lexeme ,  _, _)}
    foot     {(LFoot,    _, _)}
    aconstr  {(Aconstr,  _, _)}
    noadj    {(Noadj,    _, _)}
    str      {(Str $$,   _, _)} 
    initial  {(Init,     _, _)} 
    auxiliar {(Aux,      _, _)} 
    family   {(FamilyTok,_, _)}
    begin    {(Begin,    _, _)}
    end      {(End,      _, _)}
    id       {(ID $$,    _, _)} 
    num      {(Num $$,    _, _)} 
    '['      {(OB,       _, _)} 
    ']'      {(CB,       _, _)} 
    '{'      {(OC,       _, _)} 
    '}'      {(CC,       _, _)} 
    '('      {(OP,       _, _)} 
    ')'      {(CP,       _, _)} 
    '!'      {(Bang,     _, _)} 
    '+'      {(PlusTok, _, _)}
    '-'      {(MinusTok, _, _)}
 
%%

{- -----------------------------------------------------------------
   lists of trees 
   ----------------------------------------------------------------- -}

Input :: { [MTtree] }
Input : 
      { [] }
 | DefU Input
     {%
       let tn = pidname $1 
       in failE ("tree " ++ tn ++ " is of unspecified type") }
 | DefI Input
     { $1 : $2 }
 | DefA Input
     { $1 : $2 }
 | begin initial DefIUs end initial Input
     { (map (\e -> e {ptype = Initial}) $3) ++ $6 }
 | begin auxiliar DefAUs end auxiliar Input 
     { (map (\e -> e {ptype = Auxiliar}) $3) ++ $6 }

DefAUs :: { [MTtree] }
DefAUs : 
      { [] }
 | DefI DefAUs
      {% 
        let tn = pidname $1
        in failE ("tree " ++ tn ++ " is marked initial " 
                  ++ "within a block of auxiliary trees")
      }
 | DefA DefAUs 
      { $1 : $2 } 
 | DefU DefAUs 
      { $1 : $2 } 
 
DefIUs :: { [MTtree] }
DefIUs : 
      { [] }
 | DefA DefIUs
      {% 
        let tn = pidname $1
        in failE ("tree " ++ tn ++ " is marked auxiliary " 
                  ++ "within a block of initial trees")
      }
 | DefI DefIUs 
      { $1 : $2 } 
 | DefU DefIUs 
      { $1 : $2 } 

{- -----------------------------------------------------------------
   trees 
   ----------------------------------------------------------------- -}


{- definitions -}
 
DefU :: { MTtree }
DefU :
   TreeFamName '(' IDFeat ')' TopTree
     { buildTree Unspecified $1 $3 emptyPolPred $5 }  
 | TreeFamName '(' IDFeat ')' '(' PolPred ')' TopTree
     { buildTree Unspecified $1 $3 $6           $8 }

DefI :: { MTtree }
DefI :
   TreeFamName '(' IDFeat ')' initial TopTree
     { buildTree Initial $1 $3 emptyPolPred $6 }
 | TreeFamName '(' IDFeat ')' '(' PolPred ')' initial TopTree
     { buildTree Initial $1 $3 $6           $9 }

DefA :: { MTtree }
DefA :
   TreeFamName '(' IDFeat ')' auxiliar TopTree
     { buildTree Auxiliar $1 $3 emptyPolPred $6 }
 | TreeFamName '(' IDFeat ')' '(' PolPred ')' auxiliar TopTree
     { buildTree Auxiliar $1 $3 $6           $9 }

{- tree family / name -}

TreeFamName :: { (String,String) }
TreeFamName : id         {($1,"")}
            | id ':' id  {($1,$3)}
            | num        {(show $1,"")}
            | num ':' id {(show $1,$3)}


{- parameters, features -}

IDFeat :: { ([String],[AvPair])}
IDFeat : IDList               { ($1,[]) }
       | IDList '!' FeatList  { ($1,$3) }

{- polarities and predictors -}

PolPred :: { (MpPolarities, MpPredictors) }
PolPred : PolList                   { ($1,[]) }
        | PolList '!' PredictorList { ($1,$3) }

PolList :: { MpPolarities }
PolList :                    {emptyFM}
        | Charge id PolList {addToFM_C (+) $3 $2 $1}

PredictorList :: { MpPredictors }
PredictorList : 
      {[]}
  | PolVal Predictor PredictorList 
      {($2,$1):$3}
  | PolVal num Predictor PredictorList 
      {(map (const ($3,$1)) [1..$2]) ++ $4}

Predictor :: { AvPair }
Predictor: id        { ($1,"") }
         | id ':' id { ($1,$3) }

Charge :: { Int }
Charge: PolVal {($1)}
      | PolVal num {($1 * $2)}

PolVal :: { Int }
PolVal: '+' {(1)}
      | '-' {(-1)}

{- the trees themselves -}

TopTree :: { TrTree GNode }
TopTree : id Descrip '{' ListTree '}'
     { buildTreeNode $1 $2 $4 } 

ListTree :: { [TrTree GNode] }
ListTree :                     {[]}
         | id Descrip ListTree {(buildTreeNode $1 $2 []) : $3 }
         | TopTree ListTree    {($1:$2)}

Descrip :: { MpStuff }
Descrip :
   anchor
     {("anchor","",nullpair)}
 | type ':' anchor  TopBotF
     {("anchor","",$4)} 
 | type ':' lexeme str TopBotF
     {("lexeme",$4,$5)}
 | type ':' lexeme str 
     {("lexeme",$4,nullpair)}
 | type ':' subst TopBotF 
     {("subst","",$4)}
 | type ':' foot TopBotF 
     {("foot","",$4)} 
 | aconstr ':' noadj TopBotF 
     {("aconstr","",$4)} 
 | TopBotF 
     {("","",$1)} 

TopBotF :: { (Flist,Flist) }
TopBotF : '[' FeatList ']' '!' '[' FeatList ']'
    {($2,$6)}
 
{- -----------------------------------------------------------------
   generic stuff 
   ----------------------------------------------------------------- -}

IDList :: { [String] }
IDList : {-empty-}  {[]} 
       | id IDList  {$1:$2} 

{- feature structures -}

FeatList :: { {-FeatList-} [AvPair] }
FeatList : {-empty-}               {[]} 
         | id  ':' FeatVal FeatList {($1,$3):$4}
           {- let's not be shocked by "lex" -}
         | lexeme ':' FeatVal FeatList {("lex",$3):$4} 

FeatVal :: { String }
FeatVal: id  {$1}
       | num {show $1}
       | '+' {"+"}
       | '-' {"-"}


{

type TrTree   = Data.Tree.Tree
type MpStuff = (String,String,(Flist,Flist)) 
type MpPredictors = [(AvPair,Int)]
type MpPolarities = FiniteMap String Int

emptyPolPred = (emptyFM, [])

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

buildTree :: Ptype -> (String,String) -> ([String],Flist) -> (MpPolarities, MpPredictors) -> TrTree GNode -> MTtree
buildTree ttype (fam,id) (params,feats) (pol,pred) t = 
       TT{params = params, 
          pfamily = fam,
          pidname = id,
          pfeat = feats, 
          ptype = ttype, 
          tree = t, 
          ptpolarities = pol,
          ptpredictors = pred} 

polParser x = case polParserE x of 
                Ok p     -> p
                Failed e -> error e

happyError :: [PosToken] -> E a
happyError = parserError
}
