{
module Mparser
 
where 

import ParserLib(Token(..),PosToken,parserError)

import Btypes (Ptype(Initial,Auxiliar,Unspecified),
               Ttree(..),
               GType(Foot, Lex, Subs, Other),
               GNode(..))

import FiniteMap (FiniteMap, 
                  addToFM,
                  addToFM_C,
                  emptyFM,
                  plusFM)

import List (sort)
import qualified Data.Tree 

emptyPolPred = (emptyFM, [])

buildTree ttype id (params,feats) (pol,pred) t = 
  (id, TT{params = params, 
          pfeat = feats, 
          ptype = ttype, 
          tree = t, 
          ptpolarities = pol,
          ptpredictors = pred}) 

}
  
%name mParser
%tokentype { PosToken }

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
    '+'      {(PolPositive, _, _)}
    '-'      {(PolNegative, _, _)}
 
%%

Input : 
     {(emptyFM, [])}
 | DefI Input
     {let {(c,u) = $2 ;
           (k,e) = $1}
      in (addToFM c k e, u)}
 | DefA Input
     {let {(c,u) = $2 ;
           (k,e) = $1 }
      in (addToFM c k e, u)}
 | Def Input
     {let (c,u) = $2 in (c, $1:u)}
 | begin initial Input end initial Input
     {let {(c ,u ) = $6;
           (c',u') = $3; 
           cc' = plusFM c c'} 
      in (foldr (\(k,e) -> 
                 \fm -> (addToFM fm k e{ptype = Initial})) cc' u', u)}
 | begin auxiliar Input end auxiliar Input 
     {let {(c, u ) = $6;
           (c',u') = $3;
           cc' = plusFM c c' }
      in (foldr (\(k,e) ->
                 \fm -> (addToFM fm k e{ptype = Auxiliar})) cc' u',u)}

Def :
   id '(' IDFeat ')' TopTree
     { buildTree Unspecified $1 $3 emptyPolPred $5 }  
 | id '(' IDFeat ')' '(' PolPred ')' TopTree
     { buildTree Unspecified $1 $3 $6           $8 }

DefI :
   id '(' IDFeat ')' initial TopTree
     { buildTree Initial $1 $3 emptyPolPred $6 }
 | id '(' IDFeat ')' '(' PolPred ')' initial TopTree
     { buildTree Initial $1 $3 $6           $9 }

DefA :
   id '(' IDFeat ')' auxiliar TopTree
     { buildTree Auxiliar $1 $3 emptyPolPred $6 }
 | id '(' IDFeat ')' '(' PolPred ')' auxiliar TopTree
     { buildTree Auxiliar $1 $3 $6           $9 }

IDFeat : 
   IDList 
     { ($1,[]) }
 | IDList '!' FeatList 
     { ($1,$3) }

PolPred : 
   PolList 
      { ($1,[]) }
 | PolList '!' PredictorList 
      { ($1,$3) }

PolList :
      {emptyFM}
 | Charge id PolList
      {addToFM_C (+) $3 $2 $1}

PredictorList : 
      {[]}
  | PolVal Predictor PredictorList 
      {($2,$1):$3}
  | PolVal num Predictor PredictorList 
      {(map (const ($3,$1)) [1..$2]) ++ $4}

Predictor: id 
    { ($1,"") }
  | id ':' id
    { ($1,$3) }
 
Charge: PolVal {($1)}
      | PolVal num {($1 * $2)}

PolVal: '+' {(1)}
      | '-' {(-1)}

IDList :
   id
     {[$1]} 
 | id IDList
     {$1:$2} 

TopTree : 
   id Descrip '{' ListTree '}'
     {let {
           lt = $4;
           (x1,x2,x3,x4) = $2;
           sortedx3 = sort x3;
           sortedx4 = sort x4;
           (a,l,t,ac) = 
               case x1 of {
                   "anchor" ->  (True,  "", Lex,   True);
                   "lexeme" ->  (False, x2, Lex,   True);
                   "subst"  ->  (False, "", Subs,  True);
                   "foot"   ->  (False, "", Foot,  True);
                   "aconstr" -> (False, "", Other, True);
                   "" ->        (False, "", Other, False) };
           tr = Data.Tree.Node GN{gnname=$1, 
                     gup=sortedx3, 
                     gdown=sortedx4, 
                     ganchor=a,
                     glexeme=l,
                     gtype=t,
                     gaconstr=ac
                    } lt;
          } in tr}

ListTree :
     {[]}
 | id Descrip ListTree
     {let {
           lt = $3;
           (x1,x2,x3,x4) = $2;
           sortedx3 = sort x3;
           sortedx4 = sort x4;
           (a,l,t,ac) =     -- Anchor, Lexeme, Type, AdjConstr
               case x1 of {
                   "anchor" ->  (True,  "", Lex,   True);
                   "lexeme" ->  (False, x2, Lex,   True);
                   "subst"  ->  (False, "", Subs,  True);
                   "foot"   ->  (False, "", Foot,  True);
                   "aconstr" -> (False, "", Other, True);
                   "" ->        (False, "", Other, False) };
           ltr = (Data.Tree.Node GN{gnname=$1, 
                      gup=sortedx3, 
                      gdown=sortedx4, 
                      ganchor=a,
                      glexeme=l,
                      gtype=t,
                      gaconstr=ac
                 } []):lt;
          } in ltr}
 | TopTree ListTree
     {($1:$2)}


Descrip :
   anchor 
     {("anchor","",[],[])} 
 | type ':' lexeme str 
     {("lexeme",$4,[],[])}
 | type ':' subst '[' FeatList ']' '!' '[' FeatList ']' 
     {("subst","",$5,$9)} 
 | type ':' foot '[' FeatList ']' '!' '[' FeatList ']' 
     {("foot","",$5,$9)} 
 | aconstr ':' noadj '[' FeatList ']' '!' '[' FeatList ']' 
     {("aconstr","",$5,$9)} 
 | '[' FeatList ']' '!' '[' FeatList ']' 
     {("","",$2,$6)} 
 
FeatList : 
     {[]} 
 | id ':' id FeatList
     {($1,$3):$4}


{
happyError = parserError
}
