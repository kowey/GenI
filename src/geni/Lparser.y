{
module Lparser
 
where 

-- import Data.FiniteMap

import ParserLib(Token(..), 
                 PosToken, parserError)
import Btypes

createHandleVars predargs = 
  let handle newh oldh = if null oldh 
                         then "H" ++ show newh 
                         else oldh
      fn (nh,((oh,p),a)) = (handle nh oh,p,a)
      zpa = zip [1..] predargs
   in map fn zpa

emptyLE = ILE { iword = "",
                itreename = "", 
                iparams = [],
                ipfeat  = [],
                iptype = Unspecified,
                isemantics = [],
                ipredictors = []}
}

%name lexParser    LInput
%name semlexParser SInput
%tokentype { PosToken }

%token 
    ':'   {(Colon,        _, _)} 
    id    {(ID $$,        _, _)}  
    num   {(Num $$,       _, _)} 
    sem   {(Semantics,    _, _)}
    '('   {(OP,           _, _)} 
    ')'   {(CP,           _, _)} 
    '!'   {(Bang,         _, _)} 
    ']'   {(CB,           _, _)}
    '['   {(OB,           _, _)}
    '+'      {(PolPositive, _, _)}
    '-'      {(PolNegative, _, _)}
 
%%

LInput : 
     {[]}
 | LexEntry LInput
     {$1:$2}
        
LexEntry: id id 
   { emptyLE { iword = $1, itreename = $2 } }

SInput :
     {[]}
 | SDef SInput
     {$1:$2}

SDef :
    SLexEntry Sem 
     {$1{ isemantics = $2 }}
 |  SLexEntry '(' '!' PredictorList ')' Sem 
     {$1{ isemantics = $6,
          ipredictors = $4}}
 
SLexEntry: id '(' IDFeat ')'
         {emptyLE { iword   = $1,
                    iparams = fst $3,
                    ipfeat  = snd $3 }
         }
 
Sem: 
     {[]}
 | sem ':' '[' ListPred ']' 
     {sortSem (createHandleVars $4)}

ListPred :
     {[]}
 | Pred ListPred
     {$1:$2}

Pred :
    id ':' id '(' Params ')'  
     {(($1,$3),  $5)}
 |  id '(' Params ')'  
     {(("", $1), $3)}

Params :
     {[]}
 | id Params 
     {$1:$2}

IDFeat : 
   IDList 
     { ($1,[]) }
 | IDList '!' FeatList 
     { ($1,$3) }

IDList :
   id
     {[$1]} 
 | id IDList
     {$1:$2} 

FeatList : 
     {[]} 
 | id ':' id FeatList
     {($1,$3):$4}

PredictorList : 
      {[]}
  | PolVal Predictor PredictorList 
      {($2,$1):$3}
  | PolVal num Predictor PredictorList 
      {(map (const ($3,$1)) [1..($2)]) ++ ($4)}

Predictor: id 
    { ($1,"") }
  | id ':' id
    { ($1,$3) }
  
PolVal: '+' {(1)}
      | '-' {(-1)}

{
happyError = parserError
}
     
