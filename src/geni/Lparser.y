{
module Lparser
 
where 

-- import Data.FiniteMap

import ParserLib(Token(..), 
                 PosToken, simpleParserError)
import Btypes(ILexEntry(..), Ptype(..))

createHandleVars predargs = 
  let handle newh oldh = if null oldh 
                         then "H" ++ show newh 
                         else oldh
      fn (nh,((oh,p),a)) = (handle nh oh,p,a)
      zpa = zip [1..] predargs
   in map fn zpa

emptyLE = ILE { iword = "",
                icategory = "",
                ifamname = "", 
                iparams = [],
                ipfeat  = [],
                iptype = Unspecified,
                isemantics = [] }
}

%name lexParser    LInput
%name semlexParser SLexicon
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
    '-'      {(PolNegative, _, _)}
 
%%

LInput : 
     {[]}
 | LexEntry LInput
     {$1:$2}
        
LexEntry: id id id 
   { emptyLE { iword = $1, 
               icategory = $2,
               ifamname = $3 } }

SLexicon : SInput 
     {($1, [])}
 | SInput '!' SemFamList 
     {($1, $3)}

SInput :
     {[]}
 | SLexEntry SInput
     {$1:$2}
 

SLexEntry: id id '(' IDFeat ')' Sem
         {emptyLE { iword   = $1,
                    icategory = $2,
                    iparams = fst $4,
                    ipfeat  = snd $4,
                    isemantics = $6
                  }
         }
 
Sem: 
     {[]}
 | sem ':' '[' ListPred ']' 
     {createHandleVars $4}

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

SemFamList:
          { [] }
        | SemFam SemFamList
          { $1 : $2 }

SemFam: Num ':' '(' NumList ')' '[' IDList ']'
      {($1:$4, $7)}
    
NumList:
        {[]}
 | Num NumList
        {$1:$2}

Num: num 
    { $1 }
   | '-' num
    { (-$2) }

{
happyError = simpleParserError
}
     
