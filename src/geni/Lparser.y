{
module Lparser
 
where 

-- import Data.FiniteMap

import Data.List(sort)
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
                isemantics = [],
                iprecedence = 0 }
}

%name lexParser    Lexicon 
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

Lexicon : LInput
     { ([],[],$1) }
 | PredDirectives '!' PredDirectives LInput
     { ($1,$3,$4) }

LInput : 
     {[]}
 | LexEntry LInput
     {$1:$2}
       
LexEntry: LexEntryCore
   { $1 }
 | LexEntryCore '[' FeatList ']'
   { $1 { ipfeat = sort $3 } }

LexEntryCore: id id id 
   { emptyLE { iword = $1, 
               icategory = $2,
               ifamname = $3 } }

PredDirectives:
        { [] }
  | '[' PredItems ']' PredDirectives
        { $2 : $4 }

PredItems:
        { [] }
  | PredItem PredItems
        { $1 ++ $2 }


PredItem: '(' id ':' IDList ')'
        { map (\a -> (a,$2)) $4 }








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
                    ipfeat  = sort $ snd $4,
                    isemantics = $6
                  }
         }
 | id id '[' FeatList ']' Sem
         {emptyLE { iword   = $1,
                    icategory = $2,
                    ipfeat  = sort $ $4,
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





IDList :
     {[]} 
 | id IDList
     {$1:$2} 


{
happyError = simpleParserError
}
     
