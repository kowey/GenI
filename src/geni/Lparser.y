{
module Lparser
 
where 

import Data.List(sort)
import ParserLib(Token(..), 
                 PosToken, simpleParserError)
import Btypes(AvPair, emptyLE, ILexEntry(..), Ptype(..), Sem)

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
    '-'      {(MinusTok, _, _)}
    '+'      {(PlusTok, _, _)}
 
%%

{- -----------------------------------------------------------------
   syntactic lexicon 
   ----------------------------------------------------------------- -}


Lexicon :: { ([[AvPair]],[[AvPair]],[ILexEntry]) }
Lexicon : LInput                          { ([],[],$1) }
        | PredDrctv '!' PredDrctv LInput  { ($1,$3,$4) }

LInput :: { [ILexEntry] }
LInput : {-empty-}       {[]}
       | LexEntry LInput {$1:$2}

LexEntry :: { ILexEntry }
LexEntry: LexEntryCore                  { $1 }
        | LexEntryCore '[' FeatList ']' { $1 { ipfeat = sort $3 } }

LexEntryCore :: { ILexEntry }
LexEntryCore : id id id 
   { emptyLE { iword = $1, 
               icategory = $2,
               ifamname = $3 } }

{- precedence directives -}

PredDrctv :: { {-pred directives-} [[AvPair]] }
PredDrctv :  {-empty-}                         { [] }
          | '[' PredItems ']' PredDrctv  { $2 : $4 }

PredItems :: { [AvPair] }
PredItems :  {-empty-}         { [] }
          | PredItem PredItems { $1 ++ $2 }

PredItem :: { [AvPair] } 
PredItem: '(' id ':' IDList ')' { map (\a -> (a,$2)) $4 }


{- -----------------------------------------------------------------
   semantic lexicon 
   ----------------------------------------------------------------- -}

SLexicon :: { ([ILexEntry], [LpSemFam]) }
SLexicon : SInput                {($1, [])}
         | SInput '!' SemFamList {($1, $3)}

SInput :: { [ILexEntry] }
SInput : {-empty-}        {[]}
       | SLexEntry SInput {$1:$2}
 
SLexEntry :: { ILexEntry }
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

IDFeat :: { ([String],[AvPair])}
IDFeat : IDList               { ($1,[]) }
       | IDList '!' FeatList  { ($1,$3) }

{- semantics -}

Sem :: { Btypes.Sem }
Sem: {-empty-}                {[]}
   | sem ':' '[' ListPred ']' {createHandleVars $4}

ListPred :: { [LpRawPred] }
ListPred : {-empty-}         {[]}
         | Pred ListPred     {$1:$2}

Pred :: { LpRawPred }
Pred : id ':' id '(' Params ')'  {(($1,$3),  $5)}
     | id        '(' Params ')'  {(("", $1), $3)}

Params :: { [String] }
Params : {-empty-} {[]}
       | id Params {$1:$2}

{- semantic families -}

SemFamList :: { [LpSemFam] }
SemFamList: {-empty-}       { [] }
        | SemFam SemFamList { $1 : $2 }

SemFam :: { LpSemFam }
SemFam: Num ':' '(' NumList ')' '[' IDList ']'
      {($1:$4, $7)}

NumList :: { [Int] }
NumList: {-empty-}   {[]}
       | Num NumList {$1:$2}

Num :: { Int }
Num: num     { $1 }
   | '-' num { (-$2) }

{- -----------------------------------------------------------------
   generic stuff 
   ----------------------------------------------------------------- -}

IDList :: { [String] }
IDList : {-empty-}  {[]} 
       | id IDList  {$1:$2} 

{- feature structures -}

FeatList :: { {-FeatList-} [AvPair] }
FeatList : {-empty-}               {[]} 
         | id ':' FeatVal FeatList {($1,$3):$4}

FeatVal :: { String }
FeatVal: id  {$1}
       | num {show $1}
       | '+' {"+"}
       | '-' {"-"}



{

type LpRawPred   = ((String,String), [String])
type LpSemFam    = ([Int],[String])

createHandleVars :: [LpRawPred] -> Btypes.Sem 
createHandleVars predargs = 
  let handle newh oldh = if null oldh 
                         then "H" ++ show newh 
                         else oldh
      fn (nh,((oh,p),a)) = (handle nh oh,p,a)
      zpa = zip [1..] predargs
   in map fn zpa

happyError = simpleParserError
}
     
