{
module Tsparser
 
where 

import Data.Tree
import ParserLib hiding (Node)
}

%name targetSemParser SemR
%name testSuiteParser TestSuite

%tokentype { PosToken }
%monad { E } { thenE } { returnE }

%token 
    ':'   {(Colon,        _, _)} 
    id    {(ID $$,        _, _)}  
    sem   {(Semantics,    _, _)}
    res   {(RestrictorsTok, _,_)}
    '('   {(OP,           _, _)} 
    ')'   {(CP,           _, _)} 
    ']'   {(CB,           _, _)}
    '['   {(OB,           _, _)}
    '+'   {(PlusTok,      _, _)}
    '-'   {(MinusTok,     _, _)}
    num   {(Num $$,       _, _)}
%%

TestSuite :: { [TpCase] }
TestSuite: TestCase           { [$1] } 
         | TestCase TestSuite { $1 : $2 } 

TestCase :: { TpCase }
TestCase:    SemR Sentences { ("",$1,$2) }
        | id SemR Sentences { ($1,$2,$3) }

SemR :: { (TpSem, [AvPair]) }
SemR: Sem     { ($1,[]) }
    | Sem Res { ($1,$2) }

{- sentences -}

Sentences :: { [String] }
Sentences:                    { [] }
         | Sentence Sentences { $1 : $2 }

Sentence :: { String }
Sentence: '[' String ']' { $2 }

String :: { String }
String: id { $1 }
      | id String { $1 ++ " " ++ $2 }

{- semantics -}

Sem :: { TpSem }
Sem : sem ':' '[' ListPred ']' { $4 }

ListPred :: { TpSem } 
ListPred :               {[]}
         | Pred ListPred {$1:$2}

Pred :: { Tree TpPred }
Pred : id ':' id '(' Params ')'  {(Node ($1,$3) $5)}
     | id '(' Params ')'         {(Node ("",$1) $3)}

Params :: { TpSem } 
Params :             {[]}
       | id Params   {(Node ("",$1) []):$2}
       | Pred Params {$1:$2}

{- restrictors -}

Res :: { [AvPair] }
Res : res ':' '[' FeatList ']' { $4 }

FeatList :: { {-FeatList-} [AvPair] }
FeatList : {-empty-}               {[]} 
         | id ':' FeatVal FeatList {($1,$3):$4}

FeatVal :: { String }
FeatVal: id  {$1}
       | num {show $1}
       | '+' {"+"}
       | '-' {"-"}

{
type AvPair = (String,String)

type TpCase = ( String, (TpSem, [AvPair]), [String] )
type TpSem  = [Tree (String,String)]
type TpPred = (String,String)

happyError :: [PosToken] -> E a
happyError = parserError
}

