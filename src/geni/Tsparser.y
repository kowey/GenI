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

TestSuite: TestCase { [$1] } 
         | TestCase TestSuite { $1 : $2 } 
        
TestCase: SemR Sentences { ($1,$2) }

Sentences: Sentence           { [$1] }
         | Sentence Sentences { $1 : $2 }

Sentence: '[' String ']' { $2 }

String: id { $1 }
      | id String { $1 ++ " " ++ $2 }

SemR: Sem     { ($1,[]) }
    | Sem Res { ($1,$2) }

Sem : sem ':' '[' ListPred ']' { $4 }
Res : res ':' '[' FeatList ']' { $4 }

ListPred : {[]}
         | Pred ListPred {$1:$2}

Pred : id ':' id '(' Params ')'  {(Node ($1,$3) $5)}
     | id '(' Params ')'         {(Node ("",$1) $3)}

Params :             {[]}
       | id Params   {(Node ("",$1) []):$2}
       | Pred Params {$1:$2}

FeatList : {[]} 
         | id ':' FeatVal FeatList {($1,$3):$4}

FeatVal: id  {$1}
       | num {show $1}
       | '+' {"+"}
       | '-' {"-"}






{
happyError :: [PosToken] -> E a
happyError = parserError
}

