{
module Tsparser
 
where 

import Data.Tree
import ParserLib (Token(Comma, Colon, ID, Semantics, OP, CP, CB, OB), 
                  PosToken,
                  E(..), thenE, returnE, failE, parserError)

import Btypes (sortSem)


}

%name targetSemParser Sem
%name testSuiteParser TestSuite

%tokentype { PosToken }
%monad { E } { thenE } { returnE }

%token 
    ':'   {(Colon,        _, _)} 
    id    {(ID $$,        _, _)}  
    sem   {(Semantics,    _, _)}
    '('   {(OP,           _, _)} 
    ')'   {(CP,           _, _)} 
    ']'   {(CB,           _, _)}
    '['   {(OB,           _, _)}
%%

TestSuite: TestCase 
           { [$1] } 
          | TestCase TestSuite
           { $1 : $2 } 
        
TestCase: Sem Sentences
        { ($1,$2) }

Sentences: Sentence 
         { [$1] }
         | Sentence Sentences
         { $1 : $2 }

Sentence: '[' String ']'
      { $2 }

String: id { $1 }
      | id String { $1 ++ " " ++ $2 }

Sem : sem ':' '[' ListPred ']' 
     { $4 }

ListPred :
     {[]}
 | Pred ListPred
     {$1:$2}

Pred :
     id ':' id '(' Params ')'  
     {(Node ($1,$3) $5)}
   | id '(' Params ')'  
     {(Node ("",$1) $3)}

Params :
     {[]}
 | id Params 
     {(Node ("",$1) []):$2}
 | Pred Params
     {$1:$2}

{
happyError :: [PosToken] -> E a
happyError = parserError
}

