{
module Tsparser
 
where 

import Data.Tree
import ParserLib (Token(Comma, Colon, ID, Semantics, OP, CP, CB, OB), PosToken)

import Btypes (sortSem)

-- the monad below is from the Happy manual

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
}

%name tsParser
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


Sem :
    '[' ListPred ']' 
     { $2 }
  | sem ':' '[' ListPred ']' 
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
happyError [] = failE $ "Parser error: input ended unexpectantly: " ++ 
                        "maybe your parentheses do not match?"
happyError ((t, l, c):cs) = 
    let {
        mess = if (l == 0) 
               then "Parse error in arguments near col. " ++ show c
               else "Parse error in input file near line " ++ 
                                   show l ++ 
                                   ", col. " ++
                                   show c
    } in 
    failE mess
}
     
