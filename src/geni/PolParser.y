{
module PolParser 
 
where 

import ParserLib (Token(..),PosToken,parserError)
import FiniteMap
}
  
%name polParser
%tokentype { PosToken }

%token 
    id       {(ID $$,    _, _)} 
    num      {(Num $$,    _, _)} 
    '+'      {(PolPositive, _, _)}
    '-'      {(PolNegative, _, _)}
 %%


PolList :
      {emptyFM}
 | Charge id PolList
      {addToFM_C (+) $3 $2 $1}

Charge: PolVal {($1)}
      | PolVal num {($1 * $2)}

PolVal: '+' {(1)}
      | '-' {(-1)}

{
happyError = parserError
}
