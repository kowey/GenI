{
module GIparser
 
where 

import Btypes (ILexEntry)
import ParserLib(Token(..),PosToken,parserError)
import Data.List (intersperse)

untok (a,_,_) = a
}
%name giParser
%tokentype { PosToken }

%token 
    macros      {(Macros,      _, _)} 
    lexicon     {(Lexicon,     _, _)}  
    semlex      {(SemLexicon,  _, _)}
    gramtype    {(GrammarType,     _, _)}  
    TAGML       {(TAGMLTok, _, _)}
    GeniHand    {(GeniHandTok, _, _)}
    id           {(ID $$,       _, _)}
    '='        {(Eq,          _, _)} 
 %%

Input : InputList
     {$1}

InputList :
     {[]}
 | idkey '=' id InputList
     {(untok $1,$3):$4}
 | gramtype '=' GramTypes InputList
     {(untok $1, $3):$4}
 
idkey:   lexicon  {$1}  
       | macros   {$1}  
       | semlex   {$1}

GramTypes : TAGML    { (show.untok) $1 } 
          | GeniHand { (show.untok) $1 } 

{
happyError = parserError
}
