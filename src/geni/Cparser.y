{
module Cparser
 
where 

import Btypes (ILexEntry)
import ParserLib(Token(..),PosToken,simpleParserError)
import Data.List (intersperse)

untok (a,_,_) = a
}

%name cParser Input
%name giParser GramInput
%tokentype { PosToken }

%token 
    macros      {(Macros,      _, _)} 
    lexicon     {(Lexicon,     _, _)}  
    semlex      {(SemLexicon,  _, _)}
    grammar     {(GrammarTok,      _, _)} 
    tsem        {(TSemantics,  _, _)}
    tsuite      {(TestSuiteTok,  _, _)}
    graphical   {(Graphical,   _, _)}
    optimisations {(Optimisations, _,_)}
    polarised    {(Polarised,   _, _)}
    autopol      {(AutoPol,     _,_)}
    polsig       {(PolSig,      _,_)}
    predicting   {(Predicting,  _, _)}
    semfiltered  {(SemFiltered,  _, _)}
    chartsharing {(ChartSharing,  _, _)}
    orderedadj   {(OrderedAdj,  _, _)}
    footconstr   {(FootConstraint, _, _)}
    batch        {(Batch,  _, _)}
    repeat       {(Repeat,  _, _)}
    extrapol     {(ExtraPolarities,  _, _)}
    id           {(ID $$,       _, _)}
    true       {(TTT,         _, _)}
    false      {(FFF,         _, _)}
    '='        {(Eq,          _, _)} 
    num        {(Num $$,    _, _)} 
    '!'        {(Bang, _, _)}
    '+'        {(PolPositive, _, _)}
    '-'        {(PolNegative, _, _)}
    ','        {(Comma, _, _)}
 %%

Input : InputList
     {if (null $1) then [] else [$1]}
 | InputList '!' Input
     {($1:$3)}

InputList :
     {[]}
 | repeat '=' num InputList
     {(Repeat,show $3):$4}
 | idkey '=' id InputList
     {(untok $1,$3):$4}
 | boolkey '=' true InputList
     {($1,"True"):$4}
 | boolkey '=' false InputList
     {($1,"False"):$4}
 | optimisations '=' OptList InputList
     {(untok $1, $3):$4}
 | extrapol '=' PolList InputList
     {(untok $1, $3):$4}
 
idkey:   grammar  {$1}  
       | tsem     {$1}  
       | tsuite   {$1}


boolkey: graphical  {Graphical}

optkey: polarised    {Polarised}
      | autopol      {AutoPol}
      | polsig       {PolSig}
      | predicting   {Predicting}
      | semfiltered  {SemFiltered}
      | chartsharing {ChartSharing}
      | orderedadj   {OrderedAdj}
      | footconstr   {FootConstraint}

OptList : 
    batch  
      { show Batch }
  | OptListI
      { concat (intersperse " " $1) }

OptListI : 
      { [] }
 | optkey 
      { [(show $1)] }
 | optkey ',' OptListI
      { (show $1) : $3 }
  
PolList :
      {""}
 | Charge id PolList
      {$1 ++ $2 ++ $3}

Charge: PolVal {$1}
      | PolVal num {$1 ++ (show $2)}

PolVal: '+' {"+"}
      | '-' {"-"}


GramInput : GramInputList
     {$1}

GramInputList :
     {[]}
 | gramIdkey '=' id GramInputList
     {(untok $1,$3):$4}
 
gramIdkey:   lexicon  {$1}  
       | macros   {$1}  
       | semlex   {$1}



{
happyError = simpleParserError
}

