{
module Tsparser
 
where 

--import Data.Tree
import ParserLib hiding (Node)
import Btypes(AvPair, emptyLE, ILexEntry(..), Ptype(..), Sem, Pred,
              readGeniVal, GeniVal(..))
import System.IO.Unsafe(unsafePerformIO)
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

Sem :: { Btypes.Sem }
Sem: sem ':' '[' ListPred ']' {$4}

ListPred :: { [Btypes.Pred] }
ListPred : {-empty-}         {[]}
         | Pred ListPred     {$1:$2}

Pred :: { Btypes.Pred }
Pred : id '(' Params ')'  {("", $1, $3)}
     | id ':' id '(' Params ')' {
        unsafePerformIO $ do  
          putStrLn "Warning: handle detected and ignored (we don't do handles)"
          return ("",$3,$5)
      }

Params :: { [String] }
Params : {-empty-} {[]}
       | id Params {$1:$2}

{- tree representation for semantics: 
   for now, i don't want to deal with this, or handles
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
-}

{- restrictors -}

Res :: { [AvPair] }
Res : res ':' '[' FeatList ']' { $4 }

FeatList :: { {-FeatList-} [AvPair] }
FeatList : {-empty-}               {[]} 
         | id ':' FeatVal FeatList {($1,$3):$4}

FeatVal :: { String }
FeatVal: id  {readGeniVal $1}
       | num {GConst (show $1)}
       | '+' {GConst "+"}
       | '-' {GConst "-"}

{
type TpCase = ( String, (TpSem, [AvPair]), [String] )
type TpSem  = Btypes.Sem
-- type TpSem  = [Tree (String,String)]
-- type TpPred = (String,String)

happyError :: [PosToken] -> E a
happyError = parserError
}

