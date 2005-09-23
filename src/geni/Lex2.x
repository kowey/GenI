{
module Lex2 (
  lexer,
  --startcodes
  scMac,
  --scConfig, scLex, scIndex, scTSem, scTSuite,
  --scPol, scMorph, scFil

) where

import ParserLib 
}

%wrapper "posn"

$digit = 0-9			-- digits
$alpha = [a-zA-Z\xc0-\xd6\xe9]		-- alphabetic characters

tokens :-

  $white+				;
  \%.*				        ;
  \".*\"                      { strTok }

  restrictors                { tok RestrictorsTok }
  semantics                  { tok Semantics  }
  polarities                 { tok Polarities }
  predictors                 { tok Predictors }
  
  \+                         { tok PlusTok }
  \-                         { tok MinusTok }
  \=                         { tok EqTok }
  
  initial                    { tok Init }
  auxiliary                  { tok Aux }
  anchor                     { tok Anchor }
  family                     { tok FamilyTok }
  lex                        { tok Lexeme }
  <scMac> type                       { tok Type }
  subst                      { tok LSubst }
  foot                       { tok LFoot }
  aconstr                    { tok Aconstr }
  noadj                      { tok Noadj }
  begin                      { tok Begin }
  end                        { tok End }
  \,                         { tok Comma }
  :                          { tok Colon }
  !                          { tok Bang }
  \{                         { tok OC }
  \}                         { tok CC }
  \(                         { tok OP }
  \)                         { tok CP }
  \[                         { tok OB }
  \]                         { tok CB }
  \|                         { tok BarTok }
 
  [$digit]+                           { numTok }
  [$alpha \_][$alpha $digit \- \_ \/ \. ]*   { idTok }
   

{
lexer :: Int -> String -> [(Token, Int, Int)]
lexer startcode str = go (alexStartPos,'\n',str)
  where go inp@(pos,_,str) =
          case alexScan inp startcode of
                AlexEOF -> []
                AlexError _ -> error "lexical error"
                AlexSkip  inp' len     -> go inp'
                AlexToken inp' len act -> act pos (take len str) : go inp'

-- Token builder:
tok  t p s = tok' t p             -- discards the s (this is sugar)
tok' t (AlexPn _ l c) = (t, l, c) -- used when you need to access the s

strTok posn s = tok' (Str $ init $ tail s) posn 
numTok posn s = tok' (Num $ read s) posn
idTok  posn s = tok' (ID s) posn

-- just so we can compile this stuff seperately
main :: IO ()
main = return ()
}
