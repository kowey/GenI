{
module Lex2 (lexer) where

import ParserLib 
}

%wrapper "posn"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-

  $white+				;
  \%.*				        ;
  \".*\"                      { \p s -> tok' (Str $ init $ tail s) p }

  True                        { tok TTT }
  False                       { tok FFF }

  TSemantics                 { tok TSemantics }
  TestSuite                  { tok TestSuiteTok }
  Graphical                  { tok Graphical  }
 
  Grammar                    { tok GrammarTok }

  Macros                     { tok Macros     }
  Lexicon                    { tok Lexicon    }
  SemLexicon                 { tok SemLexicon }

  GrammarType                { tok GrammarType }
  GeniHand                   { tok GeniHandTok }
  TAGML                      { tok TAGMLTok }

  Optimisations              { tok Optimisations }
  Polarised                  { tok Polarised  }
  AutoPol                    { tok AutoPol }
  PolSig                     { tok PolSig } 
  Predicting                 { tok Predicting }
  SemFiltered                { tok SemFiltered } 
  ChartSharing               { tok ChartSharing } 
  OrderedAdj                 { tok OrderedAdj }
  FootConstraint             { tok FootConstraint }
  Batch                      { tok Batch }
  Repeat                     { tok Repeat }
  ExtraPolarities            { tok ExtraPolarities }

  restrictors                { tok RestrictorsTok }
  semantics                  { tok Semantics  }
  polarities                 { tok Polarities }
  predictors                 { tok Predictors }
  
  \+                         { tok PlusTok }
  \-                         { tok MinusTok }
  \=                         { tok Eq }
  
  initial                    { tok Init }
  auxiliary                  { tok Aux }
  anchor                     { tok Anchor }
  family                     { tok FamilyTok }
  lex                        { tok Lexeme }
  type                       { tok Type }
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
 
  [$digit]+                           { \p s -> tok' (Num $ read s) p  } 
  [$alpha \_][$alpha $digit \_ \/ \. ]*   { \p s -> tok' (ID s) p  } 
   

{
lexer :: String -> [(Token, Int, Int)]
lexer = alexScanTokens

-- Token builder:
tok  t p s = tok' t p             -- discards the s (this is sugar)
tok' t (AlexPn _ l c) = (t, l, c) -- used when you need to access the s
}
