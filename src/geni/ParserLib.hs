module ParserLib 
where

type PosToken = (Token, Int, Int)

data Token = 
    TTT | FFF |
    -- 
    GrammarTok | TSemantics | TestSuiteTok | 
    Graphical |
    -- grammar file
    GrammarType | GeniHandTok | TAGMLTok |
    Macros | Lexicon | SemLexicon | 
    -- optimisations
    Optimisations | 
    Polarised | AutoPol | PolSig | Predicting | ChartSharing | ExtraPolarities |
    FootConstraint | SemFiltered | OrderedAdj |  
    Batch | Repeat |  
    --
    Grammar | Entry | Syntax | Features | Avm | Var | 
    Id | Sem | Label |
    Const | Predicate | Argument |
    Mark | Name | Negated | Eq | Feature |
    Node | Literal | Lt | Gt | Bar |
    Init | Aux | FamilyTok |  
    Anchor | Lexeme | Type | LSubst | LFoot | Aconstr | Noadj |
    Comma | Colon | Bang | Str String |
    OC | CC | OP | CP | OB | CB | ID String | Num Int |
    Semantics | RestrictorsTok | Polarities | Predictors |
    Begin | End |
    PlusTok | MinusTok 
  deriving (Eq, Show, Read)

-- -------------------------------------------------------------------
-- error recovery
-- the monad below is from the Happy manual
-- -------------------------------------------------------------------

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

parserError :: [PosToken] -> E a
parserError [] = failE $ "Parser error: input ended unexpectantly: " ++ 
                        "maybe your parentheses do not match?"
parserError ((t, l, c):cs) = 
    let mess = if (l == 0) 
               then "Parse error in arguments near col. " ++ show c
               else "Parse error in input file near line " ++ show l 
                    ++ ", col. " ++ show c
    in failE mess

simpleParserError :: [PosToken] -> a
simpleParserError [] = error "Parser error because input file ended unexpectantly."
simpleParserError ((t, l, c):cs) = 
    let {
	mess = if (l == 0) 
	       then "Parse error in arguments near col. " ++ show c
               else "Parse error in input file near line " ++ 
				   show l ++ 
				   ", col. " ++
				   show c
    } in 
    error (mess)
