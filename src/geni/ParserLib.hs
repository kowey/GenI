module ParserLib 
where

type PosToken = (Token, Int, Int)

data Token = 
    TTT | FFF |
    -- 
    GrammarTok | TSemantics |
    Graphical |
    -- grammar file
    GrammarType | GeniHandTok | TAGMLTok |
    Macros | Lexicon | SemLexicon | 
    -- optimisations
    Optimisations | 
    Polarised | PolSig | Predicting | ChartSharing | ExtraPolarities |
    FootConstraint | SemFiltered | OrderedAdj |  
    Batch | Repeat |  
    --
    Grammar | Entry | Syntax | Features | Avm | Var | 
    Id | Sem | Label |
    Const | Predicate | Argument |
    Mark | Name | Negated | Eq | Feature |
    Node | Literal | Lt | Gt | Bar |
    Init | Aux | 
    Anchor | Lexeme | Type | LSubst | LFoot | Aconstr | Noadj |
    Comma | Colon | Bang | Str String |
    OC | CC | OP | CP | OB | CB | ID String | Num Int |
    Semantics | Polarities | Predictors |
    Begin | End |
    PolPositive | PolNegative | PolNeutral
  deriving (Eq, Show, Read)

parserError :: [PosToken] -> a
parserError [] = error "Parser error because input file ended unexpectantly."
parserError ((t, l, c):cs) = 
    let {
	mess = if (l == 0) 
	       then "Parse error in arguments near col. " ++ show c
               else "Parse error in input file near line " ++ 
				   show l ++ 
				   ", col. " ++
				   show c
    } in 
    error (mess)
