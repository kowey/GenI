% GenI surface realiser
% Copyright (C) 2005 Carlos Areces and Eric Kow
%
% This program is free software; you can redistribute it and/or
% modify it under the terms of the GNU General Public License
% as published by the Free Software Foundation; either version 2
% of the License, or (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
%
% You should have received a copy of the GNU General Public License
% along with this program; if not, write to the Free Software
% Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

\chapter{GDE Parser}
\label{cha:GDEParser}

This parser handles the pivot GDE format for use in conjunction with XMG tools.
It is implemented in Parsec.  

\begin{code}
module GdeParser(gdeLexicon, gdeSem, relthetaToSem) where
\end{code}

\ignore{
\begin{code}
import Btypes
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language (emptyDef)
import Text.ParserCombinators.Parsec.Token (TokenParser, 
    LanguageDef(..), makeTokenParser)
import qualified Text.ParserCombinators.Parsec.Token as P
\end{code}
}

\section{Lexer}

Some preliminaries about the lexicon format - comments start with 
\verb!%!.  

\begin{code}
lexer  = makeTokenParser 
         (emptyDef
         { commentLine = "%"
         , commentStart = "/*"
         , commentEnd = "*/"
         })

whiteSpace = P.whiteSpace lexer
identifier = P.identifier lexer
natural   = P.natural lexer
squares   = P.squares lexer
symbol    = P.symbol  lexer
comma     = P.comma   lexer
braces    = P.braces  lexer
semi      = P.semi    lexer
\end{code}

\section{Parser}

The lexicon format consists of arbitrarily many entries, 
seperated by whitespace.  

FIXME: Ideally they should be seperated by newlines to
match convention... but it's hard for me to figure out 
how to do this cleanly because I'm using a lexeme parser,
which (unsurprisingly) treats \verb!'\n'! as whitespace.

\begin{code}
gdeLexicon :: Parser [ILexEntry] 
gdeLexicon = do 
  l <- many gdeEntry 
  eof
  return l

gdeEntry :: Parser ILexEntry 
gdeEntry = do 
  let gdeKey k = do { symbol ("*"++ k)
                    ; optional space; char ':'; optional spaces }
  whiteSpace
  gdeKey "ENTRY"; lex <- identifier <?> "lemma"
  gdeKey "CAT"  ;        identifier <?> "part of speech" -- ignored
  gdeKey "SEM"  ; reltheta <- gdeSem <?> "semantics"
  whiteSpace 
  gdeKey "ACC"  ; optional natural -- ignored
  gdeKey "FAM"  ; fam <- identifier <?> "family name"
  gdeKey "FILTERS"   ; filts <- gdeFeats <?> "filters"
  gdeKey "EX"        ; braces (many $ noneOf "{}") <?> "exceptions"
  gdeKey "EQUATIONS" ; optional newline;
  equations <- gdeEquations <?> "equations"
  gdeKey "COANCHORS" ; optional newline;
  coanch    <- gdeCoanchors <?> "coanchors"
  -- pack this all into a lexical entry 
  let (sem,enrich) = relthetaToSem reltheta
      ilex = emptyLE { iword    = lex
                     , ifamname = fam
                     , ifilters = ("family",GConst [fam]) : filts
                     , ipfeat = enrich ++ equations ++ coanch
                     , isemantics = sem
                     }
  return ilex
\end{code}

Feature structures are pretty down to earth : \verb!val = att!
with only newlines to seperate individual attribute-value 
pairs.

\begin{code}
gdeFeats :: Parser Flist
gdeFeats = option [] $ squares $ sepBy gdeAttVal comma

gdeAttVal :: Parser AvPair
gdeAttVal = do
  att <- identifier <?> "attribute"; symbol "="
  whiteSpace
  val <- many (alphaNum <|> oneOf "+-") <?> "value"
  whiteSpace
  return (att, GConst [val])
\end{code}

We translate path equations into attribute value pairs.  Equations may
take the form \verb!node -> val = att!).  The \verb!node ->! is
optional.  If a node is provided, the val is prefixed by the node like
so: \verb!node.val!. 

\begin{code}
gdeEquations :: Parser Flist
gdeEquations = sepEndBy gdeEquation whiteSpace 

gdeEquation :: Parser AvPair 
gdeEquation = do
  node      <- (option "" gdeEquationNode) <?> "path equation node"
  (att,val) <- gdeAttVal
  let node2 = if null node then "" else node ++ "."
  return (node2 ++ att, val)

gdeEquationNode :: Parser String
gdeEquationNode = do
  i <- identifier; symbol "->"; return i  
\end{code}

Co-anchors take the form \verb!node -> word/pos!  Ignoring the
\verb!pos!, we treat all co-anchors as if they were equations of the
form \verb!node -> lex = word!.  See above for how we deal with
equations.

\begin{code}
gdeCoanchors :: Parser Flist
gdeCoanchors = sepBy gdeCoanchorEntry whiteSpace 

gdeCoanchorEntry :: Parser AvPair 
gdeCoanchorEntry = do
  node <- gdeEquationNode <?> "coancher node"
  word <- identifier <?> "coanchor lex"
  -- the rest of this stuff is ignored
  symbol "/" ; identifier <?> "part of speech"
  return (node ++ ".lex", GConst [word])
\end{code}

The semantics of a lexical entry consists of a predicate followed by some
thematic roles, for example, \verb!love<exp,cause>!.  The thematic roles
and the angle brackets may be omitted for nouns

\begin{code}
gdeSem :: Parser ([String],[String])
gdeSem = do
  predicate      <- identifier <?>            "predicate"
  thematicRoles  <- option [] gdeThetaRoles <?> "thematic roles"
  morePredicates <- many (do { semi; identifier }) <?> "other predicates"
  return (predicate:morePredicates, thematicRoles)

gdeThetaRoles = do 
  --- note that here we do not tolerate whitespace
  between (char '<') (char '>') (sepBy identifier (char ','))
\end{code}

What follows below is a bunch of little helper functions for putting
everything together.

\paragraph{relthetaToSem} translates a tuple of relations and thematic roles
like \texttt{["like", "much"], ["exp", "cause"]} into 
\begin{enumerate}
\item a semantics like
\verb$hates(E), agt(E,X1), pat(E,X2), much(E)$.  Note that E, X1, X2, are just
variable names that we make up.
\item a set of enrichement instructions of the form
\begin{verbatim}
interface.idx  = E
interface.arg1 = X1
interface.arg2 = X2
\end{verbatim}
      This is so that we can propagate the indices to trees
\end{enumerate}

\begin{code}
relthetaToSem :: ([String],[String]) -> (Sem,Flist)
relthetaToSem (rels, thetas) =
  let indices = take (length thetas) [1..]
      varE    = GVar "E"
      varX n  = GVar ("X" ++ show n)
      -- lexical entry is treated as having anonymous handle 
      relPredFn :: String -> Pred
      relPredFn   x = (GAnon, x, [varE])
      thetaPredFn :: (Num a) => String -> a -> Pred
      thetaPredFn t x = (GAnon, t, [varE, varX x])
      --
      relEnrich = ("interface.index", varE) 
      thetaEnrichFn r x = [ ("interface.arg" ++ show x, varX x)
                          , ("interface.theta" ++ show x, GConst [r]) ]
      --
      sem :: Sem 
      sem = map relPredFn rels ++ zipWith thetaPredFn thetas indices 
      enrich :: Flist
      enrich = relEnrich : (concat $ zipWith thetaEnrichFn thetas indices)
  in -- trace (showSem sem) $ 
     (sortSem sem,enrich)
\end{code}
