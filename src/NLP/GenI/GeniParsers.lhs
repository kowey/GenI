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

\chapter{File formats}
\label{cha:formats}
\label{cha:GeniParsers}

This chapter is a description of the file formats used by \geni.  We'll be
using EBNFs to describe the format below.   Here are some rules and types of
rules we leave out, and prefer to describe informally:

\begin{verbatim}
<alpha-numeric>
<string-literal> (stuff between quotes)
<opt-whatever> (systematically... "" | <whatever>)
<keyword-whatever> (systematically.. "whatever" ":")
\end{verbatim}

\ignore{
\begin{code}
module NLP.GenI.GeniParsers (
  -- * Test suites
  geniTestSuite, geniSemanticInput, geniTestSuiteString,
  geniDerivations,
  toSemInputString,
  -- * Trees
  geniMacros, geniTagElems,
  -- * Lexicon and morph
  geniLexicon, geniMorphInfo,
  -- * Basics
  geniFeats, geniPolarities, geniSemantics, geniValue, geniWords,
  -- * Helpers
  geniWord, geniLanguageDef, tillEof,
  --
  parseFromFile, -- UTF-8 version
  module Text.ParserCombinators.Parsec
) where

import NLP.GenI.General ((!+!), Interval, ival)
import NLP.GenI.GeniVal (mkGConst, mkGVar, mkGAnon)
import NLP.GenI.Btypes
import NLP.GenI.Tags (TagElem(..), emptyTE, setTidnums)
import NLP.GenI.GeniShow (GeniShow(geniShow))
import NLP.GenI.PolarityTypes

import Control.Monad (liftM, when)
import Data.List (sort)
import qualified Data.Map  as Map
import qualified Data.Tree as T
import Text.ParserCombinators.Parsec hiding (parseFromFile)
import Text.ParserCombinators.Parsec.Language (emptyDef)
import Text.ParserCombinators.Parsec.Token (TokenParser,
    LanguageDef(..), makeTokenParser)
import qualified Text.ParserCombinators.Parsec.Token as P
import qualified System.IO.UTF8 as UTF8

\end{code}
}

\section{General notes}

\subsection{Comments}

Any \geni format file can include comments.  Comments start \verb!%!.
There is also the option of using \verb'/* */' for embedded comments.

\subsection{Reserved words}

The following are reserved words.  You should not use them as variable names.
NB: the reserved words are indicated below between quotes; eg.  ``semantics''.
You can ignore C pre-processor noise such as \verb!#define SEMANTICS!

\begin{includecodeinmanual}
\begin{code}
-- reserved words
#define SEMANTICS       "semantics"
#define SENTENCE        "sentence"
#define OUTPUT          "output"
#define TRACE           "trace"
#define ANCHOR          "anchor"
#define SUBST           "subst"
#define FOOT            "foot"
#define LEX             "lex"
#define TYPE            "type"
#define ACONSTR_NOADJ   "aconstr:noadj"
#define INITIAL         "initial"
#define AUXILIARY       "auxiliary"
#define IDXCONSTRAINTS  "idxconstraints"
#define BEGIN           "begin"
#define END             "end"
\end{code}
\end{includecodeinmanual}

\subsection{Lexer}

For reference, we include the Parsec LanguageDef that we use to implement
the \geni format.

\begin{includecodeinmanual}
\begin{code}
geniLanguageDef :: LanguageDef ()
geniLanguageDef = emptyDef
         { commentLine = "%"
         , commentStart = "/*"
         , commentEnd = "*/"
         , opLetter = oneOf ""
         , reservedOpNames = [""]
         , reservedNames =
             [ SEMANTICS , SENTENCE, OUTPUT, IDXCONSTRAINTS, TRACE
             , ANCHOR , SUBST , FOOT , LEX , TYPE , ACONSTR_NOADJ
             , INITIAL , AUXILIARY
             , BEGIN , END ]
         , identLetter = identStuff
         , identStart  = identStuff
         }
  where identStuff = alphaNum <|> oneOf "_'+-."
\end{code}
\end{includecodeinmanual}

\section{The basics}

\subsection{Variables and constants}

Below are some examples of \geni variables and constants.  Note that we support
atomic disjunction of constants, as in \verb!Foo|bar|baz!, but not variables.

\begin{center}
\begin{tabular}{ll}
anonymous variable & \verb!?_! or \verb!_! \\
variables & \verb!Foo!, \verb!?X! or \verb!?x! \\
constants & \verb!Foo!, \verb!foo!, \verb!X!, \verb!x! or \verb!Foo|bar! \\
\end{tabular}
\end{center}

Here is an EBNF for GenI variables and constants

\begin{SaveVerbatim}{KoweyTmp}
<value>         ::= <variable> | <anonymous-variable> | <constant-disj>
<variable>      ::= "?" <identifier>
<anonymous>     ::= "?_" | "_"
<constant-disj> ::= <constant> (| <constant>)*
<constant>      ::= <identifier>
<identifier>    ::= <alphanumeric> | "+" | "-" | "_"
\end{SaveVerbatim}
\begin{center}
\fbox{\BUseVerbatim{KoweyTmp}}
\end{center}

\begin{code}
geniValue :: Parser GeniVal
geniValue =   ((try $ anonymous) <?> "_ or ?_")
          <|> (constants  <?> "a constant or atomic disjunction")
          <|> (variable   <?> "a variable")
  where
    question = "?"
    --
    constants :: Parser GeniVal
    constants =
      do (c:cs) <- sepBy1 (looseIdentifier <|> stringLiteral) (symbol "|")
         return (mkGConst c cs)
    variable :: Parser GeniVal
    variable =
      do symbol question
         v <- identifier
         return (mkGVar v Nothing) --FIXME
    anonymous :: Parser GeniVal
    anonymous =
      do optional $ symbol question
         symbol "_"
         return mkGAnon
\end{code}

\subsection{Feature structures}

In addition to variables and constants, \geni also makes heavy use of flat
feature structures.  They take the form \verb![foo:bar ping:?Pong]!, or more
formally,

\begin{SaveVerbatim}{KoweyTmp}
<feature-structure>      ::= "[" <atttribute-value-pair>* "]"
<attribute-value-pair>   ::= <identifier-or-reserved> ":" <value>
<identifier-or-reserved> ::= <identifier> | <reserved>
\end{SaveVerbatim}
\begin{center}
\fbox{\BUseVerbatim{KoweyTmp}}
\end{center}

\begin{code}
geniFeats :: Parser Flist
geniFeats = option [] $ squares $ many geniAttVal

geniAttVal :: Parser AvPair
geniAttVal = do
  att <- identifierR <?> "an attribute"; colon
  val <- geniValue <?> "a GenI value"
  return $ AvPair att val
\end{code}

\subsection{Semantics}
\label{sec:geni-semantics}

A \jargon{semantics} is basically a set of literals.  Semantics are used in
to provide \geni input (section \ref{sec:geni-input-semantics}) and in the
definition of lexical entries (section \ref{sec:geni-lexicon}).

Notice that this is a flat semantic representation!  No literals within
literals, please.  A literal can take one of two forms:
\begin{verbatim}
  handle:predicate(arguments)
         predicate(arguments)
\end{verbatim}

The arguments are space-delimited.  Not providing a handle is
equivalent to providing an anonymous one.

\begin{SaveVerbatim}{KoweyTmp}
<semantics>      ::= <keyword-semantics> "[" <literal>* "]"
<literal>        ::= <identifier> "(" <value>* ")"
\end{SaveVerbatim}
\begin{center}
\fbox{\BUseVerbatim{KoweyTmp}}
\end{center}

\begin{code}
geniSemantics :: Parser Sem
geniSemantics =
  do sem <- many (geniLiteral <?> "a literal")
     return (sortSem sem)

geniLiteral :: Parser Pred
geniLiteral =
  do handle    <- option mkGAnon handleParser <?> "a handle"
     predicate <- geniValue <?> "a predicate"
     pars      <- parens (many geniValue) <?> "some parameters"
     --
     return (handle, predicate, pars)
  where handleParser =
          try $ do { h <- geniValue ; char ':' ; return h }
\end{code}

\section{Semantic inputs and test suites}
\label{sec:geni-input-semantics}

\subsection{Semantic input}

The semantic input can either be provided directly in the graphical interface
or as part of a test suite.

The format for semantic inputs is actually a bit richer than the core
definition in section \ref{sec:geni-semantics}, but I have not yet written the
documentation for it.

\textbf{TODO}: The semantics may contain literal based constraints as described
in section \ref{sec:fixme}.  These constraints are just a space-delimited list
of String.  When returning the results, we separate them out from the semantics
proper so that they can be treated separately.  Index constraints are
represented as feature structures.

\begin{code}
geniSemanticInput :: Parser (Sem,Flist,[LitConstr])
geniSemanticInput =
  do keywordSemantics
     (sem,litC) <- liftM unzip $ squares $ many literalAndConstraint
     idxC       <- option [] geniIdxConstraints
     --
     let sem2     = createHandles sem
         semlitC2 = [ (s,c) | (s,c) <- zip sem2 litC, (not.null) c ]
     return (createHandles sem, idxC, semlitC2)
  where
     -- set all anonymous handles to some unique value
     -- this is to simplify checking if a result is
     -- semantically complete
     createHandles :: Sem -> Sem
     createHandles = zipWith setHandle ([1..] :: [Int])
     --
     setHandle i (h, pred_, par) =
       let h2 = if isAnon h
                then mkGConst ("genihandle" ++ show i) []
                else h
       in (h2, pred_, par)
     --
     literalAndConstraint :: Parser (Pred, [String])
     literalAndConstraint =
       do l <- geniLiteral
          t <- option [] $ squares $ many identifier
          return (l,t)

-- | The original string representation of the semantics (for gui)
geniSemanticInputString :: Parser String
geniSemanticInputString =
 do keywordSemantics
    s <- squaresString
    whiteSpace
    optional geniIdxConstraints
    return s

geniIdxConstraints :: Parser Flist
geniIdxConstraints = keyword IDXCONSTRAINTS >> geniFeats

squaresString :: Parser String
squaresString =
 do char '['
    s <- liftM concat $ many $ (many1 $ noneOf "[]") <|> squaresString
    char ']'
    return $ "[" ++ s ++ "]"

-- the output end of things
-- displaying preformatted semantic input

data SemInputString = SemInputString String Flist

instance GeniShow SemInputString where
 geniShow (SemInputString semStr idxC) =
   SEMANTICS ++ ":" ++ semStr ++ (if null idxC then "" else r)
   where r = "\n" ++ IDXCONSTRAINTS ++ ": " ++ showFlist idxC

toSemInputString :: SemInput -> String -> SemInputString
toSemInputString (_,lc,_) s = SemInputString s lc
\end{code}

\subsection{Test suite}

\geni accepts an entire test suite of semantic inputs that you can choose from.
The test suite entries can be named.  In fact, it is probably a good idea to do
so, because the names are often shorter than the expected output, and easier to
read than the semantics.  Note the expected output isn't used by \geni itself,
but external tools that ``test'' \geni.

\begin{SaveVerbatim}{KoweyTmp}
<test-suite>       ::= <test-suite-entry>*
<test-suite-entry> ::= <opt-identifier> <semantics> <expected-output>*
<expected-output>  ::= <opt-keyword-sentence> "[" <identifier>* "]"
\end{SaveVerbatim}
\begin{center}
\fbox{\BUseVerbatim{KoweyTmp}}
\end{center}

\begin{code}
geniTestSuite :: Parser [TestCase]
geniTestSuite =
  tillEof (many geniTestCase)

-- | Just the String representations of the semantics
--   in the test suite
geniTestSuiteString :: Parser [String]
geniTestSuiteString =
  tillEof (many geniTestCaseString)

-- | This is only used by the script genimakesuite
geniDerivations :: Parser [TestCaseOutput]
geniDerivations = tillEof $ many geniOutput

geniTestCase :: Parser TestCase
geniTestCase =
  do name  <- option "" (identifier <?> "a test case name")
     seminput <- geniSemanticInput
     sentences <- many geniSentence
     outputs   <- many geniOutput
     return $ TestCase name "" seminput sentences outputs

-- note that the keyword is NOT optional
type TestCaseOutput = (String, Map.Map (String,String) [String])
geniOutput :: Parser TestCaseOutput
geniOutput =
 do ws <- keyword OUTPUT >> (squares geniWords)
    ds <- Map.fromList `fmap` many geniTraces
    return (ws, ds)

geniTraces :: Parser ((String,String), [String])
geniTraces =
 do keyword TRACE
    squares $ do
      k1 <- withWhite geniWord
      k2 <- withWhite geniWord
      whiteSpace >> char '!' >> whiteSpace
      traces <- sepEndBy1 geniWord whiteSpace
      return ((k1,k2), traces)

withWhite :: Parser a -> Parser a
withWhite p = p >>= (\a -> whiteSpace >> return a)

geniSentence :: Parser String
geniSentence = optional (keyword SENTENCE) >> squares geniWords

geniWords :: Parser String
geniWords =
 unwords `fmap` (sepEndBy1 geniWord whiteSpace <?> "a sentence")

geniWord :: Parser String
geniWord = many1 (noneOf "[]\v\f\t\r\n ")

-- | The original string representation of a test case semantics
--   (for gui)
geniTestCaseString :: Parser String
geniTestCaseString =
 do option "" (identifier <?> "a test case name")
    s <- geniSemanticInputString
    many geniSentence
    many geniOutput
    return s
\end{code}

\section{Lexicon}
\label{sec:geni-lexicon}

The lexicon associates semantic entries with lemmas and trees.

\subsection{Lexicon examples}

There are two ways to write the lexicon.  We show the old (deprecated)
way first because most of the examples are still written in this style.

\paragraph{Example 1 (deprecated)}

\begin{verbatim}
le clitic (?I)
semantics:[]

le Det (?I)
semantics:[def(?I)]

livre nC (?I)
semantics:[book(?I)]

persuader vArity3 (?E ?X ?Y ?Z)
semantics:[?E:convince(?X ?Y ?Z)]

persuader v vArity3controlObj
semantics:[?E:convince(?X ?Y ?Z)]
\end{verbatim}

\paragraph{Example 2 (preferred)}

\begin{verbatim}
detester n0Vn1
equations:[theta1:agent theta2:patient arg1:?X arg2:?Y evt:?L]
filters:[family:n0Vn1]
semantics:[?E:hate(?L) ?E:agent(?L ?X) ?E:patient(?L ?Y)]
\end{verbatim}

\subsection{Notes about lexicons}

\begin{itemize}
\item The semantics associated with a lexicali item may have more than one literal
\begin{verbatim}
cher adj (?E ?X ?Y)
semantics:[?E:cost(?X ?Y) ?E:high(?Y)]
\end{verbatim}

\item A lemma may have more than one distinct semantics
\begin{verbatim}
bank n (?X)
semantics:[bank(?X)]

bank v (?E ?X ?D)
semantics:[?E:lean(?X,?D)]
\end{verbatim}

\item A semantics may be realised by more than one lexical entry (e.g.  synonynms)
\begin{verbatim}
livre nC (?I)
semantics:[book(?I)]

bouquin nC (?I)
semantics:[book(?I)]
\end{verbatim}
\end{itemize}

\subsection{Lexicon EBNF}

\begin{SaveVerbatim}{KoweyTmp}
<lexicon>        ::= <lexicon-entry>*
<lexicon-entry>  ::= <lexicon-header> <opt-filters> <semantics>
<lexicon-header> ::= <lemma> <family> <parameters>
                   | <lemma> <family> <keyword-equations> <feature-structure>
<parameters>     ::= "(" <value>* <opt-interface> ")"
<interface>      ::= "!" <attribute-value-pairs>*
<filters>        ::= <keyword-filter> <feature-structure>
\end{SaveVerbatim}
\begin{center}
\fbox{\BUseVerbatim{KoweyTmp}}
\end{center}

\begin{code}
geniLexicon :: Parser [ILexEntry]
geniLexicon = tillEof $ many1 geniLexicalEntry

geniLexicalEntry :: Parser ILexEntry
geniLexicalEntry =
  do lemma  <- (looseIdentifier <|> stringLiteral) <?> "a lemma"
     family <- identifier <?> "a tree family"
     (pars, interface) <- option ([],[]) $ parens paramsParser
     equations <- option [] $ do keyword "equations"
                                 geniFeats <?> "path equations"
     filters <- option [] $ do keyword "filters"
                               geniFeats
     keywordSemantics
     (sem,pols) <- squares geniLexSemantics
     --
     return emptyLE { iword = [lemma]
                    , ifamname = family
                    , iparams = pars
                    , iinterface = sortFlist interface
                    , iequations = equations
                    , ifilters = filters
                    , isemantics = sem
                    , isempols = pols }
  where
    paramsParser :: Parser ([GeniVal], Flist)
    paramsParser = do
      pars <- many geniValue <?> "some parameters"
      interface <- option [] $ do symbol "!"
                                  many geniAttVal
      return (pars, interface)

geniLexSemantics :: Parser (Sem, [[Int]])
geniLexSemantics =
  do litpols <- many (geniLexLiteral <?> "a literal")
     return $ unzip litpols

geniLexLiteral :: Parser (Pred, [Int])
geniLexLiteral =
  do (handle, hpol) <- option (mkGAnon,0) (handleParser <?> "a handle")
     predicate  <- geniValue <?> "a predicate"
     paramsPols <- parens (many geniPolValue) <?> "some parameters"
     --
     let (pars, pols) = unzip paramsPols
         literal = (handle, predicate, pars)
     return (literal, hpol:pols)
  where handleParser =
          try $ do { h <- geniPolValue; colon; return h }

geniPolValue :: Parser (GeniVal, Int)
geniPolValue =
  do p <- geniPolarity
     v <- geniValue
     return (v,p)
\end{code}

\section{Tree schemata}

The tree schemata file (for historical reasons, this is also called the macros
file) contains a set of unlexicalised trees organised into families.  Such
``macros'' consist of a

\begin{enumerate}
\item a family name and (optionally) a macro name
\item a list of parameters
\item ''initial'' or ''auxiliary''
\item a tree.
\end{enumerate}

\subsection{Trees}

\jargon{Trees} are recursively defined structure of form \verb!node{tree*}!
For example, in the table below, the  structure on the left should produce the
tree on the right:

\begin{SaveVerbatim}{KoweyTmp}
n1{
   n2
   n3{
      n4
      n5
     }
   n6
}
\end{SaveVerbatim}
\begin{tabular}{ll}
\BUseVerbatim{KoweyTmp} & \includegraphics[scale=0.50]{images/tree-format-example.png} \\
\end{tabular}

\subsection{Nodes}

\jargon{Nodes} consist of
\begin{enumerate}
\item a name
\item a type (optional)
\item either a lexeme, or top and bottom feature structures. Here are examples of the five possible kinds of nodes:
\end{enumerate}

\noindent
Here are some examples of nodes
\begin{verbatim}
 n1 [cat:n idx:?I]![cat:n idx:?I]            % basic
 n3 type:subst [cat:n idx:?Y]![cat:n idx:?Y] % subst
 n4 type:foot  [cat:n idx:?Y]![cat:n idx:?Y] % foot
 n5 type:lex   "de"                        % coanchor
 n2 anchor                                 % anchor
 n5 aconstr:noadj % node with a null-adjunction constraint (other than subst or foot)
\end{verbatim}

\subsection{Example}

\begin{verbatim}
adj:post(?I)  auxiliary
n0[cat:n idx:?I det:_]![cat:n idx:?I det:minus ]
{
  n1 type:foot [cat:n idx:?I det:minus]![cat:n idx:?I det:minus]
  n2[cat:a]![]
  {
    n3 anchor
  }
}

adj:pre(?I)  auxiliary
n0[cat:n idx:?I det:_ qu:_]![cat:n idx:?I det:minus ]
{
  n1[cat:a]![]
  {
    n2 anchor
  }
  n3 type:foot [cat:n idx:?I det:minus]![cat:n idx:?I det:minus]
}

vArity2:n0vn1(?E ?X ?Y) initial
n1[cat:p]![]
{
  n2 type:subst [cat:n idx:?X det:plus]![cat:n idx:?X]
  n3[cat:v idx:?E]![]
  {
    n4 anchor
  }
  n5 type:subst [cat:n idx:?Y det:plus]![cat:n idx:?Y]
}
\end{verbatim}

\subsection{EBNF}

\begin{SaveVerbatim}{KoweyTmp}
<macros> ::= <macro>*
<macro>  ::= <family-name> <opt-macro-name> <parameters> <tree-type> <tree>
             <opt-semantics> <opt-trace>

<parameters>     ::= "(" <value>* <opt-interface> ")"
<interface>      ::= ! <attribute-value-pair>*
<macro-name>     ::= <identifier>
<tree-type>      ::= "initial" | "auxiliary"
<trace>          ::= <keyword-trace> "[" <identifier>* "]"

<tree>           ::= <node> | <node> "{" <tree>* "}"
<node>           ::= <node-name> <opt-node-type> <node-payload>
<node-name>      ::= <identifier>
<node-type>      ::= <keyword-type> <core-node-type> | "anchor"
<core-node-type> ::= "foot" | "subst" | "lex"
<node-payload>   ::= <string-literal> | <feature-structure> "!" <feature-structure>
\end{SaveVerbatim}
\begin{center}
\fbox{\BUseVerbatim{KoweyTmp}}
\end{center}

\begin{code}
geniMacros :: Parser [Ttree GNode]
geniMacros = tillEof $ many geniTreeDef

initType, auxType :: Parser Ptype
initType = do { reserved INITIAL ; return Initial  }
auxType  = do { reserved AUXILIARY ; return Auxiliar }

geniTreeDef :: Parser (Ttree GNode)
geniTreeDef =
  do sourcePos <- getPosition
     family   <- identifier
     tname    <- option "" $ do { colon; identifier }
     (pars,iface)   <- geniParams
     theTtype  <- (initType <|> auxType)
     theTree  <- geniTree
     -- sanity checks?
     let treeFail x =
          do setPosition sourcePos -- FIXME does not do what I expect
             fail $ "In tree " ++ family ++ ":" ++ tname ++ " " ++ show sourcePos ++ ": " ++ x
     let theNodes = T.flatten theTree
         numFeet    = length [ x | x <- theNodes, gtype x == Foot ]
         numAnchors = length [ x | x <- theNodes, ganchor x ]
     when (not $ any ganchor theNodes) $
       treeFail "At least one node in an LTAG tree must be an anchor"
     when (numAnchors > 1) $
       treeFail "There can be no more than 1 anchor node in a tree"
     when (numFeet > 1) $
       treeFail "There can be no more than 1 foot node in a tree"
     when (theTtype == Initial && numFeet > 0) $
       treeFail "Initial trees may not have foot nodes"
     --
     psem     <- option Nothing $ do { keywordSemantics; liftM Just (squares geniSemantics) }
     ptrc     <- option [] $ do { keyword TRACE; squares (many identifier) }
     --
     return TT{ params = pars
              , pfamily = family
              , pidname = tname
              , pinterface = sortFlist iface
              , ptype = theTtype
              , tree = theTree
              , ptrace = ptrc
              , psemantics = psem
              }

geniTree :: Parser (T.Tree GNode)
geniTree =
  do node <- geniNode
     kids <- option [] (braces $ many geniTree)
             <?> "child nodes"
     -- sanity checks
     let noKidsAllowed t c = when (c node && (not.null $ kids)) $
             fail $ t ++ " nodes may *not* have any children"
     noKidsAllowed "Anchor"       $ ganchor
     noKidsAllowed "Substitution" $ (== Subs) . gtype
     noKidsAllowed "Foot"         $ (== Foot) . gtype
     --
     return (T.Node node kids)

geniNode :: Parser GNode
geniNode =
  do name      <- identifier
     nodeType  <- option "" ( (keyword TYPE >> typeParser)
                              <|>
                              reserved ANCHOR)
     lex_   <- if nodeType == LEX
                  then (sepBy (stringLiteral<|>identifier) (symbol "|") <?> "some lexemes")
                  else return []
     constr <- case nodeType of
               ""     -> adjConstraintParser
               ANCHOR -> adjConstraintParser
               _  -> return True
     (top_,bot_) <- -- features only obligatory for non-lex nodes
                    if nodeType == LEX
                       then option ([],[]) $ try topbotParser
                       else topbotParser
     --
     let top   = sort top_
         bot   = sort bot_
         nodeType2 = case nodeType of
                       ANCHOR  -> Lex
                       LEX     -> Lex
                       FOOT    -> Foot
                       SUBST   -> Subs
                       ""        -> Other
                       other     -> error ("unknown node type: " ++ other)
     return $ GN { gnname = name, gtype = nodeType2
                 , gup = top, gdown = bot
                 , glexeme  = lex_
                 , ganchor  = (nodeType == ANCHOR)
                 , gaconstr = constr
                 , gorigin  = "" }
  where
    typeParser = choice $ map (try.symbol) [ ANCHOR, FOOT, SUBST, LEX ]
    adjConstraintParser = option False $ reserved ACONSTR_NOADJ >> return True
    topbotParser =
      do top <- geniFeats <?> "top features"
         symbol "!"
         bot <- geniFeats <?> "bot features"
         return (top,bot)

-- | This makes it possible to read anchored trees, which may be
--   useful for debugging purposes.
--
--   FIXME: note that this is very rudimentary; we do not set id numbers,
--   parse polarities. You'll have to call
--   some of our helper functions if you want that functionality.
geniTagElems :: Parser [TagElem]
geniTagElems = tillEof $ setTidnums `fmap` many geniTagElem

geniTagElem :: Parser TagElem
geniTagElem =
 do family   <- identifier
    tname    <- option "" $ do { colon; identifier }
    iface    <- (snd `liftM` geniParams) <|> geniFeats
    theType  <- initType <|> auxType
    theTree  <- geniTree
    sem      <- do { keywordSemantics; squares geniSemantics }
    --
    return $ emptyTE { idname = tname
                     , ttreename = family
                     , tinterface = iface
                     , ttype  = theType
                     , ttree = theTree
                     , tsemantics = sem }

-- | 'geniParams' recognises a list of parameters optionally followed by a
--  bang (\verb$!$) and a list of attribute-value pairs.  This whole thing is
--  to wrapped in the parens.
--
--  TODO: deprecate
geniParams :: Parser ([GeniVal], Flist)
geniParams = parens $ do
  pars <- many geniValue <?> "some parameters"
  interface <- option [] $ do { symbol "!"; many geniAttVal }
  return (pars, interface)
\end{code}

\section{Morphology}

A morphinfo file associates predicates with morphological feature structures.
Each morphological entry consists of a predicate followed by a feature
structuer.  For more information, see chapter \ref{cha:Morphology}.
(\textbf{TODO}: describe format)

\begin{code}
geniMorphInfo :: Parser [(String,Flist)]
geniMorphInfo = tillEof $ many morphEntry

morphEntry :: Parser (String,Flist)
morphEntry =
  do pred_ <- identifier
     feats <- geniFeats
     return (pred_, feats)
\end{code}

% ======================================================================
% everything else
% ======================================================================

\begin{code}
-- ----------------------------------------------------------------------
-- polarities
-- ----------------------------------------------------------------------
geniPolarities :: Parser (Map.Map PolarityKey Interval)
geniPolarities = tillEof $ toMap `fmap` many pol
  where
    toMap = Map.fromListWith (!+!)
    pol = do p <- geniPolarity
             i <- identifier
             return (PolarityKey i,ival p)

-- | 'geniPolarity' associates a numerical value to a polarity symbol,
--  that is, '+' or '-'.
geniPolarity :: Parser Int
geniPolarity = option 0 (plus <|> minus)
  where
    plus  = do { char '+'; return  1   }
    minus = do { char '-'; return (-1) }

-- ----------------------------------------------------------------------
-- keyword
-- ----------------------------------------------------------------------

{-# INLINE keyword #-}
keyword :: String -> Parser String
keyword k =
  do let helper = try $ do { reserved k; colon; return k }
     helper <?> k ++ ":"

{-# INLINE keywordSemantics #-}
keywordSemantics :: Parser String
keywordSemantics = keyword SEMANTICS

-- ----------------------------------------------------------------------
-- language def helpers
-- ----------------------------------------------------------------------

lexer :: TokenParser ()
lexer  = makeTokenParser geniLanguageDef

whiteSpace :: CharParser () ()
whiteSpace = P.whiteSpace lexer

looseIdentifier, identifier, stringLiteral, colon :: CharParser () String
identifier    = P.identifier lexer

-- stolen from Parsec code (ident)
-- | Like 'identifier' but allows for reserved words too
looseIdentifier =
 do { i <- ident ; whiteSpace; return i }
 where
  ident =
   do { c <- identStart geniLanguageDef
      ; cs <- many (identLetter geniLanguageDef)
      ; return (c:cs) } <?> "identifier"

stringLiteral = P.stringLiteral lexer
colon         = P.colon lexer

squares, braces, parens :: CharParser () a -> CharParser () a
squares = P.squares lexer
braces  = P.braces  lexer
parens  = P.parens  lexer

reserved, symbol :: String -> CharParser () String
reserved s = P.reserved lexer s >> return s
symbol = P.symbol lexer

-- ----------------------------------------------------------------------
-- parsec helpers
-- ----------------------------------------------------------------------

-- | identifier, permitting reserved words too
identifierR :: CharParser () String
identifierR
  = do { c <- P.identStart geniLanguageDef
       ; cs <- many (P.identLetter geniLanguageDef)
       ; return (c:cs)
       }
       <?> "identifier or reserved word"

tillEof :: Parser a -> Parser a
tillEof p =
  do whiteSpace
     r <- p
     eof
     return r

-- stolen from Parsec and adapted to use UTF-8 input
parseFromFile :: Parser a -> SourceName -> IO (Either ParseError a)
parseFromFile p fname
    = do{ input <- UTF8.readFile fname
        ; return (parse p fname input)
        }
\end{code}
