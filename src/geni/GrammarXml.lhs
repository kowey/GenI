\chapter{XML Parser}

A simple DOM-like parser (using HaXml) for grammars in XML format.
This produces a set of trees indexed by their name.

\begin{code}
module GrammarXml where
\end{code}

\ignore{
\begin{code}
import Data.Char
import Data.FiniteMap (FiniteMap,emptyFM,addToFM_C)
import Data.List (partition,sort)
import Data.Tree
import MonadState (State, 
                   runState,
                   get, 
                   put)
import Text.XML.HaXml.Types
import Text.XML.HaXml.Combinators
import Text.XML.HaXml.Parse

import Btypes(AvPair, Flist, ILexEntry(..), 
              GType(Subs,Foot,Lex,Other),
              GNode(..), Macros, Ttree(..),
              emptyGNode, emptyMacro,
              Ptype(..), Pred)
import PolParser(polParser)
import Lex2(lexer)
-- import Tags(emptyTE,TagElem(..),Tags,TagSite,addToTags)
\end{code}
}

% ======================================================================
\section{Lexicon}
% ======================================================================

A lexicon associates some lemma with a tree family.
FIXME: For the moment, we do not handle coanchors!  We actually
drop a good deal of the information that is the lexicon.

\begin{code}
parseXmlLexicon :: String -> [ILexEntry]
parseXmlLexicon g = 
  -- extract a CElem out of the String
  let (Document _ _ ele) = xmlParse "" g 
      c = CElem ele
      -- processing phase
      lexF = tag "tagml" /> tag "lexicalization"
      lex  = lexF c
  in map parseLex lex
\end{code}

Lexical entries can be really fancy.  Each lexical entry looks 
a little like this:

\begin{verbatim}
<lexicalization>
      <tree>
        <fs>
          <f name="family">
            <sym value="commonnoun"/></f></fs></tree>
      <anchor noderef="anchor">
        <lemmaref name="agneau" cat="n"/></anchor></lexicalization>
\end{verbatim}

From the above piece of XML, we would extract the following
information: family = commonnoun, anchor = agneau

\begin{code}
parseLex :: Content -> ILexEntry
parseLex l = 
  let -- getting the family name 
      lFeatsF = keep /> tag "tree" /> featStructF /> featF
      feats   = map parseFeature (lFeatsF l)
      famFeats = filter (\ (a,_) -> a == "family") feats
      fam = if null famFeats 
            then "UNKWNOWN" 
            else (snd.head) famFeats
      -- getting the lemma 
      lemmaF = attributed "name" (keep /> tag "anchor")
      lemma  = concatMap fst (lemmaF l) -- should only be one element 
      -- creating a lexical entry: note that we leave the
      -- semantics empty; this will have to be read from 
      -- another file
  in ILE{ iword = lemma
        , itreename = fam
        , iparams = []
        , ipfeat = []
        , iptype = Unspecified
        , isemantics = []
        , ipredictors = []
  }
\end{code}


% ======================================================================
\section{Macros}
% ======================================================================

\begin{code}
type MTree = Ttree GNode

parseXmlGrammar :: String -> Macros
parseXmlGrammar g = 
  -- extract a CElem out of the String
  let (Document _ _ ele) = xmlParse "" g 
      c = CElem ele
      -- processing phase
      entriesF = tag "grammar" /> tag "entry"
      entries  = entriesF c
      --
      res = foldr parseEntry emptyFM entries
  in res

parseEntry :: Content -> Macros -> Macros 
parseEntry e mac =
  let synF = keep /> tag "tree"    
      trcF = keep /> tag "trace"    
      intF = keep /> tag "interface"
      -- litF = keep /> tag "semantics" /> tag "literal"
      -- sem  = map parseLiteral (litF e)
      syn  = synF e
      trc  = trcF e
      int  = intF e
      -- read the tree name 
      nameF = attributed "name" keep
      name  = concatMap fst (nameF e) -- should only be one element 
      -- read the tree family name 
      famNameF = keep /> tag "family" /> txt 
      famName  = unwrap (famNameF e) -- should only be one element 
      -- build the tree 
      t = t2 { ptpolarities = if null trc 
                              then emptyFM 
                              else parseTrace (head trc) 
             -- there should only be one interface though
             , pidname = name
             , params  = fst pf 
             , pfeat   = snd pf
             }
          where t2 = if null syn 
                     then emptyMacro 
                     else parseTree (head syn)
                pf = if null int 
                     then ([],[])
                     else parseInterface (head int)
  in addToFM_C (++) mac famName [t]
\end{code}



% ----------------------------------------------------------------------
\subsection{Syntax}
% ----------------------------------------------------------------------

Below, we're going need to use a state monad to keep track of some stuff like
node numbering, the tree type, etc.  Here will be the contents of the
state.

\begin{code}
data TreeInfo = TI {
  -- tiAdjnodes :: [TagSite],
  -- tiSubstnodes :: [TagSite],
  tiNum     :: Int,
  -- tiLex     :: String,
  tiHasFoot :: Bool
} 
\end{code}

Tree parsing consists of building up the tree with the recursive
function parseNode, and then extracting (from the State monad)
some information which is global to the tree.

\begin{code}
parseTree :: Content -> MTree 
parseTree t = 
  let nodeF  = keep /> tag "node"
      node   = nodeF t
      initTi = TI { tiNum = 0, tiHasFoot = False }
                    -- tiAdjnodes = [], tiSubstnodes = [] }
      (parsedTr,finalTi)  = runState (parseNode $ head node) initTi
      (tr,info) = if null node  
                  then (Node emptyGNode [], initTi) 
                  else (parsedTr, finalTi)
  in emptyMacro { tree = tr
                , ptype = if (tiHasFoot info) then Auxiliar else Initial
                }
\end{code}

We recurse through a basic tree structure to build the TAG tree.  Nodes 
have children which are also nodes.  The structure is something like this:

\begin{verbatim}
<node type="none">
  <narg>
  <fs>
    <f name="cat">
      ...
    </f>
    <feature name="top"><avm>
        ...
    </avm></feature>
  <fs>
  </narg>
  <node mark="subst">  <-- RECURSION HERE
       ...
  </node>
</node>
\end{verbatim}

The annoying thing is the features.  The MG builds recursive feature
structures, where the top and bottom features are substructures of 
the global fs.  GenI, on the other hand, assumes two flat feature 
lists, top and bottom.   We work around this by simply assuming that
the fs recursion never goes further than that one level, and that 
global features belong to both the top and bottom lists.

\begin{code}
parseNode :: Content -> State TreeInfo (Tree GNode)
parseNode n = do
  st <- get
  let -- the fs parent structure 
      wholeF = keep /> tag "narg" /> featStructF /> featF
      -- detecting top, bottom and global features
      makeAttrF v = attrval ("name", AttValue [Left v])
      topAttrF = makeAttrF "top"
      botAttrF = makeAttrF "bot"
      topF = subFeatF topAttrF
      botF = subFeatF botAttrF 
      subFeatF av = (av `o` wholeF) /> featStructF /> featF
      -- global features are neither top nor bottom
      gloF = (wholeF `without` topAttrF) `without`  botAttrF
      -- saving the feature lists 
      topFl  = map parseFeature (topF n)
      botFl  = map parseFeature (botF n)
      gloFl  = map parseFeature (gloF n)
      -- reading the node type
      ntypeF    = attributed "type" keep
      ntypeStr  = concatMap fst (ntypeF n) -- should only be one element 
      ntype     = case ntypeStr of 
                    "subst"  -> Subs
                    "foot"   -> Foot
                    "anchor" -> Lex
                    _        -> Other
      {- 
      -- the cat feature
      isLexeme (a,_) = a == "phon"
      (lexL, gloFl)  = partition isLexeme gloFl'
      (ntype, lex)   = if null lexL 
                       then (ntype', "") 
                       else (Lex   , snd $ head lexL) -}
      -- FIXME: explicit constraints need to be accounted for
      aconstr  = (ntype == Subs || ntype == Foot)
      -- the node name is just the counter
      name = show $ tiNum st
      -- saving the results in a Gnode
      gn = GN { gnname  = name,
                gup     = sort $ topFl ++ gloFl,
                gdown   = sort $ botFl ++ gloFl,
                ganchor = (ntype == Lex),
                glexeme = "",
                gtype   = ntype,
                gaconstr = aconstr }
      {-
      -- add to the list of substitution and adjunction nodes as needed
      snodes' = tiSubstnodes st
      anodes' = tiAdjnodes st
      site   = (name, gup gn, gdown gn)
      snodes = if (ntype == Subs) then (site:snodes') else snodes' 
      anodes = if aconstr then anodes' else (site:anodes')
      -}
  -- update the monadic state
  let st2 = st { tiNum = (tiNum st) + 1,
                 -- tiLex = if ntype == Lex then lex else (tiLex st),
                 tiHasFoot = (tiHasFoot st) || (ntype == Foot) }
                 {- ,
                 tiAdjnodes = anodes,
                 tiSubstnodes = snodes }
                 -}
  put st2
  -- recursion to the kids  
  let kidsF  = keep /> tag "node"
  kids <- mapM parseNode (kidsF n)
  -- output the node
  return (Node gn kids)
\end{code}

% ----------------------------------------------------------------------
\subsection{Semantics}
% ----------------------------------------------------------------------

We parse each literal in the tree semantics separately.  Note the
case-conversion for labels.  This is because labels are assumed to
be constants.  We discriminate between arguments of the form
\verb$<const>Foo</const>$ or \verb$<var>Bar</var>$ in the same way as
parseFeatVal above.

\begin{code}
parseLiteral :: Content -> Pred 
parseLiteral lit =
  let labelF    = children `o` (keep /> tag "label")
      predF     = children `o` (keep /> tag "predicate")
      label     = (toUpperHead . concatParseSym . labelF) lit
      predicate = (concatParseSym . predF) lit
      concatParseSym = concatMap parseSym -- assumes a singleton list
      -- arguments
      argsF     = children `o` (keep /> tag "arg") 
      arguments = map parseSym (argsF lit)
  in (label, predicate, arguments)
\end{code}

% ----------------------------------------------------------------------
\subsection{Interface}
% ----------------------------------------------------------------------

The interface is the mechanism which allows us to perform lexicalisaiton
and to instantiate the semantic indices of a tree.  For example, a tree
like \verb$S(N [idx:X], V, N [idx:Y])$ could have an interface
\verb$[arg0:X, arg1:Y]$.  In order to instantiate the tree with the
semantics \texttt{hates(h,m,j)}, we could set \verb$X$ to \texttt{m} and
\verb$Y$ to j.

In the TAGMLish format, the interface takes the form of a feature
structure.  Following the current GenI design, we seperate the interface
into a list of parameters and tree features.  For example, in the
interface below, the parameters would be \verb$[?A,?I]$ and the features
would be 
\fs{\it anch:manger\\ \it obj:?I\\  \it suj:?A\\}

\begin{verbatim}
<interface>
  <fs>
    <f name="anch"><sym value="manger"/></f>
    <f name="arg0"><sym varname="@A"/></f>
    <f name="arg1"><sym varname="@I"/></f>
    <f name="obj"> <sym varname="@I"/></f>
    <f name="suj"> <sym varname="@A"/></f>
  </fs>
</interface>
\end{verbatim}

\begin{code}
parseInterface :: Content -> ([String], Flist)
parseInterface int =
  let iFeatsF  = keep /> featStructF /> featF
      feats    = map parseFeature (iFeatsF int)
      --
      (args,noargs) = partition fn feats 
                      where fn (a,_) = take 3 a == "arg" 
      --
      params   = (snd.unzip) args
  in (params, noargs)
\end{code}


% ----------------------------------------------------------------------
\subsection{Trace}
% ----------------------------------------------------------------------

Normally the trace is meant to keep a record of the classes that are 
used to build a grammatical tree.  We hijack the trace to encode extra
information, namely polarities for optimisation.  Each class is 
associated with a list of polarities; the polarities of a tree is the
concatenation of all polarities of all its classes.  The list is written
as an underscore-delimeted suffix to the class name, for instance,
\verb$subjCan_+np_+vp$, meaning that class subjCan has polarities
\verb$[+np,+vp]$.  There should be no underscores in the class name.

\begin{code}
parseTrace :: Content -> FiniteMap String Int
parseTrace tr = 
  let classF  = keep /> tag "class" /> keep
      classes = map (unwrap.txt) $ classF tr
      --
      isDelim  = (== '_')
      words_ []  = []
      words_ l   = a : (if null b then [] else (words_ $ drop 1 b))
                   where (a,b) = break isDelim l
      --
      dropName = (drop 1) . words_
  in (polParser.lexer) $ unwords $ concatMap dropName classes 
\end{code}

% ----------------------------------------------------------------------
\section{XML Content to Haskell}
% ----------------------------------------------------------------------

Turns a Content into a String.  Returns empty string if the Content is
not a CString

\begin{code}
unwrap :: [Content] -> String
unwrap [(CString _ c)] = c          
unwrap _ = ""
\end{code}

% ----------------------------------------------------------------------
\section{Miscellaneous}
% ----------------------------------------------------------------------

\paragraph{XML snippets}

We collect bits and pieces of the XML format as global functions for
use throughout the code.

\begin{code}
featStructF = tag "fs"
featF       = tag "f"
\end{code}

\paragraph{parseFeature} Extracts a an attribute-value pair (att,value) out of
the XML.

\begin{verbatim}
<f name="att">
  <sym varname="value"/>
</f>
\end{verbatim}

Note: GenI does \emph{not} handle atomic disjunctions, so we simply
treat them as variable values by ignoring the sym tags and extracting
the variable name from the coref 
(Yannick Parmetier says that atomic disjunctions always have one).
Disjunctions look like this in the XML:

\begin{verbatim}
<vAlt coref="@X">
 <sym value="foo"/>
 <sym value="bar"/>
</vAlt>
\end{verbatim}
\begin{code}
parseFeature :: Content -> AvPair
parseFeature f =
  let -- parsing the attribute
      -- the TAG fs attribute is expressed as the value of XML attribute "name"
      subfeatF = attributed "name" keep
      feat     = concatMap fst (subfeatF f) -- should only be one element 
      -- parsing the value
      symF  = keep /> tag "sym"
      disjF = attributed "coref" (keep /> tag "vAlt")
      -- converting the value to GenI format
      readAttr fn = concatMap fst (fn f)
      disjStr     = (toUpperHead . drop 1 . readAttr) disjF
      -- deciding what type of feature we have
      val      = if null disjStr 
                 then concatMap parseSym (symF f) -- singleton list 
                 else disjStr
  in (feat, val)
\end{code}

\paragraph{parseSym} converts sym tags into GenI constants or variables.
GenI currently relies on a convention where upper-case is for variables
and lower for constants\footnote{naturally we assume the metagrammar
output is case-insensitive}.  

\begin{enumerate}
\item constants are of the form \verb$<sym value="foo"/>$.  
      We lower-case the first letter.
\item variables are of the form \verb$<sym varname="@Bar"/>$.
      We strip off the initial @ and upper-case the first letter of.
\end{enumerate}

Note: the tagname does not neccesarily have to be sym.

\begin{code}
parseSym :: Content -> String 
parseSym s =
  let -- parsing the value
      varF     = varAttrF keep 
      varAttrF = attributed "varname"
      constF   = attributed "value" keep 
      -- converting the value to GenI format
      readAttr fn = concatMap fst (fn s)
      varStr   = (toUpperHead . drop 1 . readAttr) varF 
      constStr = (toLowerHead . readAttr) constF
  in  -- deciding what type of feature we have
      if null varStr then constStr else varStr
\end{code}

\paragraph{toUpperHead and toLowerHead} make the first character of a
string upper and lower case, respectively.  

\begin{code}
toUpperHead []    = []
toUpperHead (h:t) = (toUpper h):t
toLowerHead []    = []
toLowerHead(h:t)  = (toLower h):t
\end{code}

%\paragraph{ditchAccents} is a stupid hack to strip off the accents
%of a string because WxWidgets (or something else) chokes whilst
%trying to display them in Linux.
