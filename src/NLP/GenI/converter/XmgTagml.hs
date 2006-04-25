{-# OPTIONS_GHC -w #-}

module NLP.GenI.Converter.XmgTagml where

import Text.XML.HaXml.Xml2Haskell
import Text.XML.HaXml.OneOfN
import Char (isSpace)


{-Type decls-}

data Grammar = GrammarEntry [Entry]
             | GrammarSubgrammar [Subgrammar]
             deriving (Eq,Show)
data Subgrammar = Subgrammar Subgrammar_Attrs [Entry]
                deriving (Eq,Show)
data Subgrammar_Attrs = Subgrammar_Attrs
    { subgrammarId :: String
    } deriving (Eq,Show)
data Entry = Entry Entry_Attrs Family Trace Tree Semantics
                   Interface
           deriving (Eq,Show)
data Entry_Attrs = Entry_Attrs
    { entryName :: String
    } deriving (Eq,Show)
newtype Family = Family String 		deriving (Eq,Show)
newtype Trace = Trace [Class] 		deriving (Eq,Show)
newtype Class = Class String 		deriving (Eq,Show)
data Tree = Tree Tree_Attrs Node
          deriving (Eq,Show)
data Tree_Attrs = Tree_Attrs
    { treeId :: String
    } deriving (Eq,Show)
data Node = Node Node_Attrs (Maybe Narg) [Node]
          deriving (Eq,Show)
data Node_Attrs = Node_Attrs
    { nodeType :: Node_type
    , nodeName :: (Maybe String)
    } deriving (Eq,Show)
data Node_type = Node_type_nadj  |  Node_type_std  | 
                 Node_type_subst  |  Node_type_lex  |  Node_type_anchor  | 
                 Node_type_coanchor  |  Node_type_foot
               deriving (Eq,Show)
newtype Narg = Narg Fs 		deriving (Eq,Show)
data Fs = Fs Fs_Attrs [F]
        deriving (Eq,Show)
data Fs_Attrs = Fs_Attrs
    { fsCoref :: (Maybe String)
    } deriving (Eq,Show)
data F = FSym F_Attrs Sym
       | FVAlt F_Attrs VAlt
       | FFs F_Attrs Fs
       deriving (Eq,Show)
data F_Attrs = F_Attrs
    { fName :: String
    } deriving (Eq,Show)
data VAlt = VAlt VAlt_Attrs (List1 Sym)
          deriving (Eq,Show)
data VAlt_Attrs = VAlt_Attrs
    { vAltCoref :: (Defaultable String)
    } deriving (Eq,Show)
data Sym = Sym
    { symValue :: (Maybe String)
    , symVarname :: (Maybe String)
    } deriving (Eq,Show)
newtype Semantics = Semantics [Semantics_] 		deriving (Eq,Show)
data Semantics_ = Semantics_Literal Literal
                | Semantics_Sym Sym
                | Semantics_Semdominance Semdominance
                deriving (Eq,Show)
data Literal = Literal Literal_Attrs (Maybe Label) Predicate [Arg]
             deriving (Eq,Show)
data Literal_Attrs = Literal_Attrs
    { literalNegated :: (Defaultable String)
    } deriving (Eq,Show)
newtype Label = Label Sym 		deriving (Eq,Show)
newtype Predicate = Predicate Sym 		deriving (Eq,Show)
data Arg = ArgSym Sym
         | ArgFs Fs
         deriving (Eq,Show)
data Semdominance = Semdominance Semdominance_Attrs (List1 Arg)
                  deriving (Eq,Show)
data Semdominance_Attrs = Semdominance_Attrs
    { semdominanceOp :: (Defaultable String)
    } deriving (Eq,Show)
newtype Interface = Interface (Maybe Fs) 		deriving (Eq,Show)


{-Instance decls-}

instance XmlContent Grammar where
    fromElem (CElem (Elem "grammar" [] c0):rest) =
        case (many fromElem c0) of
        (a,_) -> (Just (GrammarEntry a), rest)
        ([],_) ->
                case (many fromElem c0) of
                (a,_) -> (Just (GrammarSubgrammar a), rest)
                ([],_) ->
                    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (GrammarEntry a) = [CElem (Elem "grammar" [] (concatMap toElem a) )]
    toElem (GrammarSubgrammar a) = [CElem (Elem "grammar" [] (concatMap toElem a) )]
instance XmlContent Subgrammar where
    fromElem (CElem (Elem "subgrammar" as c0):rest) =
        (\(a,ca)->
           (Just (Subgrammar (fromAttrs as) a), rest))
        (many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Subgrammar as a) =
        [CElem (Elem "subgrammar" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes Subgrammar_Attrs where
    fromAttrs as =
        Subgrammar_Attrs
          { subgrammarId = definiteA fromAttrToStr "subgrammar" "id" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "id" (subgrammarId v)
        ]
instance XmlContent Entry where
    fromElem (CElem (Elem "entry" as c0):rest) =
        (\(a,ca)->
           (\(b,cb)->
              (\(c,cc)->
                 (\(d,cd)->
                    (\(e,ce)->
                       (Just (Entry (fromAttrs as) a b c d e), rest))
                    (definite fromElem "<interface>" "entry" cd))
                 (definite fromElem "<semantics>" "entry" cc))
              (definite fromElem "<tree>" "entry" cb))
           (definite fromElem "<trace>" "entry" ca))
        (definite fromElem "<family>" "entry" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Entry as a b c d e) =
        [CElem (Elem "entry" (toAttrs as) (toElem a ++ toElem b ++ toElem c
                                           ++ toElem d ++ toElem e))]
instance XmlAttributes Entry_Attrs where
    fromAttrs as =
        Entry_Attrs
          { entryName = definiteA fromAttrToStr "entry" "name" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "name" (entryName v)
        ]
instance XmlContent Family where
    fromElem (CElem (Elem "family" [] c0):rest) =
        (\(a,ca)->
           (Just (Family a), rest))
        (definite fromText "text" "family" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Family a) =
        [CElem (Elem "family" [] (toText a))]
instance XmlContent Trace where
    fromElem (CElem (Elem "trace" [] c0):rest) =
        (\(a,ca)->
           (Just (Trace a), rest))
        (many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Trace a) =
        [CElem (Elem "trace" [] (concatMap toElem a))]
instance XmlContent Class where
    fromElem (CElem (Elem "class" [] c0):rest) =
        (\(a,ca)->
           (Just (Class a), rest))
        (definite fromText "text" "class" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Class a) =
        [CElem (Elem "class" [] (toText a))]
instance XmlContent Tree where
    fromElem (CElem (Elem "tree" as c0):rest) =
        (\(a,ca)->
           (Just (Tree (fromAttrs as) a), rest))
        (definite fromElem "<node>" "tree" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Tree as a) =
        [CElem (Elem "tree" (toAttrs as) (toElem a))]
instance XmlAttributes Tree_Attrs where
    fromAttrs as =
        Tree_Attrs
          { treeId = definiteA fromAttrToStr "tree" "id" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "id" (treeId v)
        ]
instance XmlContent Node where
    fromElem (CElem (Elem "node" as c0):rest) =
        (\(a,ca)->
           (\(b,cb)->
              (Just (Node (fromAttrs as) a b), rest))
           (many fromElem ca))
        (fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Node as a b) =
        [CElem (Elem "node" (toAttrs as) (maybe [] toElem a ++
                                          concatMap toElem b))]
instance XmlAttributes Node_Attrs where
    fromAttrs as =
        Node_Attrs
          { nodeType = definiteA fromAttrToTyp "node" "type" as
          , nodeName = possibleA fromAttrToStr "name" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrTyp "type" (nodeType v)
        , maybeToAttr toAttrFrStr "name" (nodeName v)
        ]
instance XmlAttrType Node_type where
    fromAttrToTyp n (n',v)
        | n==n'     = translate (attr2str v)
        | otherwise = Nothing
      where translate "nadj" = Just Node_type_nadj
            translate "std" = Just Node_type_std
            translate "subst" = Just Node_type_subst
            translate "lex" = Just Node_type_lex
            translate "anchor" = Just Node_type_anchor
            translate "coanchor" = Just Node_type_coanchor
            translate "foot" = Just Node_type_foot
            translate _ = Nothing
    toAttrFrTyp n Node_type_nadj = Just (n, str2attr "nadj")
    toAttrFrTyp n Node_type_std = Just (n, str2attr "std")
    toAttrFrTyp n Node_type_subst = Just (n, str2attr "subst")
    toAttrFrTyp n Node_type_lex = Just (n, str2attr "lex")
    toAttrFrTyp n Node_type_anchor = Just (n, str2attr "anchor")
    toAttrFrTyp n Node_type_coanchor = Just (n, str2attr "coanchor")
    toAttrFrTyp n Node_type_foot = Just (n, str2attr "foot")
instance XmlContent Narg where
    fromElem (CElem (Elem "narg" [] c0):rest) =
        (\(a,ca)->
           (Just (Narg a), rest))
        (definite fromElem "<fs>" "narg" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Narg a) =
        [CElem (Elem "narg" [] (toElem a))]
instance XmlContent Fs where
    fromElem (CElem (Elem "fs" as c0):rest) =
        (\(a,ca)->
           (Just (Fs (fromAttrs as) a), rest))
        (many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Fs as a) =
        [CElem (Elem "fs" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes Fs_Attrs where
    fromAttrs as =
        Fs_Attrs
          { fsCoref = possibleA fromAttrToStr "coref" as
          }
    toAttrs v = catMaybes 
        [ maybeToAttr toAttrFrStr "coref" (fsCoref v)
        ]
instance XmlContent F where
    fromElem (CElem (Elem "f" as c0):rest) =
        case (fromElem c0) of
        (Just a,_) -> (Just (FSym (fromAttrs as) a), rest)
        (_,_) ->
                case (fromElem c0) of
                (Just a,_) -> (Just (FVAlt (fromAttrs as) a), rest)
                (_,_) ->
                        case (fromElem c0) of
                        (Just a,_) -> (Just (FFs (fromAttrs as) a), rest)
                        (_,_) ->
                            (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (FSym as a) = [CElem (Elem "f" (toAttrs as) (toElem a) )]
    toElem (FVAlt as a) = [CElem (Elem "f" (toAttrs as) (toElem a) )]
    toElem (FFs as a) = [CElem (Elem "f" (toAttrs as) (toElem a) )]
instance XmlAttributes F_Attrs where
    fromAttrs as =
        F_Attrs
          { fName = definiteA fromAttrToStr "f" "name" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "name" (fName v)
        ]
instance XmlContent VAlt where
    fromElem (CElem (Elem "vAlt" as c0):rest) =
        (\(a,ca)->
           (Just (VAlt (fromAttrs as) a), rest))
        (definite fromElem "sym+" "vAlt" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (VAlt as a) =
        [CElem (Elem "vAlt" (toAttrs as) (toElem a))]
instance XmlAttributes VAlt_Attrs where
    fromAttrs as =
        VAlt_Attrs
          { vAltCoref = defaultA fromAttrToStr "" "coref" as
          }
    toAttrs v = catMaybes 
        [ defaultToAttr toAttrFrStr "coref" (vAltCoref v)
        ]
instance XmlContent Sym where
    fromElem (CElem (Elem "sym" as []):rest) =
        (Just (fromAttrs as), rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem as =
        [CElem (Elem "sym" (toAttrs as) [])]
instance XmlAttributes Sym where
    fromAttrs as =
        Sym
          { symValue = possibleA fromAttrToStr "value" as
          , symVarname = possibleA fromAttrToStr "varname" as
          }
    toAttrs v = catMaybes 
        [ maybeToAttr toAttrFrStr "value" (symValue v)
        , maybeToAttr toAttrFrStr "varname" (symVarname v)
        ]
instance XmlContent Semantics where
    fromElem (CElem (Elem "semantics" [] c0):rest) =
        (\(a,ca)->
           (Just (Semantics a), rest))
        (many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Semantics a) =
        [CElem (Elem "semantics" [] (concatMap toElem a))]
instance XmlContent Semantics_ where
    fromElem c0 =
        case (fromElem c0) of
        (Just a,rest) -> (Just (Semantics_Literal a), rest)
        (_,_) ->
                case (fromElem c0) of
                (Just a,rest) -> (Just (Semantics_Sym a), rest)
                (_,_) ->
                        case (fromElem c0) of
                        (Just a,rest) -> (Just (Semantics_Semdominance a), rest)
                        (_,_) ->
                            (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Semantics_Literal a) = toElem a
    toElem (Semantics_Sym a) = toElem a
    toElem (Semantics_Semdominance a) = toElem a
instance XmlContent Literal where
    fromElem (CElem (Elem "literal" as c0):rest) =
        (\(a,ca)->
           (\(b,cb)->
              (\(c,cc)->
                 (Just (Literal (fromAttrs as) a b c), rest))
              (many fromElem cb))
           (definite fromElem "<predicate>" "literal" ca))
        (fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Literal as a b c) =
        [CElem (Elem "literal" (toAttrs as) (maybe [] toElem a ++ toElem b
                                             ++ concatMap toElem c))]
instance XmlAttributes Literal_Attrs where
    fromAttrs as =
        Literal_Attrs
          { literalNegated = defaultA fromAttrToStr "no" "negated" as
          }
    toAttrs v = catMaybes 
        [ defaultToAttr toAttrFrStr "negated" (literalNegated v)
        ]
instance XmlContent Label where
    fromElem (CElem (Elem "label" [] c0):rest) =
        (\(a,ca)->
           (Just (Label a), rest))
        (definite fromElem "<sym>" "label" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Label a) =
        [CElem (Elem "label" [] (toElem a))]
instance XmlContent Predicate where
    fromElem (CElem (Elem "predicate" [] c0):rest) =
        (\(a,ca)->
           (Just (Predicate a), rest))
        (definite fromElem "<sym>" "predicate" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Predicate a) =
        [CElem (Elem "predicate" [] (toElem a))]
instance XmlContent Arg where
    fromElem (CElem (Elem "arg" [] c0):rest) =
        case (fromElem c0) of
        (Just a,_) -> (Just (ArgSym a), rest)
        (_,_) ->
                case (fromElem c0) of
                (Just a,_) -> (Just (ArgFs a), rest)
                (_,_) ->
                    (Nothing, c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (ArgSym a) = [CElem (Elem "arg" [] (toElem a) )]
    toElem (ArgFs a) = [CElem (Elem "arg" [] (toElem a) )]
instance XmlContent Semdominance where
    fromElem (CElem (Elem "semdominance" as c0):rest) =
        (\(a,ca)->
           (Just (Semdominance (fromAttrs as) a), rest))
        (definite fromElem "arg+" "semdominance" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Semdominance as a) =
        [CElem (Elem "semdominance" (toAttrs as) (toElem a))]
instance XmlAttributes Semdominance_Attrs where
    fromAttrs as =
        Semdominance_Attrs
          { semdominanceOp = defaultA fromAttrToStr "ge" "op" as
          }
    toAttrs v = catMaybes 
        [ defaultToAttr toAttrFrStr "op" (semdominanceOp v)
        ]
instance XmlContent Interface where
    fromElem (CElem (Elem "interface" [] c0):rest) =
        (\(a,ca)->
           (Just (Interface a), rest))
        (fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Interface a) =
        [CElem (Elem "interface" [] (maybe [] toElem a))]


{-Done-}
