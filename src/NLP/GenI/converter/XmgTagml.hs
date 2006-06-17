{-# OPTIONS_GHC -w #-}
module NLP.GenI.Converter.XmgTagml where

import Text.XML.HaXml.XmlContent
import Text.XML.HaXml.OneOfN


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

instance HTypeable Grammar where
    toHType x = Defined "grammar" [] []
instance XmlContent Grammar where
    toContents (GrammarEntry a) =
        [CElem (Elem "grammar" [] (concatMap toContents a) ) ()]
    toContents (GrammarSubgrammar a) =
        [CElem (Elem "grammar" [] (concatMap toContents a) ) ()]
    parseContents = do 
        { e@(Elem _ [] _) <- element ["grammar"]
        ; interior e $ oneOf
            [ return (GrammarEntry) `apply` many parseContents
            , return (GrammarSubgrammar) `apply` many parseContents
            ] `adjustErr` ("in <grammar>, "++)
        }

instance HTypeable Subgrammar where
    toHType x = Defined "subgrammar" [] []
instance XmlContent Subgrammar where
    toContents (Subgrammar as a) =
        [CElem (Elem "subgrammar" (toAttrs as) (concatMap toContents a)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["subgrammar"]
        ; interior e $ return (Subgrammar (fromAttrs as))
                       `apply` many parseContents
        } `adjustErr` ("in <subgrammar>, "++)
instance XmlAttributes Subgrammar_Attrs where
    fromAttrs as =
        Subgrammar_Attrs
          { subgrammarId = definiteA fromAttrToStr "subgrammar" "id" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "id" (subgrammarId v)
        ]

instance HTypeable Entry where
    toHType x = Defined "entry" [] []
instance XmlContent Entry where
    toContents (Entry as a b c d e) =
        [CElem (Elem "entry" (toAttrs as) (toContents a ++ toContents b ++
                                           toContents c ++ toContents d ++ toContents e)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["entry"]
        ; interior e $ return (Entry (fromAttrs as)) `apply` parseContents
                       `apply` parseContents `apply` parseContents `apply` parseContents
                       `apply` parseContents
        } `adjustErr` ("in <entry>, "++)
instance XmlAttributes Entry_Attrs where
    fromAttrs as =
        Entry_Attrs
          { entryName = definiteA fromAttrToStr "entry" "name" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "name" (entryName v)
        ]

instance HTypeable Family where
    toHType x = Defined "family" [] []
instance XmlContent Family where
    toContents (Family a) =
        [CElem (Elem "family" [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["family"]
        ; interior e $ return (Family) `apply` (text `onFail` return "")
        } `adjustErr` ("in <family>, "++)

instance HTypeable Trace where
    toHType x = Defined "trace" [] []
instance XmlContent Trace where
    toContents (Trace a) =
        [CElem (Elem "trace" [] (concatMap toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["trace"]
        ; interior e $ return (Trace) `apply` many parseContents
        } `adjustErr` ("in <trace>, "++)

instance HTypeable Class where
    toHType x = Defined "class" [] []
instance XmlContent Class where
    toContents (Class a) =
        [CElem (Elem "class" [] (toText a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["class"]
        ; interior e $ return (Class) `apply` (text `onFail` return "")
        } `adjustErr` ("in <class>, "++)

instance HTypeable Tree where
    toHType x = Defined "tree" [] []
instance XmlContent Tree where
    toContents (Tree as a) =
        [CElem (Elem "tree" (toAttrs as) (toContents a)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["tree"]
        ; interior e $ return (Tree (fromAttrs as)) `apply` parseContents
        } `adjustErr` ("in <tree>, "++)
instance XmlAttributes Tree_Attrs where
    fromAttrs as =
        Tree_Attrs
          { treeId = definiteA fromAttrToStr "tree" "id" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "id" (treeId v)
        ]

instance HTypeable Node where
    toHType x = Defined "node" [] []
instance XmlContent Node where
    toContents (Node as a b) =
        [CElem (Elem "node" (toAttrs as) (maybe [] toContents a ++
                                          concatMap toContents b)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["node"]
        ; interior e $ return (Node (fromAttrs as))
                       `apply` optional parseContents `apply` many parseContents
        } `adjustErr` ("in <node>, "++)
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

instance HTypeable Narg where
    toHType x = Defined "narg" [] []
instance XmlContent Narg where
    toContents (Narg a) =
        [CElem (Elem "narg" [] (toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["narg"]
        ; interior e $ return (Narg) `apply` parseContents
        } `adjustErr` ("in <narg>, "++)

instance HTypeable Fs where
    toHType x = Defined "fs" [] []
instance XmlContent Fs where
    toContents (Fs as a) =
        [CElem (Elem "fs" (toAttrs as) (concatMap toContents a)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["fs"]
        ; interior e $ return (Fs (fromAttrs as))
                       `apply` many parseContents
        } `adjustErr` ("in <fs>, "++)
instance XmlAttributes Fs_Attrs where
    fromAttrs as =
        Fs_Attrs
          { fsCoref = possibleA fromAttrToStr "coref" as
          }
    toAttrs v = catMaybes 
        [ maybeToAttr toAttrFrStr "coref" (fsCoref v)
        ]

instance HTypeable F where
    toHType x = Defined "f" [] []
instance XmlContent F where
    toContents (FSym as a) =
        [CElem (Elem "f" (toAttrs as) (toContents a) ) ()]
    toContents (FVAlt as a) =
        [CElem (Elem "f" (toAttrs as) (toContents a) ) ()]
    toContents (FFs as a) =
        [CElem (Elem "f" (toAttrs as) (toContents a) ) ()]
    parseContents = do 
        { e@(Elem _ as _) <- element ["f"]
        ; interior e $ oneOf
            [ return (FSym (fromAttrs as)) `apply` parseContents
            , return (FVAlt (fromAttrs as)) `apply` parseContents
            , return (FFs (fromAttrs as)) `apply` parseContents
            ] `adjustErr` ("in <f>, "++)
        }
instance XmlAttributes F_Attrs where
    fromAttrs as =
        F_Attrs
          { fName = definiteA fromAttrToStr "f" "name" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "name" (fName v)
        ]

instance HTypeable VAlt where
    toHType x = Defined "vAlt" [] []
instance XmlContent VAlt where
    toContents (VAlt as a) =
        [CElem (Elem "vAlt" (toAttrs as) (toContents a)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["vAlt"]
        ; interior e $ return (VAlt (fromAttrs as)) `apply` parseContents
        } `adjustErr` ("in <vAlt>, "++)
instance XmlAttributes VAlt_Attrs where
    fromAttrs as =
        VAlt_Attrs
          { vAltCoref = defaultA fromAttrToStr "" "coref" as
          }
    toAttrs v = catMaybes 
        [ defaultToAttr toAttrFrStr "coref" (vAltCoref v)
        ]

instance HTypeable Sym where
    toHType x = Defined "sym" [] []
instance XmlContent Sym where
    toContents as =
        [CElem (Elem "sym" (toAttrs as) []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["sym"]
        ; return (fromAttrs as)
        } `adjustErr` ("in <sym>, "++)
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

instance HTypeable Semantics where
    toHType x = Defined "semantics" [] []
instance XmlContent Semantics where
    toContents (Semantics a) =
        [CElem (Elem "semantics" [] (concatMap toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["semantics"]
        ; interior e $ return (Semantics) `apply` many parseContents
        } `adjustErr` ("in <semantics>, "++)

instance HTypeable Semantics_ where
    toHType x = Defined "semantics" [] []
instance XmlContent Semantics_ where
    toContents (Semantics_Literal a) = toContents a
    toContents (Semantics_Sym a) = toContents a
    toContents (Semantics_Semdominance a) = toContents a
    parseContents = oneOf
        [ return (Semantics_Literal) `apply` parseContents
        , return (Semantics_Sym) `apply` parseContents
        , return (Semantics_Semdominance) `apply` parseContents
        ] `adjustErr` ("in <semantics>, "++)

instance HTypeable Literal where
    toHType x = Defined "literal" [] []
instance XmlContent Literal where
    toContents (Literal as a b c) =
        [CElem (Elem "literal" (toAttrs as) (maybe [] toContents a ++
                                             toContents b ++ concatMap toContents c)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["literal"]
        ; interior e $ return (Literal (fromAttrs as))
                       `apply` optional parseContents `apply` parseContents
                       `apply` many parseContents
        } `adjustErr` ("in <literal>, "++)
instance XmlAttributes Literal_Attrs where
    fromAttrs as =
        Literal_Attrs
          { literalNegated = defaultA fromAttrToStr "no" "negated" as
          }
    toAttrs v = catMaybes 
        [ defaultToAttr toAttrFrStr "negated" (literalNegated v)
        ]

instance HTypeable Label where
    toHType x = Defined "label" [] []
instance XmlContent Label where
    toContents (Label a) =
        [CElem (Elem "label" [] (toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["label"]
        ; interior e $ return (Label) `apply` parseContents
        } `adjustErr` ("in <label>, "++)

instance HTypeable Predicate where
    toHType x = Defined "predicate" [] []
instance XmlContent Predicate where
    toContents (Predicate a) =
        [CElem (Elem "predicate" [] (toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["predicate"]
        ; interior e $ return (Predicate) `apply` parseContents
        } `adjustErr` ("in <predicate>, "++)

instance HTypeable Arg where
    toHType x = Defined "arg" [] []
instance XmlContent Arg where
    toContents (ArgSym a) =
        [CElem (Elem "arg" [] (toContents a) ) ()]
    toContents (ArgFs a) =
        [CElem (Elem "arg" [] (toContents a) ) ()]
    parseContents = do 
        { e@(Elem _ [] _) <- element ["arg"]
        ; interior e $ oneOf
            [ return (ArgSym) `apply` parseContents
            , return (ArgFs) `apply` parseContents
            ] `adjustErr` ("in <arg>, "++)
        }

instance HTypeable Semdominance where
    toHType x = Defined "semdominance" [] []
instance XmlContent Semdominance where
    toContents (Semdominance as a) =
        [CElem (Elem "semdominance" (toAttrs as) (toContents a)) ()]
    parseContents = do
        { e@(Elem _ as _) <- element ["semdominance"]
        ; interior e $ return (Semdominance (fromAttrs as))
                       `apply` parseContents
        } `adjustErr` ("in <semdominance>, "++)
instance XmlAttributes Semdominance_Attrs where
    fromAttrs as =
        Semdominance_Attrs
          { semdominanceOp = defaultA fromAttrToStr "ge" "op" as
          }
    toAttrs v = catMaybes 
        [ defaultToAttr toAttrFrStr "op" (semdominanceOp v)
        ]

instance HTypeable Interface where
    toHType x = Defined "interface" [] []
instance XmlContent Interface where
    toContents (Interface a) =
        [CElem (Elem "interface" [] (maybe [] toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["interface"]
        ; interior e $ return (Interface) `apply` optional parseContents
        } `adjustErr` ("in <interface>, "++)



{-Done-}
