# Tree schemata

{cha:TreeSchemata}

This module provides basic datatypes specific to Tree Adjoining Grammar
tree schemata.

## Tree schemata

In GenI, the tree schemata are called ‘macros’ for historical reasons.
We are working to phase out this name in favour of the more standard
‘tree schema(ta)’.

## Tree manipulation

### Traversal

### Utility functions

## TAG nodes (GNode)

### Traversal

### Utilities

A TAG node may have a category. In the core GenI algorithm, there is
nothing which distinguishes the category from any other attributes. But
for some other uses, such as checking if it is a result or for display
purposes, we do treat this attribute differently. We take here the
convention that the category of a node is associated to the attribute
“cat”.

A TAG node might also have a lexeme. If we are lucky, this is explicitly
set in the glexeme field of the node. Otherwise, we try to guess it from
a list of distinguished attributes (in order of preference).

### Pretty printing

The default show for GNode tries to be very compact; it only shows the
value for cat attribute and any flags which are marked on that node.

### Fancy disjunction
