---
title: Lexical filtering 
---

Lexical filtering is a feature which allows the user to constrain the
lexical selection to only those items that contain a certain property,
for example, the realising an item as a cleft.

The idea is that the user provides an input like
`idxconstraints:[cleft:j]`, which means that the lexical selection must
include exactly one tree with the property cleft:j in its interface.
This mechanism works as pre-processing step after lexical selection and
before polarity automaton construction, in conjuction with the
ExtraPolarities mechanism. What we do is

1.  Preprocess the lexically selected trees; any tree which has a a
    desired property (e.g. `cleft:j`) in its interface is assigned a
    positive polarity for that property (`+cleft:j`)

2.  Add all the index constraints as negative extra polarities
    (`-cleft:j`)

Note: To use lexical filtering, [polarity filtering](polarity.html)
must be enabled.
