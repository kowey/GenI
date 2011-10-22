---
title: Feature structures
---

A GenI feature structure is a mapping from attributes to values.

Feature structures in GenI are flat, in other words not recursive;
however, feature structures may appear as part of some larger
structure (eg. a TAG tree, or a lexical entry). The scope of
the unification variables is over the whole structure.

## Feature structure unification

Feature structure unification is a process which merges two
feature structures.  Any substitutions that result unification 
are percolated throughout the feature structure's parent
(eg. the elementary tree in which it appears).

Unification fails if, at any point during the unification process, the
two lists have different constant values for the same attribute. For
example, unification fails on the following inputs because they have
different values for the *number* attribute:

      [ cat:np number:3 ]
    ⊔ [ cat:np number:2 ]

Note that the following input should also fail as a result on the
coreference on *?X*.

      [ cat:np  one: 1   two:2  ]
    ⊔ [ cat:np  one: ?X  two:?X ]

On the other hand, any other pair of feature lists should unify
succesfully, even those that do not share the same attributes. Below are
some examples of successful unifications:

      [ cat:np one: 1   two:2  ]
    ⊔ [ cat:np one: ?X  two:?Y ]
    → [ cat:np one: 1   two:2  ]


      [ cat:np          number:3 ]
    ⊔ [ cat:np case:nom          ]
    → [ cat:np case:nom number:3 ]
