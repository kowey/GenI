---
title: Paraphrase selection
---

Paraphrase selection can largely be performed by constraining the
lexical selection. We will extend the lexical selection process and the
input semantics to express such constraints, but before going into the
details, let us mock up a small usage scenario. Consider the input
semantics below. We will imagine a hypothetical grammar which associates
it with three paraphrases:

> `l1:give(e,j,b,m), l2:john(j), l3:mary(m)}`

- *John gave the book to Mary.*
- *Mary was given the book by John.*
- *The book was given to Mary by John.*

To restrict the selection, we
might specify that the literal {l1:give(e,j,b,m)} be realised by a verb
in the passive voice. We rewrite the input semantics as follows with the
expected effects:

> `l1:give(e,j,b,m)[PassiveForm], l2:john(j), l3:mary(m)`

- *~~John gave the book to Mary.~~*
- *Mary was given the book by John.*
- *The book was given to Mary by John.*

Adding more properties to the input semantics simply narrows down the
resulting output even more. We restrict the literal further still:

> `l1:give(e,j,b,m)[PassiveForm, CanonicalToObject], l2:john(j), l3:mary(m)`

- *~~John gave the book to Mary.~~*
- *~~Mary was given the book by John.~~*
- *The book was given to Mary by John.*

These tokens with which we enrich the input semantics are called 
**tree properties**

We will now see exactly [how they are used](#enriched-stuff)
and and [where they come from](#tree-properties) in practice

## Enriched lexical items and input semantics
<a name="enriched-stuff"/>

The extensions require that we introduce enriched versions of the
lexicon and the input semantics, both taking tree properties into
account. The basic idea is that linguists use tree properties to
describe lexical items and the surface realiser uses them to filter the
lexical selection.

- A **tree property** is an identifier. Some examples of tree property
  are `PassiveForm` and `CanonicalToObject`.

- An **enriched lexical item** is a triple $〈T,S,LTP〉$. $T$ and $S$
  are the elementary tree and lexical semantics.  $LTP$ is a
  set of tree properties.

- An **enriched input semantics** is a set of enriched literals of the
  form $L[tp_1,\ldots,tp_n]$, <!-- _ -->
  where $L$ is a saturated literal and
  $tp_1,\ldots,tp_n$ <!-- _ -->
  is a possibly empty set of tree properties. As a
  notational convenience, we omit the square brackets when the set of
  tree properties is empty.

- The **plain input semantics** is the result of stripping away all the
  tree properties from an enriched semantics. Given an enriched input
  semantics $ES$ we say that the plain input semantics is the set of
  literals of the form $L_i$ where $L_i[tp_1,\ldots,tp_n] \in ES$
  <!-- _ -->

## Enriched lexical selection
<a name="enriched-stuff"/>

Taking tree properties into account consists of filtering the lexical
items so that only those with the desired tree properties are retained.
Given an enriched semantics $ES$, we instantiate the lexicon as usual
(see [lexical selection](lexical-selection.html) and return the set of
enriched lexical items such that for each item $〈T,S,LTP〉$:

-   (as usual) its instantiated semantics $S$ is non-empty and subsumes
    the plain input semantics;

-   for every enriched literal $L[tp_1,\ldots,tp_n]$ in the enriched
    <!-- _ -->
    input semantics $ES$, if $L \in S$ then
    $〈tp_1,\ldots,tp_n〉\subseteq LTP$.
    <!-- _ -->

## Where tree properties come from
<a name="tree-properties"/>

The tree selection mechanism requires that every lexical item in the
grammar “possess”, i.e. be associated with, a set of tree properties.
There are many ways of achieving this intermediate goal. One possible
solution might be manual annotation, but this is neither desirable nor
necessary. It is undesirable because realistic TAG grammars are large
enough to make such a process error-prone and cumbersome. It is
unnecessary in the case of grammars built from the XMG metagrammar
compiler because the annotations are already encoded in another
resource, the metagrammar from which it was compiled. See the XMG
documentation for more details.

## Producing at most one output
<a name="one-result"/>

The more tree properties used to enrich the input semantics, the fewer
lexical items are selected. Pushing this to its logical conclusion, one
could conceivably add enough tree properties for each literal to be
realised by at most one lexical item each.[^1] For the most part having
an ambiguity-free lexical selection is enough to guarantee that the
surface realiser returns a single paraphrase, the exceptions being
inputs where the lack of word order constraints (e.g., intersective
modifiers) comes into play. The tree properties associated with each
lexical item must *uniquely* identify that item. In other words, each
enriched lexical item must be associated with a so-called tree
identifier:

> In an FB-LTAG grammar, a **tree identifier** is a set of tree properties
> $I$. If in a set of lexical entries, there is only one enriched
> lexical item $〈T,S,LTP〉$ such that $LTP
> \subseteq I$, we say that the tree identifier is unique to the set.

What is important here is not just the fact that tree identifiers are
unique. After all, the elementary trees produced by XMG already have
unique names like `Tn0Vn1-387` which would technically allow one to
achieve the same result. But these do not have the same practical use as
proper tree identifiers, the main reason being that they are completely
arbitrary, and are not imbued with the same linguistic significance as
tree properties. The reason linguistic significance matters is that it
gets us closer to the objective of choosing
**contextually appropriate**
paraphrases. Basically, we need some means of representing linguistic
alternatives and selecting from them. We believe that disjunctions in
the metagrammar serves as a mechanism for representing the alternatives,
and that tree properties provide the mechanism for selecting among them.

[^1]: We say at most because of lexical items with a multi-literal
    semantics
