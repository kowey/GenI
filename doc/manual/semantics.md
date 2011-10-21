---
title: Semantics
---

## Overview

TODO: What is a semantics?

## Subsumption

We say that $L \sqsubseteq I$ (or $L$ subsumes $I$; mnemonic: $L$ for
lemma, and $I$ for input semantics as an example use) if for each
literal $l \in L$, we can find a *distinct* literal $i \in I$ such that
$l \sqsubseteq i$ (no resuing literals in $I$).

Notes about the subsumeSem function:

1.  We return multiple results because it’s important to take into
    account the possibility that one semantics subsumes different
    subsets of an another semantics. For example: {name(?X,?Y)} subsumes
    two different parts of {love(j,m), name(j,john), name(m,mary)}

2.  You MUST propagate the substitutions throughout any objects that
    contain the semantics.

3.  We return the unified semantics and not just the substitutions so
    that we know to do with anonymous variables.

As for literals $l$ and $i$, $l \sqsubseteq i$ if

1.  For the corresponding relations $lr$ and $ir$, $lr \sqsubseteq ir$

2.  $l$ and $i$ have the same arity

3.  All arguments $l_n \sqsubseteq i_n$

<!-- _ -->

## Unification

We say that $X \sqcup Y$ if... TODO
