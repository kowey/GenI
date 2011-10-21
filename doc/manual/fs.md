# Feature structures

## Core types

A feature structure is a list of attribute-value pairs.

## Basic functions

### Traversal

### Pretty printing

## Feature structure unification

{sec:fs\_unification} Feature structure unification takes two feature
lists as input. If it fails, it returns Nothing. Otherwise, it returns a
tuple with:

1.  a unified feature structure list

2.  a list of variable replacements that will need to be propagated
    across other feature structures with the same variables

Unification fails if, at any point during the unification process, the
two lists have different constant values for the same attribute. For
example, unification fails on the following inputs because they have
different values for the *number* attribute:

> { cat:np\
>  number:3\
> } { cat:np\
>  number:2\
> }

Note that the following input should also fail as a result on the
coreference on *?X*.

> { cat:np\
>  one: 1\
>  two:2\
> } { cat:np\
>  one: ?X\
>  two:?X\
> }

On the other hand, any other pair of feature lists should unify
succesfully, even those that do not share the same attributes. Below are
some examples of successful unifications:

> { cat:np\
>  one: 1\
>  two:2\
> } { cat:np\
>  one: ?X\
>  two:?Y\
> } $\rightarrow$ { cat:np\
>  one: 1\
>  two:2\
> },

> { cat:np\
>  number:3\
> } { cat:np\
>  case:nom\
> } $\rightarrow$ { cat:np\
>  case:nom\
>  number:3\
> },

## Fancy disjunction
