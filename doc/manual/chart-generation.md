---
title: Chart generation
---

# Realisation algorithms

## One-phase generation

This is a standard chart-and-agenda mechanism, where each iteration
consists of picking an item off the agenda and combining it with
elements from the chart.  You most likely want to avoid this option.

## Two-phase generation

Following
TODO: citation
 , we could also separate realisation into two distinct phases.
This requires that we maintain two seperate agendas and process them
sequentially, one loop after the other. See {switchToAux} for details.

-   If both Agenda and AuxAgenda are empty then there is nothing to do,
    otherwise, if Agenda is empty then we switch to the application of
    the Adjunction rule.

-   After the rule is applied we classify solutions into those that are
    complete and cover the semantics and those that don’t. The first
    ones are returned and added to the result, while the others are sent
    back to Agenda.

-   Notice that if we are applying the Substitution rule then the
    current agenda item is added to the chart, otherwise it is deleted.

### Switching phases

After the substitution and na-constraint phases are complete, we switch
to the final adjunction phase. We do this by deleting junk from the
agenda (particularly, trees with open substitution sites remaining),
transfering trees from the holding pen to the chart and setting the
phase to AdjunctionPhase

### Completion

#### SemFilter Optimisation

{sec:semfilter}

The purpose of the semantic filter optimisation is to take full
advantage of Carroll’s delayed adjunction. Consider the semantics
{def(m), poor(m), brokenhearted(m), man(m), def(w), woman(w),
beautiful(w), heartless(w), rejects(w,m)}. At the switchToAux step, we
are left with the initial trees {man}, {woman}, {the woman rejects the
man}.

It would be nice to filter out the structures {man} and {woman} since we
know that they are not going to be semantically complete even with
adjunction. More precisely, on the switch to adjunction, we do the
following:

-   Take the union of the semantics of all auxiliary trees; which we
    call $\phi^*$

-   Delete any initial tree with semantics $\phi^s$ such that
    $\phi^s \cup \phi^*$ is not the target semantics

In other words, we delete all initial trees that cannot produce a
semantically complete result even with the help of auxiliary trees.

FIXME: comment 2006-04-18: sem filter each polarity path separately
(this is more aggressive; it gives us much more filtering)

## Operations

We implement the two TAG operations, substitution and adjunction, below.
These are the only two operations we have, because we’re working with a
very simple builder that constructs derived trees.

### Substitution

{sec:substitution} {applySubstitution} Given a SimpleItem it returns the
list of all possible substitutions between it and the elements in Chart

### Adjunction

{sec:adjunction} {sec:ordered\_adjunction} {sec:foot\_constraint}
{applyAdjunction2p} Given a SimpleItem, it returns the list of all
possible adjunctions between it and the elements in Chart. The Chart
contains Auxiliars, while SimpleItem is an Initial

Note: as of 13 april 2005 - only uses ordered adjunction as described in

Note that in the one-phase variant of non-adjunction, we can’t do
top/bot unification on the fly, because afaik we can’t tell that a node
will never be revisited. One example of this is if you try to adjoin
into the root

The main work for adjunction is done in the helper function below (see
also figure {fig:adjunction}). Auxiliary tree `te1` has a root node `r`
and a foot node `f`. Main tree `te2` has an adjunction site `an`. The
resulting tree `res` is a result of splicing `te1` into `te2`. We
replace `s` with the nodes `anr` and `anf` (which are the results of
unifying `an` with `r` and `f` respectively).

> > ![image](images/adjunction.pdf) {fig:adjunction} {iapplyAdjNode}

In addition to the trees proper, we have to consider that each tree has
a list with a copy of its adjunction sites. The adjunction list of the
result (`adjnodes res`) should then contain `adjnodes te1` and
`adjnodes te2`, but replacing `r` and `an` with `anr`.

### Helper functions for operations

#### Derivation trees

We make the simplifying assumption that each chart item is only used
once. This is clearly wrong if we allow for items with an empty
semantics, but since we do not actually allow such a thing, we’re ok.

## Dispatching new results

Dispatching is the process where new chart items are assigned to one of
the trash, agenda, auxiliary agenda or chart. The item could be modified
during dispatch-time; for example, we might do top/bottom unification on
it. See {sec:dispatching} for more details.

### Top and bottom unification

During initialisation of the chart, any nodes which can never receive
adjunction should be top-bottom unified to start with.

##### tbUnifyTree

unifies the top and bottom feature structures of each node on each tree.
Note: this only determines if it is possible to do so. Actually
returning the results is possible and even easy (you’ll have to look
back into the darcs repository and unpull the patch from
2006-05-21T15:40:51 “Remove top/bot unification standalone code.”) but
since it is only used in the one-phase algorithm and for the graphical
interface, I decided not to bother.

Our helper function corresponds to the first unification step. It is
meant to be called from a fold. The node argument represents the current
node being explored. The Either argument holds a list of pending
substitutions and a copy of the entire tree.

There are two things going on in here:

1.  check if unification is possible - first we apply the pending
    substitutions on the node and then we check if unification of the
    top and bottom feature structures of that node succeeds

2.  keep track of the substitutions that need to be performed - any new
    substitutions that result from unification are added to the pending
    list

Note that we wrap the second argument in a Maybe; this is used to
indicate that if unification suceeds or fails. We also use it to prevent
the function from doing any work if a unification failure from a
previous call has already occured.

Getting this right was a big pain in the butt, so don’t go trying to
simplify this over-complicated code unless you know what you’re doing.

## Unpacking the results

Unpacking the results consists of converting each result into a sentence
automaton (to take care of atomic disjunction) and reading the paths of
each automaton.

### Sentence automata

{listToSentenceAut} converts a list of GNodes into a sentence automaton.
It’s a actually pretty stupid conversion in fact. We pretty much make a
straight path through the automaton, with the only cleverness being that
we provide a different transition for each atomic disjunction.

## Partial results

The user may ask for partial results when realisation fails. We
implement this using a greedy, full-commitment algorithm. Find the
discarded result that matches the largest part of the semantics and
output that fragment. If there are parts of the input semantics not
covered by that fragment, search for the largest chunk that covers the
missing semantics. Recurse until there are no more eligible items.
