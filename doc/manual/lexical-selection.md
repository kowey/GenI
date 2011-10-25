---
title: Lexical selection
---

## Overview

Lexical selection combines the input semantics with the lexicon and the
tree schema file.  The input to lexical selection is (i) a flat input
semantics and (ii) an [FB-LTAG](fb-ltag.html) grammar.  The output is a
a set of elementary trees which can then passed on to the tree assembly
phase.

The lexical selection process can be described as

1. Select from the lexicon a set of lemmas from the lexicon whose
   semantics subsumes the input semantics.  Instantiate their semantics
   by unifying it with the relevant subset of the input semantics.
   We can call the results of this process *instantiated lemmas*

2. Combine the lemmas with the tree families they ask for.
   Each tree family contains a set of tree schemata.
   [Enrich](#enrichment) these tree schemata and unify the
   their semantics with the semantics of the instantiated lemmas.
   The result of this is a set of elementary trees

Because the input semantics is saturated, the instantiated lexical item
also has a saturated semantics.

## Enrichment
<a name="enrichment"/>

Enrichment is a process which adds features to either the interface, an
explicitly named node or the co-anchor of a lexically selected tree. The
enrichement information comes from the lexicon in the form of a path
equations which specify

1.  the location

2.  top or bottom

3.  the attribute

4.  what value to associate with it

The conventions taken by GenI for path equations are:

----------------------------------------------------------------------------
equation             effect
------------------   -------------------------------------------------------
`interface.foo=bar`  `foo=bar` is unified into the interface (not the tree)

`anchor.bot.foo=bar` `foo=bar` is unified into the bottom feature of the node
                     which is marked anchor.

`toto.top.foo=bar`   `foo=bar` is unified into the top feature of node named
                     “toto”

`anchor.foo=bar`     same as `anchor.bot.foo=bar`

`anc.whatever...`    same as `anchor.whatever...`

`top.foo=bar`        same as `anchor.top.foo=bar`

`bot.foo=bar`        same as `anchor.bot.foo=bar`

`foo=bar`            same as `anchor.bot.foo=bar`

`toto.foo=bar`       same as `toto.top.foo=bar` (creates a warning)

-----------------------------------------------------------------------------

## Lemanchor mechanism

One problem in building reversible grammars is the treatment of
co-anchors. In the French language, for example, we have some structures
like *C’est Jean qui regarde Marie* (*It is John who looks at Mary*)

One might be tempted to hard code the “ce” (“it”) and the “être” (is) into the
tree for regarder (look at), something like

    s(ce, être, n↓, qui, v(regarder), n↓)

Indeed, this would work just fine for generation, but not for parsing.
When you parse, you would encounter inflected forms for these items for
example *c’* for *ce* or *sont* or *est* for *être*.  Hard-coding the
*ce* into such trees would break parsing.

To work around this, we propose a mechanism to have our co-anchors and
parsing too. Co-anchors that are susceptible to morphological variation
should be

-   marked in a substitution site (this is to keep parsers happy)

-   have a feature `bot.lemanchor:foo` where foo is the coanchor you
    want

GenI will convert these into non-substitution sites with a lexical item
leaf node.
