---
title: Ranking output with Optimality Theory
---

If your tree schemata are annotated with traces (TODO link to traces and
metagrammars), you can re-use them as a basis for ranking the output
produced by GenI. The basic idea is to supply a list of either positive,
negative or negative conjunction constraints.

For users familiar with Haskell, the constraints are described with the
following type:

Roughly speaking the more highly ranked the constraint, the greater the
impact of a violation of that constraint will be. See section
{sec:ranking-procedure} for more details on the ranking procedure.

## Input format

Constraints are expressed in JSON as a list of {ranking levels}. A
ranking level is a list of constraints that should be assigned the same
rank. In lieu of a formal description, we provide an example below: {NB:
Either the JSON format or the JSON parser used by GenI is strict enough
to refuse initial whitespace in this file.}

    [
     [{"neg-constraint": "dian0Vn1dePassive"},
      {"pos-constraint": "CanonicalSubject"}],

     [{"neg-conj-constraint": ["InvertedNominalSubject",
                               "CanonicalSententialObjectFinite"]}],

     [{"neg-conj-constraint": ["InvertedNominalSubject",
                               "UnboundedCleft"]},
      {"neg-constraint": "CleftSubject"}]
    ]

This example constraints file has three ranking levels. These levels
contain following constraints:

1.  A negative constraint saying that `dian0Vn1dePassive` should not
    appear, and a positive one saying that `CanonicalSubject` *should*
    appear. These constraints appear together only because the author of
    the example thinks they should have the same rank, not because there
    is neccesarily any inherent relationship between them.

2.  A single negative conjunction constraint saying that
    `InvertedNominalSubject` and `CanonicalSententialObjectFinite`
    should not appear together.

3.  A negative conjunction constraint saying tat
    `InvertedNominalSubject` and `UnboundedCleft` should not appear
    together; and also a negative constraints saying that `CleftSubject`
    should not appear. As with the first ranking level, there is no
    relationship between these two constraints. We just put them on the
    same level to give them the same rank

## Ranking procedure

{sec:ranking-procedure}

Generation results are sorted according to their highest-ranking
constraint violation (moving on to the next-highest ranking violation
and so forth in case of a tie). The best result appears first.

## Output format

Constraint violations can be outputted as JSON objects as the following
example shows

      { "lex-item": "discuter:n0Vn1pn2:Tn0Vn1pn2-5830:22",
      , "rank": 6,
      , "violation": {"neg-constraint": "passiveVerbMorphology"}
      }

Positive constraint violations are not associated with any lexical items
so the lex-item field is omitted for them.
