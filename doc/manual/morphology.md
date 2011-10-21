# Morphology

There are two basic tasks: morphological input and output. GenI farms
out morphology to whatever third party program you specify on the
command line. Note that a simple and stupid “sillymorph” realiser is
provided either in the GenI repository or on hackage.

## Morphological input

Morphological input means attaching morphological features on trees. The
user specifies morphological input through the input semantics. Our job
is to identify morphological predicates like {plural(x)} and apply
features like { num:pl} on the relevant trees.

## Morphological realisation

{Morphological realisation} refers to the actual process of converting
lemmas and morphological information into inflected forms. We do this by
calling some third party software specified by the user.

The morphological software must accept a JSON list of {lemma sentences}
where each lemma sentence is itself a list of objects containing a lemma
and a feature structure.

    [
     [{"lemma": "le",       "lemma-features": "[num:sg gen:f]"},
      {"lemma": "fille",    "lemma-features": "[num:sg]"},
      {"lemma": "detester", "lemma-features": "[num:sg tense:past]"},
      {"lemma": "le",       "lemma-features": "[num:pl gen:m]"},
      {"lemma": "garcon",   "lemma-features": "[num:pl]"}
     ],

     [{"lemma": "ce",       "lemma-features": "[]"},
      {"lemma": "etre",     "lemma-features": "[]"},
      {"lemma": "le",       "lemma-features": "[]"},
      {"lemma": "garcon",   "lemma-features": "[]"},
      {"lemma": "que",      "lemma-features": "[]"},
      {"lemma": "le",       "lemma-features": "[num:sg gen:f]"},
      {"lemma": "fille",    "lemma-features": "[num:sg]"},
      {"lemma": "detester", "lemma-features": "[num:sg tense:past]"}
     ]
    ]

NB: I recommend using a JSON library instead of parsing and writing this
by hand.

The morphological realiser may return more than one output per sentence.
Indeed, we expect a JSON-formatted list (a) of lists (b), where each (b)
provides a number of candidate morphological realisations for a sentence
in (a). The list (a) must have the same length as the input because each
item in (a) is expected to correspond to a sentence from the input.

Notice that the morphological generator can choose to delete spaces or
do other orthographical tricks in between words:

    [
     ["la fille detestait les garcons"],

     ["c'est le garcon que la fille detestait"
     ,"c'est les garcons que la fille detestait"]
    ]

If your morphological software does not do this, you could wrap it with
a simple script.
