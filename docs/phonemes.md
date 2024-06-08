# Phonemes

## Introduction

Phonemes are the audible sounds for a language. It's also
the term used to refer to the individual elements in the
"intermediate form" used when converting between orthographies.

## Groups

Sometimes, you want to categorise phonemes in different ways.
One way to do this is to use groups. Groups allow you to 
partition your set of phonemes into separate[^1] sets.
The main usage of groups is to separate vowels and consonants
for syllabic orthographies that combine consonants and
vowels into individual characters. 

If you want, you can even make sub-groups of phonemes.
However, you cannot have a group with both phonemes and
sub-groups.

e.g. In Haskell, the following groups of phonemes would 
translate into the following types:

```
* vowel
  a
  e
  i
  o
  u
* consonant
  b
  d
  g
  k
  l
  m
  n
  s
  t
  w
```

```haskell
data Phoneme = Ph1 Ph_vowel | Ph2 Ph_consonant
  deriving (Eq, Ord, Show)

data Ph_vowel
  = Ph_vowel_a | Ph_vowel_e | Ph_vowel_i | Ph_vowel_o | Ph_vowel_u
  deriving (Eq, Ord)

instance Show Ph_vowel where
  show Ph_vowel_a = "a"
  show Ph_vowel_e = "e"
  ...

data Ph_consonant
  = Ph_consonant_b | Ph_consonant_d | Ph_consonant_g | Ph_consonant_k
  | Ph_consonant_l | Ph_consonant_m | Ph_consonant_n | Ph_consonant_s
  | Ph_consonant_t | Ph_consonant_w
  deriving (Eq, Ord)

instance Show Ph_consonant where
  show Ph_consonant_b = "b"
  show Ph_consonant_d = "d"
  ...

```

## Properties

Properties are divided into two groups, `aspect`s and `trait`s
(terms subject to change). 

A `trait` is a piece of information
about a phoneme that is always true; e.g. "**o** is always a vowel",
so you could give **o** the attribute `vowel`.

`trait`s are boolean by default. You define them by writing
`trait <name>` in the header of the phoneme file. If you want
more than just `True/False` as the options of this attribute, you
can specify the names of the options by adding a colon after the
`trait` name and then listing the options. e.g.
`trait release : labialised palatalised`. For these `trait`s,
Haskell internally uses a `Maybe` type, so not all phonemes need
to have this attribute listed. When applying a `trait` with multiple
options to a phoneme, you must add `<trait_name>=<trait_value>` after
the colon for that phoneme, e.g. `articulation=velar`.

An `aspect` on the other hand, is extra information about a phoneme
that can change. A common example could be vowel length, but length
isn't always discernable from the orthography alone.

A more visible example is for languages with vowel harmony, where vowels are
grouped by some property. Usually, each vowel in one group corresponds
to a vowel in the other group(s). For example, in Mongolian, vowels
are grouped into "front" and "back" vowels, like so:

|name|front|back|
|----|-----|----|
| e  | e   | a  |
| u  | u   | ʊ  |
| o  | o   | ɔ  |
| i  | i   | i  |

In this case, you could create an `aspect` called `harmony`, which can
be either `front` or `back`. Then, you could define four vowels, `e`, `u`,
`o`, and `i`, where the first three have the `aspect` harmony. That way,
when writing your `output` specification, you can easily choose the
output character based on the value of `harmony`.

## Example

As an example of the above features, the following specification
would be translated into the following Haskell code:

```
aspect harmony : front back
trait neutral
trait articulation : labial alveolar velar 

=====
* vowel
  e : harmony
  u : harmony
  o : harmony
  i : neutral

* consonant
  b : articulation=labial
  d : articulation=alveolar
  g : articulation=velar
  ...
  

```

```haskell

data Tr_articulation
  = Tr_articulation_labial
  | Tr_articulation_alveolar
  | Tr_articulation_velar
  deriving (Eq, Ord)

instance Show Tr_articulation where
  show Tr_articulation_labial   = "labial"
  show Tr_articulation_alveolar = "alveolar"
  show Tr_articulation_velar    = "velar"

data Asp_harmony
  = Asp_harmony_front
  | Asp_harmony_back
  deriving (Eq, Ord)

instance Show Asp_harmony where
  show Asp_harmony_front = "front"
  show Asp_harmony_back  = "back"

data Phoneme = Ph1 Ph_vowel | Ph2 Ph_consonant
  deriving (Show, Eq, Ord)

data Ph_vowel
  = Ph_vowel_e Asp_harmony
  | Ph_vowel_u Asp_harmony
  | Ph_vowel_o Asp_harmony
  | Ph_vowel_i
  deriving (Eq, Ord)

instance Show Ph_vowel where
  show Ph_vowel_e hrm = "e" <> ", harmony=" <> show hrm
  show Ph_vowel_u hrm = "u" <> ", harmony=" <> show hrm
  show Ph_vowel_o hrm = "o" <> ", harmony=" <> show hrm
  show Ph_vowel_i = "i"

data Ph_consonant
  = Ph_consonant_b
  | Ph_consonant_d
  | Ph_consonant_g
  deriving (Eq, Ord)

instance Show Ph_consonant where
  show Ph_consonant_b = "b"
  show Ph_consonant_d = "d"
  show Ph_consonant_g = "g"

isNeutral :: Phoneme -> Bool
isNeutral (Ph1 Ph_vowel_i) = True
isNeutral _ = False

whatArticulation :: Phoneme -> Maybe Tr_articulation
whatArticulation (Ph2 Ph_consonant_b) = Just Tr_articulation_labial
whatArticulation (Ph2 Ph_consonant_d) = Just Tr_articulation_alveolar
whatArticulation (Ph2 Ph_consonant_g) = Just Tr_articulation_velar
whatArticulation _ = Nothing
```




## Footnotes

[^1]: Technically, the set theory term is "pairwise disjoint".