# Phonemes

## Introduction

Phonemes are the audible sounds for a language. It's also
the term used to refer to the individual elements in the
"intermediate form" used when converting between orthographies.


## Properties

Properties are divided into two groups, `aspect`s and `attribute`s
(terms subject to change). 

An `attribute` is a piece of information
about a phoneme that is always true; e.g. "**o** is always a vowel",
so you could give **o** the attribute `vowel`.

`attribute`s are boolean by default. You define them by writing
`attribute <name>` in the header of the phoneme file. If you want
more than just `True/False` as the options of this attribute, you
can specify the names of the options by adding a colon after the
`attribute` name and then listing the options. e.g.
`attribute release : labialised palatalised`. For these `attribute`s,
Haskell internally uses a `Maybe` type, so not all phonemes need
to have this attribute listed.

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


```
aspect harmony : front back
attribute neutral


=====
* vowel
  e : harmony
  u : harmony
  o : harmony
  i : neutral

* consonant
  ...

```


