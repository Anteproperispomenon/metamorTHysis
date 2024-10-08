# Glossary

Since there are a lot of terms with specific meanings in the
documentation, this document provides a ~~quick~~ reference for
such terms.

#### Aspect

An aspect is extra information about a phoneme that effectively allows you to "divide"
a phoneme into multiple "sub-phonemes". Aspects are defined in the phoneme specification,
seperately from the phonemes themselves. 

When using a phoneme that has aspects in the input specification, you must include values
for each value the phoneme has.

#### Boolean

Concerning values that can only be either "True"/"On" or "False"/"Off". 

Boolean expressions are ways of combining different Boolean values to produce a new 
value. For example, the boolean expression (`a AND b`) is only true when both `a`
and `b` are true. Otherwise, the result is false.

A Boolean operator is an `AND`/`OR`/`NOT`/etc...  element in a Boolean expression.

#### Case

The case of a character/letter is just whether the letter is upper-case/capital/majuscule
or lower-case/minuscule.

If a character is **casable**, then it has a corresponding character of the opposite case.
Otherwise, a character is uncasable.

#### Character

##### General Usage

In general, a [character](https://en.wikipedia.org/wiki/Character_(computing)) is a 
single component of text that has a unique [Unicode](#unicode) [codepoint](#codepoint).
A character can be a letter, a space, a punctuation mark, a diacritic, a symbol, an emoji,
or something else. In Unicode, characters have associated properties, such as case, equivalent
upper-case/lower-case character, which script it belongs to, etc...

Sometimes, two characters can look (almost) identical, but have different properties.
For example, the Latin Capital A `A` and the Greek Capital Alpha `Α` are indistinguishable 
in most fonts, but if you convert each letter to lower-case, they will instead come out as
lower-case a `a` and lower-case alpha `α` respectively.

Another confusing thing is that there can be more than one way of representing a letter with
a diacritic. For common letter-diacritic pairs, there is a usually a "pre-composed" character
with just a single code-point. For example, the letter `é` has the codepoint `U+00E9`. However,
it can also be created by using the plain letter `e` (`U+0065`) followed by the combining
acute accent character `U+0301`. This results in `é`, which is seemingly identical, but is
represented differently internally.

##### Pattern Usage

In metamorTHysis [patterns](#pattern), a character just refers to a (piece of an) external
representation of a [phoneme](#phoneme). It can be represented directly, or by specifying
the codepoint of the character in the usual `U+...` form.

#### Class

A [class](parsing.md#classes) is a simple way for the input specification to group together characters that are
functionally identical and then write a pattern that works if any character in the class
is encountered. 

#### Codepoint

A codepoint is just a number that corresponds to a specific [Unicode](#unicode)
character, and is usually represented in the format `U+XYZW`, where `XYZW` is
a hexadecimal[^1] number. When you look up information about a specific character,
the `U+...` format is almost always listed.

In general, related characters have nearby codepoints. E.g. letters from the same
script are grouped together.

#### Encoding

An encoding is a way to store a list of [codepoints](#codepoint) in a
standardised way. These codepoints can then be interpreted as a piece
of text that can then be displayed to the user. Most modern encodings
use [Unicode](#unicode) to convert these codepoints to text, but there
are other encodings (especially ones designed for languages with large
numbers of characters) that use their own set of codepoints.

For now, metamorTHysis exclusively uses [UTF-8](https://en.wikipedia.org/wiki/UTF-8)
as its encoding. This means that the specification files must be in
UTF-8, and the files converted by the enduser must be in UTF-8.

#### Enduser

Someone using a parser created with metamorTHysis to convert text from
one orthography to another.

This contrasts with just ["User"](#user).

#### Follow Patterns/Markers

See [Lookahead Patterns/Markers](#lookahead-patternsmarkers)

#### Grapheme

A grapheme is a unit of textual meaning in a language. For the most part,
it has the same meaning as [character](#character) in the documentation.

#### Group

A group is a division of phonemes. They are specified when defining your
phoneme set.

A common usage of groups is to divide phonemes into consonants and vowels.

#### Lookahead Patterns/Markers

Also known as followed-by patterns/markers or just follow-patterns/follow-markers.
Lookahead patterns have a lookahead marker of the form `>...` that checks
the [properties](#property) of the following phoneme. 

#### Marker

A marker is an element of a pattern that is neither a character/codepoint nor
a phoneme. In the input specification, they are found exclusively on the 
right-hand side of a pattern, while in the output specification, they can
be found on either side.

Markers can be for checking/changing state, specifying case, adding additional
info about where in a word a pattern can occur, etc...

#### metamorTHysis

This program. It converts specifications of [phonemes](#phoneme), 
input [patterns](#pattern), and output patterns into a [Parser](#parser)
that can be used to convert between different orthographies of the
same language. Unfortunately, the orthographies have to be deterministically
convertible between each other, which basically means there have to be
consistent rules for converting between the orthographies. All the orthographies
use the same internal representation, which consists of a list of [phonemes](#phoneme)
specified by the user.

The name is a combination of "Orthography" and "Metamorphosis". The **TH** is
stylised in upper-case since the program makes extensive use of `Template Haskell`,
which is almost always abbreviated to `TH` in code. 

There's no specific reason I swapped out the `o` for a `y`; I just liked it better.

#### Orthography

A way of depicting speech in a language textually, according to established
rules/spellings. Can sometimes overlap with terms like "alphabet" or "script".

#### Parser

A program/piece of code that takes text as input and then decides what
to do based on what it sees. A common usage is to take text as input, and
then produce modified text as output.

Note that there are two parsers discussed in this documentation. The
first is the generated parser, which is the parser that metamorTHysis
produces that can then be used by an enduser to convert text. The other
parser is the parser that metamorTHysis uses to read phoneme/input/output
specifications to then produce the generated parser. Hopefully it should
be clear in context.

#### Pattern

Patterns are the main component of the input and output specifications. 
Each one has a left-hand side and a right-hand side, seperated by a colon.

The left-hand side has the internal representation (i.e. [phoneme(s)](#phoneme)),
while the right-hand side has the external representation (i.e.[character(s)](#character)).
[Markers](#marker) can also appear, mostly on the right-hand side. They
provide extra information about how to process the pattern, and where it
can occur.

#### Phoneme

In Linguistics, a phoneme refers to a single sound that is produced
when speaking. Since determining which phonemes are distinct from
each other varies depending on language, 

In metamorTHysis, a phoneme refers to the internal representation
of a sound, as decided by the creator of the parser. While the set
of phonemes defined by the user isusually based on the set of distinct
phonemes in a language, they may not quite be one-to-one. For example,
the Peurunvan orthography for [Breton](https://en.wikipedia.org/wiki/Breton_language)
has a letter `zh` which is pronounced the same way as either `z` or `h`,
depending on the dialect. Thus, it doesn't represent a distinct sound,
but it is still a distinct unit of phonological meaning. Thus, it would
be a represented by a seperate "phoneme" in a metamorTHysis phoneme set.

#### Property

A [property](phonemes.md#properties) is a predefined set of extra information
that can then be applied to a phoneme in the phoneme specification. A property
is either an [aspect](#aspect) or a [trait](#trait).

#### Specification

The user-created docuements that specify the [phoneme set](phonemes.md), the
[input parser](parsing.md), and the [output runner](output.md).

#### State

There are two slightly different uses of `state` in the documentation. 

**_A_** state is a piece of data that can change as the parser parses a 
word for the enduser. They can be defined near the start of an input/output
specification, and can be either [Boolean](#boolean) or have one of a
number of specified values. States that can take one of a number of values
are often called "Value States" for brevity. Note that value states can
also be "off", which is their default value. 

**_The_** state is the list of the current values of all states. It changes
whenever a pattern succeeds that changes the state in some way. *The* state
is reset to have all values be `off` when the parser encounters a new word.

#### Symbol

In a pattern, a symbol can either be a character or a [marker](#marker).

#### Trait

A trait is an extra piece of information about a phoneme that is always the
same for that character. It can simply be on/off ("Boolean"), or have a
number of possible values that it can take.

Traits are specified seperately from phonemes, so the same trait can be
can be applied to different phonemes. However, the if the trait is non-Boolean,
the different phonemes with the same trait can have different values of the
same trait.

The trait can then be used when using a [lookahead marker](#lookahead-patternsmarkers).

Note that a phoneme can only have one value from a trait. Trying to specify
the same trait multiple times for a single phoneme will result in an error.

#### Unicode

[Unicode](https://en.wikipedia.org/wiki/Unicode) is, in brief, a standardised 
numbering for characters. Each letter/symbol/punctuation mark/diacritic/emoji/etc... 
is assigned a unique [codepoint](#codepoint), which is just a number.

#### User

Someone using metamorTHysis to create a parser. This is distinct from
[Enduser](#enduser), someone using the parser to convert text.

In some cases, I may also use "creator" with this meaning.

## Footnotes

[^1]: You don't need to know what a hexadecimal number is to use a
      codepoint; just copy the value as you see it.

