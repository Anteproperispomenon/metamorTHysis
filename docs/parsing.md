# Parsing

## Introduction

Parsing is how you convert text in a certain orthography to
a list of phonemes. This document describes how to write
the specification that `metamorTHysis` will use to generate
such a parser.

## Specification

### Introduction

When you make a parser file, the basic setup is this:

```

orthography : name_of_this_orthography
phoneme set : name_of_phoneme_set

====

# Classes and States

====

# Single-phoneme Patterns here

====

# Multi-phoneme Patterns here (optional)


```

Lines that start with `#` are "comments"; i.e. any
text on that line after the `#` is ignored by the
program. This is useful for providing extra information
about the patterns/etc... that you're writing. 

Sections are delimited with four or more equal signs in a row
(i.e. `====`). The sections must come in the order listed
above. If you wish to leave any section empty, just
write two lines with four equal signs with nothing in
between them.

e.g.
```
orthography : name_of_this_orthography
phoneme set : name_of_phoneme_set

====

====

...

```

...would be a pattern with no classes or states.

### Header

The header is the first thing in the file. it consists
of two pieces of information: The orthography name, and
the phoneme set name. The orthography name is just a
name to make it easier to refer to this orthography,
while the phoneme set name refers to the file that
defines the phoneme set you're working with. It can
be a file name, or the name of the phoneme set
specified in the phoneme file.

### Patterns

Patterns are the most important part of the parser file.
They are what let you convert text into phonemes.

A pattern consists of two parts: the output phoneme(s),
and the input sequence. The general form of a pattern
is e.g.

```
# output : input
ts : t s
```

with the output phoneme(s) on the left of the colon and
the input sequence on the right. Note that there must
be a space between each value in the input sequence.

Note that you can have multiple patterns for the same
phoneme. In which case, you just put patterns with the
same phoneme on multiple lines, e.g.

```
ts : t s
ts : t z
ts : c
```

#### Characters

For the most part, when writing the input sequence, 
you'll just use the characters you want to use directly
in the sequence. However, some characters do not work
well when used this way. Diacritics, right-to-left 
characters, and zero-width characters are all difficult
to use correctly in this setting. For such characters,
it is easier to use its **Unicode Codepoint** instead. The
Unicode Codepoint of a character is a number that refers
to a specific character. Typically, a unicode codepoint
is represented in the form `U+WXYZ`, where `WXYZ` are
the digits of a number in hexadecimal form. If you want
to find out the Unicode Codepoint of a character, you
can search online for it, and you'll typically find
the character in question. Then, there's typically
codepoint listed in the `U+...` form shown above. Usually,
you can just copy this codepoint value and put in the 
pattern, e.g.

```
ch : U+02A7
```

... and it'll work. If you don't know which codepoint you
*should* use, it's not a bad idea to include multiple 
possibilities in your patterns, since it's possible that
other users have used the "wrong" characters. Just make
sure that the correct option is included among the possibilities,
and that it's the one being used in the output file.
Also note that you can use this form of codepoint input
when defining a class, as seen [in the classes section](#classes).

Finally, since some characters have other meanings when
used (see [Special Patterns](#special-patterns)), they can't
be used directly when writing a pattern. To use them in 
a pattern, either use their codepoint form, or just prepend
a blackslash (`\`) to the character in question. e.g. if
you want to use a `#` in a pattern...

```
kh : k \# 
```

#### Single-Phoneme Patterns vs. Multi-Phoneme Patterns

You may have noticed that the example input file has two
sections for patterns: single-phoneme patterns and
multi-phoneme patterns. This is because the output phoneme(s)
are interpreted differently in the two sections.

In the Single-Phoneme section, only the first string before 
the colon is interpreted as a phoneme, and the other strings
between it and the colon are interpreted as aspects of the
phoneme (see [phonemes.md](phonemes.md) for more info on
aspects/etc...). Thus, only one phoneme can be used per
pattern in this section.

e.g. something like 

```
x uv : ...
```

would be interpreted as the phoneme `x` with its first
aspect set to the value `uv`.

In the Multi-Phoneme section, each string is interpreted
as an individual phoneme. This is useful for syllabaries
and other writing systems where a single character can
represent more than one phoneme. You can still use 
phonemes that require aspects, but you need to enclose
the character and their aspects in parentheses. e.g.
for the example above... 

```
(x uv) : ...
```

would be the equivalent pattern in the Multi-Phoneme
section. On the other hand,

```
x uv : ...
```

would be interpreted as the phoneme `x` followed by
the phoneme `uv`.

Note that you can still have single-phoneme
patterns in the multi-phoneme section, it's just that
left-hand-sides/output phonemes are interpreted
differently. The right-hand-sides/input sequences are
treated the same in the two sections.

#### Special Patterns

Not all values in the output sequence/right-hand-side
of a pattern represent characters that can be parsed.
There are also several characters that represent 
special information about the pattern:

  - `^` : This pattern can **only** occur at the beginning of a word.
  - `%` : This pattern **can't** occur at the beginning of a word.
  - `$` : This pattern can **only** occur at the end of the word.
  - `&` : This pattern **can't** occur at the end of a word.
  - `*` : Indicates that a `class` is being used (see below).
  - `@` : Indicates that a `state` must be equal to a certain value for this pattern to be valid.
  - `!` : Indicates that a `state` must be changed/set to certain value if this pattern is successful.
  - `+` : This pattern represents an upper-case letter/grapheme.
  - `-` : This pattern represents a  lower-case letter/grapheme.

First, if `+` or `-` are to be used, they must be the **first** symbol 
in the input sequence. However, most of the time, using `+` or `-` is
completely unnecessary, as the program automatically generates parsers
for the "official" upper-case and lower-case variants of a character.
You only need to use these if the character you're using has an 
upper-case or lower-case variant that isn't considered "official",
or the official upper-case/lower-case variant of the letter doesn't
work for this orthography. e.g. The dotted vs. undotted `i` in Turkish
doesn't work with properly with the official `toUpperCase` and
`toLowerCase` functions. In which case, you'd want to write the
following:

```
i-dot : - i
i-dot : + U+0130
i-no-dot : - U+0131
i-no-dot : + I
```

Next are the patterns `^` and `%`. If present, these must be the first[^1]
value in the sequence (but still after `+`/`-` if they're present). 
These require that a pattern only work at the start of a word, or
work anywhere *except* the start of a word, respectively. These can
be useful when a letter has a different interpretation depending on
whether it's the first letter in a word or not. e.g. In the U'mista
orthography for Kwak'wala, words that start with a glottal stop
followed by a vowel don't notate the glottal stop, since there are
no words that start with a vowel *not* preceded by a glottal stop.
In this case, you could write the following patterns:

```
a : a
e : e
i : i
...

glot a : ^ a
glot e : ^ e
glot i : ^ i
...
```

Note that we didn't have to use `%` for the non-start-of-word vowels;
this is because the parser will always check start-of-word patterns
first before moving on to other patterns when parsing the first 
phoneme of a word. Thus, the primary purpose of `%` is to enforce
that a certain phoneme **can't** occur at the beginning of a word.

`$` and `&` work similarly, except they match the *end* of a word
instead of the beginning. They must be the final value in an input
sequence if present. Like `^`/`%`, they can be useful if the final
grapheme of a word is interpreted differently from how it would
be interpreted in the middle of a word. Again, it's only necessary
to use `%` if a certain sequence of characters can **only** occur
in the middle of a word.

Another use of `^` and `$` is for matching specific words that don't
follow the usual pronunciation rules of a language. This is only
really feasible for a small number of words, since it can bloat the
size and running time of the parser. It can be useful in languages
such as Welsh, where there are a few small words that don't quite
fit the usual pronunciation rules. e.g.

```
(y schwa)   : ^ y $
(y schwa) r : ^ y r $
(y schwa) n : ^ y n $
...
```

Next is `*`, which is just how `classes` are used in input sequences.
To use them, you just write `*` immediately followed by the name of
the class (no space between `*` and the name). e.g. if `apost` is the
name of a class, then to use it in a pattern, you would write e.g.

```
t'  : t *apost
ts' : t *apost s
ts' : t s *apost
```

For more information on how to write and use classes, see [Classes](#classes)
below.

Finally, there are the special characters `@` and `!`. These are 
used like `*` for classes, except they have an additional parameter.
You use them like so:

```
u : u !last_vowel=high
o : o !last_vowel=mid
a : a !last_vowel=low

lg : @last_vowel=low l
l  : l
rh : @last_vowel=low r
r  : r
...
```

Again, `@` is used to check the current state, and
`!` is used to change one of the state values. See
[States](@states) for more information.

### Classes

Classes are a way to make parsing text easier. Let's
say you have an orthography where apostrophes are 
commonly used. However, there are multiple different
apostrophes that are often used when typing in this
orthography. In that case, you can write

```
class apost : ' ` U+0313 U+0315
```

in the class section. This will let you cut down
on the number of patterns you need to write, since
you don't have to write one for each possible apostrophe.

#### Overlapping Classes

Note that issues may occur when writing multiple
classes with overlapping characters.

e.g.

```
...
====

class lab  : w v f W V F
class fric : v f s z V F S Z

====
...
```

In this case, the classes `lab` and `fric` share
the characters `v` and `f`. This can easily cause
a problem if you have patterns that **start** with
the two patterns, e.g.

```
...
hx : *fric x z
vx : *lab  x 
...
```

These patterns might not work properly, because of
the underlying structure of the generated parser.
It might not be a problem if you never have to choose
between the two patterns, e.g.

```
tf : t *fric
df : d *fric
kw : k *lab
gw : g *lab
```

...since the generated parser never has to make a choice
between the two classes. 

However, in general, it is best to avoid using overlapping
classes, since they can cause unexpected problems. Only
use them if you fully understand when it's safe to do so.

If you absolutely must use overlapping classes, you can
split the classes up into multiple "sub-classes", like so:

```
class fric     : s z S Z
class fric_lab : v f V F
class lab      : w W
```

and then, with the first example, we can duplicate the
patterns like so:

```
...
hx : *fric x z
hx : *fric_lab x z
vx : *lab  x 
vx : *fric_lab x
...
```

In the future, there may be an option in the parser to
automatically split up overlapping classes like so.

### States

#### Introduction

`States` are the main way to provide information about
phonemes earlier in a word to the parser further along
in the word. For example, let's say you wanted to know
what the last vowel parsed was. That can be done like so:

```
...
====
# Classes and states

state last_vowel : va ve vi vo vu

====

# Single-phoneme Patterns

a : a !last_vowel=va
e : e !last_vowel=ve
i : i !last_vowel=vi
o : o !last_vowel=vo
u : u !last_vowel=vu
...
```

After which, you could change how other phonemes are
parsed depending on the previous vowel.

#### Defining States

`States` are defined in the same section as classes,
except instead of starting with `class`, they start
with `state`, e.g.

```
state last_vowel : va ve vi vo vu
state seen_n
```

These are the two different kinds of states:
**enumeration**/**value** states, and **boolean** states
respectively. **Boolean** states are only ever `on`/`off`,
while **value** states can be `off` or hold one of the
values to the right of the colon.

As suggested above, **boolean** states are defined by
writing `state <state_name>`, and then not including
a colon or any other text after the state name.

On the other hand, **value** states are defined
like `classes`, except the line starts with `state`
instead of `class`, and the right-hand side is a list
of value names[^2] instead of individual characters.
Note that you **can** reuse value names in different
states, but you can't use the same value name twice
in one state. i.e.

```
# These two states are fine together
state last_vowel      : va ve vi vo vu
state last_last_vowel : va ve vi vo vu

# As are these two
state last_fric : v f s z th sh zh
state last_lav  : v f w m p b

# This state is NOT okay
state not_okay : not not yes
```

#### Changing States

At the beginning of parsing a word, all states are set
to `off`. To change the value of any of the current
states, you end one of your patterns with a string of
the following form:

```
!<state_name>=<state_value>
```

If `state_name` refers to a **boolean** state, then 
`state_value` must indicate either `on` or `off`. There
are many acceptable strings to represent this, such
as `true`, `yes`, `y`, `on`, or `t` to represent `on`,
and `false`, `no`, `n`, `off`, or `f` to represent `off`.

On the other hand, if `state_name` refers to a **value** state,
then `state_value` must either be one of the values that state
can take, or it can be `off`. Again, you can use `false`, `n`,
etc... to indicate `off`. However, you **can't** just set
the value to `on`, since there is no default `on` value
for **value** states. 

Note that you can change multiple states at once simply
by putting multiple state-change strings in the same
pattern.

#### Checking States

In order for changing the state to have any meaning, there
needs to be a way to inspect the state during a pattern.
To do this, you add a string of the following form
near[^3] the beginning of the output sequence:

```
@<state_name>=<state_value>
```

The form is nearly identical to the change-state strings,
except that the first character is `@` instead of `!`. The 
other difference is that you **can** check whether a **value**
state is `on`, since all it checks is that **value** state in
question isn't `off`. 

Note that putting multiple state-checks in the same pattern
means that **all** of the state-checks must hold. If you only
want **at least one** of the state-checks to hold, you can
create multiple of the same pattern, with one state-check
in each.

As an example, the following specification...

```
...

====

# What the last vowel was
state last_vowel : va ve vi vo vu

# Whether the last consonant was velar.
state last_is_velar

====

# Single-phoneme Patterns

a : a !last_vowel=va
e : e !last_vowel=ve
i : i !last_vowel=vi
o : o !last_vowel=vo
u : u !last_vowel=vu

# Return "lg" if the last vowel was "o" or "u"
lg : @last_vowel=vo l !last_is_velar=on
lg : @last_vowel=vu l !last_is_velar=on

# Otherwise, return "l"
l  : l !last_is_velar=off

g : g !last_is_velar=on

# Return "ng" if the last consonant was velar,
# and if the vowel was "e" or "i".
ng : @last_is_velar=on @last_vowel=e n
ng : @last_is_velar=on @last_vowel=i n

# Otherwise return "n"
n : n !last_is_velar=off

...

```

uses both `all` state checks (the patterns for `ng`) and 
`any` state checks (the patterns for `lg` and `ng`).


## Footnotes

[^1]: Note that state checks can come before `^` or `%`, but it only really makes sense
      for `%`, since the state at the beginning of a word is always the default state.
[^2]: A value name is any string of characters starting with a Latin letter, and followed
      by a Latin letter, an Arabic numeral, or one of the characters `-`, `_`, or `'`.
[^3]: Technically, it can be anywhere in the pattern so long as it's after a `+` or `-`
      character if present.
