# Output

## Introduction

The **output specification** is how you go from the internal
phoneme-list to text in the orthography of your choice. The
format is very similar to that of the **parser specification**,
but with some key differences:

  - No single-phoneme pattern section
  - No classes
  - State-checks are placed **to the left** of the colon
  - More complicated types of cases/capitalisations.
  - Support for checking the following phoneme.
  - Support for checking traits and groups.

So if you haven't read [the parser specification](parsing.md) yet,
you'll probably want to do that first.

## Specification

### Introduction

The basic setup of an output specification is very similar to that
of a parser specification:


```

name : name_of_this_orthography
default case : <insert default case here>

====

# States and Group/Trait imports

====

# Single-phoneme and Multi-phoneme Patterns here

```

Again, lines that start with **`#`** are comments, and
sections are separated by four or more equal signs
(i.e. **`====`**). The sections **must** come in the
same order they are listed here, but you can leave the
state/import section empty if you don't need it.

### Header

Like with the parser specification, the output specification
starts with a simple header. `name` is equivalent to `orthography`
in a parser specification. `default case`, on the other hand, tells
the parser what kind of case to use for a pattern if no case is 
specified. This is because the output specification has more
nuanced support for cases, especially when multiple phonemes
are converted into a single character, which can be common in
syllabaries, syllabic alphabets, and abugidas. For more info
on the different cases, go down to the [case](#cases) section.

### States and Imports

Declaring `States` works very much the same way it did in the
phoneme specification; for more info, see [the state section there](parsing.md#states).

What's new is the ability to import `groups` and `traits`. You must
import a trait or a group if you want to make use of one in a "follow"
check. These are patterns that depend on the next phoneme in a word.
Using a group or trait makes such patterns much more succinct, since
you don't need to write one pattern for each possible phoneme it can
be followed by. 

To import a group or trait, you simply write `import group <group_name>`
or `import trait <trait_name>` respectively in this section. If you
forget to import a group/trait and then try to use it later on, the
compiler should remind you to add the correct `import` statement to
this section.

For more info on how to actually use groups or traits in patterns,
go down to the ["Followed-by" Patterns](#followed-by-patterns) section.

### Patterns

Patterns work much the same way they did [in phoneme specifications](parsing.md#patterns),
with a few differences [as mentioned in the introduction](#introduction).

The most important distinctions are:

  - No separate single-phoneme section
  - State-checks come **before** the colon
  - Cases (i.e. capitalisation rules) are more complicated
  - New "followed-by" patterns

The lack of a separate "single-phoneme" section just means that you need to
write your patterns as though they were in the Multi-Phoneme section in the
parser specification. For most single phonemes, this doesn't change anything.
The only exceptions are phonemes that have aspects. For those phonemes, you
have to enclose them in parentheses.

State-checks still work the same way as before; they're just placed before
the colon now. e.g. instead of 

```
g : @last_vow=e ĝ
```

you'd write

```
g @last_vow=e : ĝ
```

Note that you still *set* states on the right-hand side of the colon. e.g.

```
e : e !last_vow=e
```

is still the correct syntax.

#### Cases

Cases are more complicated in output specifications compared to how they
are in parser specifications. Most cases are now specified in the form `/xy`
where `x` determines how to interpret the case of the input, and `y` 
determines how to apply the case to the output. 

Since multiple phonemes can be compacted into a single character, there
needs to be a way to handle multiple phonemes of different cases. The
choice of how to handle this is called the `input style`. 

Similarly, when one (or more) phonemes correspond to multiple characters,
there needs to be a way to determine which output characters to apply the
case to. This is called the `output style`.

The input style (first character) can be chosen as follows:

|code|description|
|----|-----------|
| `a`/`A`      | Use the case of the **first** casable phoneme. |
| `z`/`Z`      | Use the case of the **last** casable phoneme.  |
| `0`          | If all phonemes are upper-case or uncasable, use upper-case. Otherwise, use lower-case.   |
| `9`/`t`/`T`  | If all phonemes are lower-case or uncasable, use lower-case. Otherwise, use upper-case.   |

The output style (second character) can be chosen as follows:

|code|description|
|----|-----------|
| `t`/`T` | Set the first casable character to upper-case, and leave the rest as lower-case (i.e. Title-case). |
| `a`/`A` | Make all casable characters in the output the same case. |

The most common/reasonable choices for case are `/at` and `/tt`. Both correspond
roughly to using title-case. The difference being that `/at` only looks at the
first casable phoneme, while `/tt` looks for the casable phoneme with the 
"highest" case. For the most part, you can just set your default case to one of
these and your output should behave as expected. If you want any specific pattern
to behave differently, you can just insert one of these case strings at the beginning
of the right-hand-side of the pattern.

You can also just use `/` to set the case to "null". This means that the input
case is ignored and the output uses the exact same characters listed in the
patterns. You might want to use this for uncased orthographies, such as most
syllabaries.

However, if the character you're using doesn't follow standard unicode casing conventions,
you can use the special case pattern `/?x`, where `x` is the input style of the
case[^1]. Now, the output pattern can be divided in two: one portion for upper-case,
and one portion for lower-case. The two halves are separated by a pipe (`|`). For example,

```
i : \?9 I | ı
```

would produce `I` when upper-case and `ı` when lower-case. 

#### "Followed-By" Patterns

"Followed-By" patterns are patterns that depend on the next phoneme in the list.
They're useful when the representation of a phoneme depends on the next phoneme,
but doesn't change how that next phoneme is represented. e.g. If a phoneme is 
represented one way when followed by a vowel, and another way when followed by
a consonant, you'll want to use a "Followed-By" pattern. 

To use a followed-by pattern, you start with a `>` followed by a string. That
string can be a group, a trait, an aspect, or a phoneme. For value-traits and
value-aspects, it can optionally followed by an `=` and then a value. For example,
for the phoneme specification...

```
trait back : velar palatal uvular

aspect air : rough smooth

trait nasal


====


* vowel
  a : air
  e : air
  i : air
  o
  u
* consonant
  n : nasal
  m : nasal
  ng : nasal back=velar
  j : back=palatal
  d
  t
  p
  b
  g : back=velar
  k : back=velar
  q : back=uvular
  x : back=velar
  xh : back=uvular
  ...

```

You could use the following "followed-by" patterns:

```
...

# An "o" followed by ng, j, g, k, q, x, or xh
o >back : õ
# Any other "o"
o : o

# A "u" followed by a nasal phoneme.
u >nasal : ũ
u : u

(a rough)  : á
(a smooth) : a

# A rough "i" followed by q or xh
(i rough)  : ḯ
# Any other rough "i".
(i rough)  : í

(i smooth) : i
(e rough)  : é
(e smooth) : e

t >air=rough : ṭ
t : t
d : d
p >air=rough : ṗ
p : p

# "g" followed by another "g".
g >g : ğ
g : g

# etc...

```

**NEW**: You can now use boolean expressions with followed-by patterns.
They work similarly to plain followed-by patterns, but are grouped together
between parentheses after the `>`. e.g.

```
t >(a|o|u) : ...
e >(nasal|air=rough) : ...
i >!nasal : ... # or >~nasal
```

If you aren't familiar with Boolean expressions, they work like so:

|symbol|example|description|
|------|-------|-----------|
| `&`/`&&`     |`a&b` | Only true if both expressions are true. |
| `\|`/`\|\|`  |`a\|b`| True if either expression is true |
| `!`/`~`      |`!a`  | Only true if the following expression is false |


You can also use nested parentheses for more complex expressions. However,
if you don't use nested parentheses when mixing `&`s and `|`s, it will be
interpreted like so:

```
(a&b|!c&d|e|f&!g&h) ==> ((a & b) | ((!c) & d) | e | (f & (!g) & h))
```

**Note**: Using `!`/`~` together with aspect-at/trait-at patterns might
not work as expected; e.g.

```
t >~air=rough : ...
```

would match any following phoneme *except* those that have the aspect `air`
set to `rough`. i.e. it would match any phoneme that doesn't have the `air`
aspect at all, as well as phoneme *with* air set to anything other than
`rough`.

If you want to simulate something more like `air!=rough`, i.e. "the next 
phoneme must have the `air` aspect, but it can't be `rough`", you can use
the following equivalent expression:

```
t >(air&!air=rough)
```


## Footnotes

[^1]: Arguably, this reverses the order of input style and output style, which
      can be confusing. The reason for the reversal has to do with how the 
      parser and internal types work.
