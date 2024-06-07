# metamorTHysis

## About

metamorTHysis is a Domain-Specific Language/Parser Generator for converting 
between different orthographies of the same natural language. Unfortunately,
it can only work on orthographies that can be deterministically converted 
between each other. 

## How Does it Work?

The goal of metamorTHysis is to have the user provide a "specification"
of a language that can be used to automatically generate a parser that
can automatically convert from any orthography with a specification to
another orthography with a specification.

The language specification consists of three parts:
  
  * The phoneme specification (one for the whole language)
  * The input specification (one for each orthography)
  * The output specification (one for each orthography)

This architecture is designed so that you only have to write one
input specification and one output specification for each orthography.
The program works by converting from one orthography to an
"intermediate form" (defined in the phoneme speicification), and
then converting the intermediate form to the output orthography.

### Phoneme Specification

The phoneme specification is for defining the "intermediate form"
thay you'll use for converting between orthographies. It is essentially
a list of phonemes (i.e. a list of identifiers) together with optional
properties for each phoneme. At the moment, these properties are primarily
used by the outputter to determine how to process phonemes.

Typically, phoneme files use the extension ".thym" or ".thyt", but you're
free to use whichever extension you choose.

For more info on Phoneme specifications, see [the phoneme documentation](docs/phonemes.md).

### Parser Specification

Writing the parser specification is how you get input text into the
intermediate phoneme list mentioned above. It is, effectively, a
list of phonemes matched to how they are represented in this 
orthography. The actual format is more complicated than that,
so you'll want to look into the [the parser documentation](docs/parsing.md).

Typically, parser files use the extension ".thyp" or ".thyi".

### Output Specification

The output specification is how you go from a list of phonemes
to the desired output text. The format is very similar to the 
parser format, but there are some key differences. For more
information, have a look at [the output documentation](docs/output.md).

Typically, output file ue the extenion ".thyo".

## Notes

### Case/Capitalisation

Note that as of 2024-06-07, case detection is *still not implemented*.
i.e. When a phoneme is parsed, data about whether the character was 
upper-case or lower-case is not retained. While there is already a lot 
of code to handle cases, it's effectively useless, since there's no
indication of which case a `Phoneme` should be.

This does not mean that you just shouldn't care about case; the
generated parsers already interpret both upper-case and lower-case
versions of the same character **as** the same character; you
don't have to write out two patterns for the same character.

Similarly, you should still include case information in your
specifications where it's important. Once casing is implemented,
it will correctly interpret the casing information in your
specification files.

