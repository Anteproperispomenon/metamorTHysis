# metamorTHysis

## About

metamorTHysis is a Domain-Specific Language for converting between
different orthographies of the same natural language. Unfortunately,
it can only work on orthographies that can be deterministically
converted between each other. 

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
properties for each phoneme. These properties are useful when the
input/output depends on e.g. where in a word a letter falls, whether
certain letters have already been encountered, etc...





