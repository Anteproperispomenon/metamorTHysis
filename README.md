# metamorTHysis

## About

metamorTHysis is a Domain-Specific Language/Parser Generator for converting 
between different orthographies of the same natural language. Unfortunately,
it can only work on orthographies that can be deterministically converted 
between each other. 

## Getting Started

### Installing Haskell + Tools

In order to compile these programs, you'll need to have GHC (The Glasgow
Haskell Compiler) and some of its related tools installed. At the moment,
the best way to install these tools is to use [ghcup](https://www.haskell.org/ghcup/).
If you follow that link, it should give you instructions on how to install
ghcup. If you don't know what PowerShell (for Windows users) or a terminal
(for Mac and Linux/Unix users) are, ask someone with more technical experience
to help you install it. 

When you run this command, it'll ask you whether you want to install various
tools used with Haskell. You'll need the following tools:

  - GHC
  - Cabal
  - Stack
  - HLS (technically optional, but very helpful)

With these tools, you will be able to create orthography converters if
you write working specifications.

**Note**: Installing these components will take up a **significant** amount
of space on your main hard drive (for Windows users, this is your `C:\` drive).
If you have `<10GB` of free space on it, be very careful when installing these
tools, since they can take up a lot of space (at least temporarily).

### Additional Tools

For Windows users, I highly recommend installing [Windows Terminal](https://apps.microsoft.com/detail/9n0dx20hk701).
It makes it easy to use multiple different PowerShell/Command Line sessions
in the same window.

For writing the code, I recommend using [Visual Studio Code](https://code.visualstudio.com/),
often shortened to just "VS Code". Once you have VS Code installed, you should
also install [the Haskell plugin](https://marketplace.visualstudio.com/items?itemName=haskell.haskell)
for it, which will make editing your code (slightly) easier. 

### Starting Your Project

The best way to get started making a metamorTHysis-based converter is
to use the example stack-template. You can see what this looks like
at [metamorTHysis-template](https://github.com/Anteproperispomenon/metamorTHysis-template),
but it's easier to just automatically use it when creating a new project 
with `Stack`.

To do this, open up a terminal/shell andgo to the location on your computer 
where you want your project repository to be, and then copy and run the 
following command:

```
stack new my-converter Anteproperispomenon/metamorTHysis-template-beta
```

(Replace `my-converter` with the name you want to call your program)

This will create a new project with some example orthographies and
pre-filled in source files. You probably won't even have to modify
the following files:

  - `server/Server.hs`
  - `app/Main.hs`

And all you need to do to `server/Main.hs` is input the name of the
language your orthography converter is for.

Modifying `src/Orthographies.hs` will take quite a bit more work, but
it already has information on how to modify it to use your specification
files.

Most of the actual specification files are placed in the `orth/` directory,
which is where you should place your orthography and phoneme specificaitons
(see below).

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

### Putting It Together

You need to write an actual Haskell `.hs` file to group your
specification files into one program. If you created your
repository with the [metamorTHysis template](https://github.com/Anteproperispomenon/metamorTHysis-template),
it should already have an example file at `src/Orthographies.hs` that
will explain how to specify your specification files.

For more info, look at the [compiling documentation](docs/compiling.md).

## Running Your Program

After compiling your code, you'll probably want to actaully run the program(s).
You can either run them via Stack, or copy the binary files to another location
and run them there. For more info, see [running](docs/running.md).

## Notes

### Case/Capitalisation

Casing has now been implemented as of September/October 2024. However,
you should still make sure you check that casing behaves as you expect it 
when testing the generated parser.

~~Note that as of 2024-06-07, case detection is *still not implemented*.
i.e. When a phoneme is parsed, data about whether the character was 
upper-case or lower-case is not retained. While there is already a lot 
of code to handle cases, it's effectively useless, since there's no
indication of which case a `Phoneme` should be.~~

~~This does not mean that you just shouldn't care about case; the
generated parsers already interpret both upper-case and lower-case
versions of the same character **as** the same character; you
don't have to write out two patterns for the same character.~~

~~Similarly, you should still include case information in your
specifications where it's important. Once casing is implemented,
it will correctly interpret the casing information in your
specification files.~~

