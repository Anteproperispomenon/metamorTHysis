# Running

Your program won't be much use if you don't have a way to run it.
Fortunately, the [metamorTHysis template](https://github.com/Anteproperispomenon/metamorTHysis-template)
automatically creates some programs that you can run right out of the box.
These programs should be compiled anytime you run

```bash
stack build
```

to compile your code. 

## The Programs

As discussed in [compiling](compiling.md), there are two programs automatically
created when you use the [template](https://github.com/Anteproperispomenon/metamorTHysis-template):
`<project-name>-cli` and `<project-name>-server`. You can run these by using the `stack run`
command followed by the program name. e.g. If the name of your project is `my-converter`, you
can run the server with the following command:

```bash
stack run my-converter-server
```

Note that if you want to give arguments to the program, you need to first add an extra `--` to
the command when using `stack run`. e.g. to run the server on port `9007`, you would need to
run the following command:

```bash
stack run my-converter-server -- --port 9007
```

When running the programs by themselves later on, you don't need this extra `--`.

### The CLI Program

The CLI (command line interface) program is meant for simple use of converting files from one
orthography to another. At the moment, this only works on plain-text files that are **entirely**
in the orthography you specify. 

For more info on the possible options in *your* converter, you can run it through Stack like so:

```bash
stack run my-converter-cli -- --help
```

This will give you a list of the possible options, along with a list of the possible orthographies.

The basic options for the CLI program are:

| Argument   | Description |
|------------|-------------|
| `--from`   | The orthography of the input file|
| `--to`     | The desired orthography of the output file|
| `--input`  | The name of the input file|
| `--output` | (Optional) The name of the output file. If left unspecified, will use a default name based on the input file name and the output orthography|

Also, note that on Windows (and possibly Unix-based operating systems), you can create batch/shell
files to automatically convert files when dragged-and-dropped onto the executable. You'd have to
create one batch file for each input/output orthography pair, but it may be more usable for certain
users. e.g. For the orthographies `orth1` and `orth2`, you could create a file called `O1toO2.bat` like
so:

```bat
my-converter-cli --input %1 --from orth1 --to orth2
```

Or, on Unix-like OSes, create a file called `O1toO2.sh` like so:

```bash
./my-converter-cli --input %1 --from orth1 --to orth2
```

To create a new batch file on Windows, go to the directory where you want to create it in `File Explorer`,
and then right-click in the directory, mouse over `New`, and then click `Text Document` in the menu
that pops up. This will create a new text document with a name like `New Text Document.txt`. Completely
replace the whole name (including the `.txt`) with the name of your desired batch file, which will have
to end in `.bat` (e.g. `O1toO2.bat`). This will cause a popup asking whether you are sure you want to
change the extension of this file. Click `Yes`; this won't cause any problems.

If you don't see the `.txt` at the end of the text file when renaming it, you can turn it on by selecting
`View` at the top of the `File Explorer` window, and then make sure `File Name Extensions` is enabled
near the right side of the panel.

After you've created this/these batch/shell file(s), you can distribute it/them together with the `CLI` program
to allow drag-and-drop operation. See ["Running the Programs on Their Own"](#running-the-programs-on-their-own)
for more details of how to distribute the program(s).

### The Server Program

As mentioned in [compiling](compiling.md), the Server program has two uses: one for application
developers, and one for end-users. At the moment, you can use either one by simply running
`my-converter-server`. 

#### JSON Server

The `JSON` server takes `POST` requests of `JSON` data and then returns an object indicating
whether the text could successfully be converted or not.

In general, `POST` requests are made to `<server>/convert` or `<server>/query` with the
following `JSON`:

| Key | Value |
|-----|-------|
| `"text"`    | The text to be converted by the server |
| `"input"`   | The orthography of the input text |
| `"output"`  | The desired orthography of the output text |

Note that you can also send a `POST` request with the text "info" or "details" to
`<server>/query`[^1] which will send back a `JSON` object containing the language name
and a list of orthographies, with each orthography having a list of ways to specify that
orthography in `JSON`, as well as a description of the orthography (if specified by the
programmer in the `Orthographies.hs` file). This can be useful if an application developer
wants to create a program that works for any `metamorTHysis-server` program, since they
can just send an info request and then create the UI around the response.

The information response data object is as follows:

```js
{ "language" : String // Name of the language
, "data" : // List of objects...
    [ { "name" : String   // Name of the orthography
      , "desc" : String   // Description of the orthography (can be null or ommitted)
      , "args" : [String] // List of ways to specify the orthography
      }
    ]
}
```

Now, when you actually send a `POST` request to `<server>/convert`, you will receive
a simple object, consisting of either a `"text"` field or an `"error"` field. If the
text could be converted successfully, only the text field will be present. Otherwise,
only the error field will be present. In the future, there may be cases where both
`text` and `error` are present, but such a response will not occur at this time.

#### HTML/AJAX Server

The `HTML`/`AJAX` server makes use of the `JSON` server, but provides a simple `HTML`
interface to use it. It can be reached by going to `<server>/interactive/convert.html`,
which on Windows can be reached (by default) at `localhost:8081/interactive/convert.html`.

For more info on how to customise this version, see [compiling](compiling.md#htmlajax-server).

## Running the Programs on Their Own

These programs are useful, but they aren't useful for end-users if they have to install GHC, Stack,
etc... just to run your programs. Fortunately, when compiling, Stack generates some executable files
that you can distribute to other users on the same operating system. Unfortunately, they can be hard
to find if you don't know where to look. 

When running `stack build`, near the end, there will be a line something like:

```
Installing executable my-converter-cli in ...\my-converter\.stack-work\install\<some_numbers>\bin
```

You can just navigate to this directory, and then copy `my-converter-cli[.exe]` and 
`my-converter-server[.exe]` to the directories of your choice. Note that in order for
`my-converter-server[.exe]`'s HTML server to work, you'll also need to copy the `static`
directory from the root of your project to the same directory as `my-converter-server[.exe]`.

If you don't see this line when running `stack build`, try modifying one of the `Main.hs`
files slightly, by adding an extra (empty) line or by adding a comment.

You should only do this when you're confident that your converter will work (or when you
want someone else to test it), since you'll need to repeat this step anytime you make modifications
to the project code (if you want it to be reflected in these copied binary files).

Now that you don't have to run these programs via Stack, you can run them as normal, e.g.

```bash
my-converter-cli --help

my-converter-cli --from orth1 --to orth2 --input "example.txt" --output "example-out.txt"

my-converter-server --port 8999
```

## Footnotes

[^1]: You can also just send a `GET` request to `<server>/info` to get the same orthography data.
