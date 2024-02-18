gen-opcodes
===========

generates the opcodes for csound-expression lib

It contains three utilities:

* `gen-opcodes` - generates the package for csound opcodes in the current directory.

* `gen-opcodes-prepare-docs` - generats the `docs.txt` file, with descriptions of the opcodes.

* `gen-opcodes-unparsed` - shows all opcodes that are failed to parse.


### How to generate the package with opcodes

The package is generated from the HTML-file that contains the list of all opcodes.
It's in the `resources/MiscQuickref.html`. It's downloaded from the official Csound
docs. In the docs it's called Opcodes Overview.

With this file we can generate the description of the docs by launching
the utility `gen-opcodes-prepare-docs`. It generastes the file `docs.txt`
in the current directory. Then we need to place it in the `resources`
directory of the repo and rebuild the package.

After that we can launch the `gen-opcodes` and it will generate the package we need.

Note that if you are not a developer of the package the launch of `gen-opcodes` is enough.
We need to regenrate the docs only if we want to upgrade the opcodes for the new version.

To update the version of the generated package we need to edit the
corresponding .cabal file in the resources.
