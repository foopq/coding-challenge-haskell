
Requirements:

    This was built on an up to date Ubuntu system with ghc 7.4.1.
    The packages required are "ghc" and "cabal-install" and their dependencies.

    We also require the following cabal packages (installed from build script, so this can be ignored):
        containers, deepseq, json, mtl, parallel, split, text


Building instructions:

    To build, simply run the ./build.sh script.


Running instructions:

    Run using:

        ./sortable-challenge <listings.txt> <products.txt> <results.json>

    If no command line arguments are given, the defaults above are assumed.


