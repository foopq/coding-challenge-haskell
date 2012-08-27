#!/bin/bash

cabal install json mtl parallel split text containers deepseq
cabal configure
cabal build

ln -s `find -name sortable -type f` ./sortable-challenge


