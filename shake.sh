#!/bin/sh
mkdir -p _shake
stack exec -- ghc --make -itools Shakefile.hs -rtsopts -threaded -with-rtsopts=-I0 -outputdir=_shake -o _shake/build && _shake/build --color "$@"
