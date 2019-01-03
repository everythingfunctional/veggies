#!/bin/sh
mkdir -p tools_build
stack exec -- ghc --make -itools tools/makeDriver.hs -rtsopts -threaded -with-rtsopts=-I0 -outputdir=tools_build -o tools_build/makeDriver
