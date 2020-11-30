#!/usr/bin/env bash

# TODO: run with valgrind once gfortran memory leaks have been fixed
# i.e.  --runner "valgrind --leak-check=full --error-exitcode=1"

set -e

FPM_FLAGS="--flag -Wall --flag -Wextra --flag -Wimplicit-interface --flag -Werror --flag -fPIC --flag -g3 --flag -fbounds-check --flag -fcheck-array-temporaries --flag -fbacktrace --flag -std=f2018 --flag -fcheck=all"

fpm test ${FPM_FLAGS}
