#!/bin/bash

DRIVER_PROGRAM="test_build/vegetable_driver"

./Shakefile.hs "${DRIVER_PROGRAM}" && $DRIVER_PROGRAM -q -v -f "${1}"
