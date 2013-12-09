#!/bin/bash

# choose wowcamldebug over ocamldebug
cmd=$(which wowcamldebug)
if ! [ -n "$cmd" ]; then
  echo "wowcamldebug not found! Using ocamldebug."
  cmd=ocamldebug
fi
if ! [ -n "$cmd" ]; then
  echo "Neither wowcamldebug nor ocamldebug found!"
  exit 1
fi

# make & 
time ./make.sh bdebug && \
$cmd $(find _build -type d | sed 's/^/-I /') $(ocamlfind query -recursive -i-format batteries cil xml-light) goblint.byte $@

