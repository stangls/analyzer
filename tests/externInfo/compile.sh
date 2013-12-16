#!/bin/bash

f=$1

fOut=$(echo "$f" | sed 's/\..*//')

gcc -pthread -o "$fOut" "$1"

