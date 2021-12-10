#!/bin/bash

# cabal file template
name="day$(printf "%02d\n" $1)"
cabal run $name "`cat $2`"
