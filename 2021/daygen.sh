#!/bin/bash

DAY=$1

echo "Creating files for day $DAY"
touch "./input/day$DAY-test.txt"
touch "./input/day$DAY.txt"
touch "./lib/day$DAY.ml"

dune build