#!/bin/bash

DAY=$1

echo "Downloading input for day $DAY"
curl -s -H "Cookie: session=$AOC_SESSION_COOKIE" "https://adventofcode.com/2024/day/$DAY/input" > "./input/day$DAY.txt"

echo "Creating files for day $DAY"
touch "./input/day$DAY-test.txt"
touch "./solutions/day$DAY.ml"

dune build