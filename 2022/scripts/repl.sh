#!/bin/bash
cd "`dirname $0`/.."

clj -M -m nrepl.cmdline --interactive