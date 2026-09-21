#!/bin/bash

# Script to generate bit-width specific top-level Verilog modules.
#
# Part of the publication Keszocze et al., Low-level optimizations in
# high-level HDLs: Is there a benefit?, International Workshop on
# Boolean Problems, 2026.
for f in $(ls ./clash/*.hs); do
  echo $f
  stack run clash -- --verilog $f
done
