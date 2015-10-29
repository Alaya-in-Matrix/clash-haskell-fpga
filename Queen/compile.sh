#!/bin/bash
git clean -xdf
clash --verilog Queen.hs
git clean -xf
