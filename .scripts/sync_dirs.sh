#!/usr/bin/env bash

DIR1=$(realpath $1)
DIR2="ssh://daiwz@hal9002//$DIR1"
unison -auto $DIR1 $DIR2
