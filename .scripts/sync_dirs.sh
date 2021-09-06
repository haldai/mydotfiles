#!/usr/bin/env bash

DIR1=$(realpath $1)

if [ -z "$2" ]
then
    TARGET="hal9002"
else
    TARGET=$2
fi
DIR2="ssh://daiwz@$TARGET//$DIR1"
unison -auto $DIR1 $DIR2
