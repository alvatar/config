#!/bin/bash

SAVEIFS=$IFS
IFS=$(echo -en "\n\b")

# set me
FILES=$*
for f in $FILES
do
  #mp3val -f "$f"
  mp3check --fix-headers --fix-crc --cut-junk-start --cut-junk-end --cut-tag-end --add-tag "$f"
done

# restore $IFS
IFS=$SAVEIFS

