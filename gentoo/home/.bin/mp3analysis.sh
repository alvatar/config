#!/bin/bash

# DIR="$*"
# if [ "x$DIR" == "x" ]
# then
  # DIR=.
# fi

# find "$DIR" -iname "*mp3" -print0 | while read -d $'\0' file
# do
  # mp3info -r a -p "%f\n bitrate: %r | corrupt frames: %b \n" "$file"
# done


SAVEIFS=$IFS
IFS=$(echo -en "\n\b")
# set me
FILES=$*
for f in $FILES
do
  mp3info -r a -p "%f\n bitrate: %r | corrupt frames: %b \n" "$f"
done
# restore $IFS
IFS=$SAVEIFS

