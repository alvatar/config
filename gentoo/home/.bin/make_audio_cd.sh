#!/bin/zsh

# Simple script to make an audio CD from current mp3 folder

ls "*wav" &> /dev/null
if test 0 -eq $?
then
  echo "Please check that there are no wav files already in the directory"
  exit 1
fi

for i in *mp3; do mpg123 -w ${i/mp3/wav} ${i}; done
cdrecord  -eject -pad -audio *wav

for i in *mp3
do
  rm ${i/mp3/wav}
done

