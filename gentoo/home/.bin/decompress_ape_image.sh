#!/bin/bash

LAME_PARS="--alt-preset insane"

echo "Script ape2mp3"
echo
echo

for i in $*; do
case $i in
*.[aA][pP][eE])
echo "Processing file $i...";;
*)
echo "Warning: File $i don't have .ape extension. Ommiting..."
continue
esac

FILENAME="$(basename $i)"
FILENAME="${FILENAME%.[aA][pP][eE]}"

#nice -n19 java -jar ../jmac-1.74/distributables/jmac.jar d $i $FILENAME.wav
shntool conv -o wav "$i" "$FILENAME.wav"

nice -n19 lame $LAME_PARS $FILENAME.wav $FILENAME.mp3
rm $FILENAME.wav
if [ -e $FILENAME.cue ]; then
mp3splt -f -c $FILENAME.cue -o @n+-+@t $FILENAME.mp3
rm $FILENAME.mp3
fi
done
