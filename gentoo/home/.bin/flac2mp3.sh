#! /bin/bash

find . -type f -name "*.flac" -print0 | while read -d $'\0' a
do
    OUTF=${a%.flac}.mp3
    CUEF=${a%.flac}.cue

    ARTIST=`metaflac "$a" --show-tag=ARTIST | sed s/.*=//g`
    TITLE=`metaflac "$a" --show-tag=TITLE | sed s/.*=//g`
    ALBUM=`metaflac "$a" --show-tag=ALBUM | sed s/.*=//g`
    GENRE=`metaflac "$a" --show-tag=GENRE | sed s/.*=//g`
    TRACKNUMBER=`metaflac "$a" --show-tag=TRACKNUMBER | sed s/.*=//g`
    DATE=`metaflac "$a" --show-tag=DATE | sed s/.*=//g`

    # VBR
    #flac -c -d "$a" | lame -m j -q 0 --vbr-new -V 0 -s 44.1 - "$OUTF"
    # Preset
    flac -c -d "$a" | lame --preset insane - "$OUTF"
    if test -e "$CUEF"; then
      mp3splt -f -c "$CUEF" "$OUTF"
      # Not necessary without VBR
      # for i in *mp3; do
        # vbrfixc "$i" "$i"
      # done
      # rm vbrfix.tmp
      rm "$OUTF"
    else
      id3 -t "$TITLE" -T "${TRACKNUMBER:-0}" -a "$ARTIST" -A "$ALBUM" -y "$DATE" -g "${GENRE:-12}" "$OUTF"
    fi
done
