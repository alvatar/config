#!/bin/sh

if test $# -gt 0
then
  mplayer `grep -o "http://.*$" $1`
else
  echo "Usage: playpls <file-containing-url>"
fi
