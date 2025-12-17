#!/bin/bash
HOUR=$(date +%H)
if [ $HOUR -ge 22 ] || [ $HOUR -lt 8 ]; then
    gammastep -P -O 3000 -m randr
else
    gammastep -P -O 6500 -m randr
fi
