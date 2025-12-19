#!/bin/bash
STATE_FILE="/tmp/color-temp-state"

if [ -f "$STATE_FILE" ] && [ "$(cat $STATE_FILE)" = "warm" ]; then
    gammastep -P -O 6500 -m randr
    echo "normal" > "$STATE_FILE"
else
    gammastep -P -O 3000 -m randr
    echo "warm" > "$STATE_FILE"
fi
