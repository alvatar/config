#!/bin/bash
# Get the connected external display (anything that's not eDP-1)
EXTERNAL=$(xrandr | grep " connected" | grep -v "eDP-1" | awk '{print $1}')
if [ -n "$EXTERNAL" ]; then
    # External display found, set up external on left, laptop on right
    xrandr --output "$EXTERNAL" --mode 2880x1800 --scale 1.30x1.30 --pos 0x0 \
           --output eDP-1 --mode 3840x2400 --pos 3744x0
else
    # No external display, just use laptop
    xrandr --output eDP-1 --mode 3840x2400 --pos 0x0
fi
