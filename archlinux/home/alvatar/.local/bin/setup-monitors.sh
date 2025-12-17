#!/bin/bash

# Check if DP-2 is connected
if xrandr | grep "^DP-2 connected"; then
    # DP-2 is connected, set it up with scaling
    xrandr --output DP-2 --mode 2880x1800 --scale 1.30x1.30 --pos 0x0
    xrandr --output eDP-1 --mode 3840x2400 --pos 3840x0
else
    # DP-2 not connected, just use eDP-1
    xrandr --output eDP-1 --mode 3840x2400 --pos 0x0
fi
