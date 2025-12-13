#!/bin/bash

# Replace with your actual laptop keyboard name from xinput list
LAPTOP_KB="AT Translated Set 2 keyboard"

# Get the device ID
DEVICE_ID=$(xinput list | grep "$LAPTOP_KB" | grep -o 'id=[0-9]*' | grep -o '[0-9]*')

if [ -n "$DEVICE_ID" ]; then
    # Swap Caps Lock and Control for this device
    setxkbmap -device $DEVICE_ID -option ctrl:swapcaps
    echo "Swapped Caps Lock and Control for laptop keyboard (ID: $DEVICE_ID)"
else
    echo "Laptop keyboard not found"
fi
