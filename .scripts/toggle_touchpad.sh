#!/usr/bin/env bash

TOUCH_NAME='TOPS0101:00 35CC:0102 Touchpad'

TOUCH_ENABLED=$(xinput list-props "$TOUCH_NAME" | grep 'Device Enabled' | awk '{split($0,a,":")k; print(a[2])}' | sed 's/[^0-9]*//g')

echo $TOUCH_ENABLED

if [ $TOUCH_ENABLED -gt 0 ]; then
 xinput disable "$TOUCH_NAME"
else
 xinput enable "$TOUCH_NAME"
fi
