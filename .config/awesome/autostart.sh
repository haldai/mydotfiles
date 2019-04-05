#!/usr/bin/env bash

function run {
    if ! pgrep $1 ; then
        $@&
    fi
}

# Startups
run /usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1
run compton --config ~/.config/compton.conf
run ibus-daemon --xim -d
run nm-applet
run caffeine
run udiskie -ans
run blueman-applet
run libinput-gestures-setup start
run ss-qt5
run xautolock -time 10 -locker slock
run /home/daiwz/.conky/myconky-start.sh

# Settings
xbacklight -set 90
amixer set Master 20% off
amixer set Headphone 100% on
amixer set Speaker 100% off
amixer set Capture 100% nocap
