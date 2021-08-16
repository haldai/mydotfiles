#!/usr/bin/env bash

function run {
    if ! pgrep $1 ; then
        $@&
    fi
}

# Startups
run nosleep on
# run no_lid_suspend
run /usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1
# run picom --experimental-backends --config ~/.config/compton.conf
run picom --config ~/.config/compton.conf
run udiskie -ans
run /home/daiwz/.scripts/nodimming.sh
run blueman-applet
run libinput-gestures-setup start
# run ss-qt5
# run emacs --daemon
run solaar --window=hide
run fcitx5
run /home/daiwz/.conky/myconky-start.sh
run minidlnad -f /home/daiwz/.config/minidlna/minidlna.conf -P /home/daiwz/.config/minidlna/minidlna.pid

# Settings
# xbacklight -set 50
amixer set Master 20% off
amixer set Headphone 100% on
amixer set Speaker 100% off
amixer set Capture 100% nocap
