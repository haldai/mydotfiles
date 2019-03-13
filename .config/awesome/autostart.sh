#!/usr/bin/env bash

function run {
    if ! pgrep $1 ; then
        $@&
    fi
}

# Startups
run exec --no-startup-id /usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1
run undervolt.sh
run compton --config ~/.config/compton.conf
run nm-applet
run fcitx -d
run volumeicon
run caffeine
run udiskie -ans
run blueman-applet
run urxvtd
run sbxkb
run libinput-gestures-setup start
run copyq
run sleep 1; /home/daiwz/.conky/myconky-start.sh
