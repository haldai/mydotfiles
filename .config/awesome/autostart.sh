#!/usr/bin/env bash

function run {
    if ! pgrep $1 ; then
        $@&
    fi
}

run compton --config ~/.config/compton.conf
run nm-applet
run fcitx -d
run volumeicon
run caffeine
run udiskie -ans
run blueman-applet
run urxvtd
