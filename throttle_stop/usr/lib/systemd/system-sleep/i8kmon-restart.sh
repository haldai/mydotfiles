#!/usr/bin/env sh

if [ "${1}" == "post" ]; then
    systemctl restart i8kmon
fi
