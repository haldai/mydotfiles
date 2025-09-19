#!/bin/bash

modprobe i2c-dev
su -c '/usr/sbin/i2cset -y 3 0x48 0x2 0 && /usr/sbin/i2cset -y 3 0x48 0x3 0'
