#!/usr/bin/env sh

# undervolt settings, require msr-tools from AUR
/usr/bin/wrmsr 0x150 0x80000011EF600000 # cpu -130mV
/usr/bin/wrmsr 0x150 0x80000111F6600000 # gpu -75mV
/usr/bin/wrmsr 0x150 0x80000211F1E00000 # cache -110mV
