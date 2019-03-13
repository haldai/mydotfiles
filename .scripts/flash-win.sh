#!/bin/bash

# flashes the active window

# require transset-df and a composite manager
transset-df -a -m 0
sleep .1
transset-df -a -x 1
sleep .1
transset-df -a -m 0
sleep .1
transset-df -a -x 1

