#!/usr/bin/python2

import keyring

def get_pass_work():
    return keyring.get_password("mail", "work")

def get_pass_personal():
    return keyring.get_password("mail", "personal")
