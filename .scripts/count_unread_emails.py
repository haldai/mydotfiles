#!/usr/bin/python

import imaplib
import re
import keyring

M = imaplib.IMAP4_SSL("outlook.office365.com")
M.login("wdai2@ic.ac.uk", keyring.get_password("work_mail", "daiwz"))

status, counts = M.status("INBOX","(MESSAGES UNSEEN)")

if status == "OK":
	unread = re.search(r'UNSEEN\s(\d+)', counts[0].decode('utf-8')).group(1)
else:
	unread = "N/A"

print(unread)
