#!/usr/bin/python

import imaplib
import email
import datetime
import keyring

def process_mailbox(M):
    rv, data = M.search(None, "(UNSEEN)")
    if rv != 'OK':
        print("No messages found!")
        return

    for num in data[0].split():
        rv, data = M.fetch(num, '(BODY.PEEK[])')
        if rv != 'OK':
            print("ERROR getting message", num)
            return
        if type(data[0][1]) == bytes:
            msg = email.message_from_bytes(data[0][1])
        else:
            msg = email.message_from_string(data[0][1])
        print('\nFrom:', msg['From'])
        print('Subject: %s' % (msg['Subject']))
        date_tuple = email.utils.parsedate_tz(msg['Date'])
        if date_tuple:
            local_date = datetime.datetime.fromtimestamp(email.utils.mktime_tz(date_tuple))
            print("Local Date:", local_date.strftime("%a, %d %b %Y %H:%M:%S"))
            # with code below you can process text of email
            # if msg.is_multipart():
            #     for payload in msg.get_payload():
            #         if payload.get_content_maintype() == 'text':
            #             print  payload.get_payload()
            #         else:
            #             print msg.get_payload()

        print('〰〰〰〰〰〰〰〰〰〰〰〰〰〰〰〰〰〰〰〰')
M = imaplib.IMAP4_SSL("outlook.office365.com")
M.login("wdai2@ic.ac.uk", keyring.get_password("work_mail", "daiwz"))

rv, data = M.select("INBOX")
if rv == 'OK':
    process_mailbox(M)
M.close()
M.logout()
