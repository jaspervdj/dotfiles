#!/usr/bin/python
import subprocess

def get_gmail_password(account):
    # `decrypt-password` script is found in bin/
    return subprocess.check_output(
            ['decrypt-password', account, 'imap.gmail.com']).rstrip('\n')

def sync_folder(folder):
    return folder not in [
        '[Gmail]/Drafts',
        '[Gmail]/Important',
        '[Gmail]/Spam']
