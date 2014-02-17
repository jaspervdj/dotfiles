#!/usr/bin/python
import subprocess

def get_gmail_password(account):
    return subprocess.check_output(
            ['security', 'find-internet-password', '-w',
                    '-r', 'imap',
                    '-a', account,
                    '-s', 'imap.gmail.com'],
            stderr = subprocess.STDOUT).rstrip('\n')
