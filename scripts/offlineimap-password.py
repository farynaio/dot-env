import os
import subprocess

def mailpasswd(acct):
  acct = os.path.basename(acct)
  path = "/Users/devil/.passwd/%s.gpg" % acct
  args = ["gpg", "--use-agent", "--quiet", "--batch", "-d", path]
  try:
    return subprocess.check_output(args).strip()
  except subprocess.CalledProcessError:
    return ""
