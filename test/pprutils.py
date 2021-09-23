HEADER = '\033[95m'
OKBLUE = '\033[94m'
OKCYAN = '\033[96m'
OKGREEN = '\033[92m'
WARNING = '\033[93m'
FAIL = '\033[91m'
ENDC = '\033[0m'
BOLD = '\033[1m'
UNDERLINE = '\033[4m'

def hdr(s):  return "{}{}{}".format(HEADER,s,ENDC)
def good(s): return "{}{}{}".format(OKGREEN,s,ENDC)
def warn(s): return "{}{}{}".format(WARNING,s,ENDC)
def uhoh(s): return "{}{}{}".format(FAIL,s,ENDC)
def line(s): return "{}{}{}".format(UNDERLINE,s,ENDC)
def bold(s): return "{}{}{}".format(BOLD,s,ENDC)
