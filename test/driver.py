import argparse
import os
import subprocess
import sys

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

PROJECT_PATH = os.getcwd()
TESTS_LOC = os.path.join(PROJECT_PATH, "test/tests")
SRC_FILE_EXTENSION = ".hsclox"
INTERPRETER_BIN = "ihsclox"
ALL_TESTS_MAGIC_WORD = "ALL"
STDOUT_SUFFIX = ".stdout"

# =================================================================================
# Instantiate args. do it here to make it global to avoid explicit passing all over
parser = argparse.ArgumentParser()
parser.add_argument( "-t", "--test"
                    , default=ALL_TESTS_MAGIC_WORD
                    , help="test to be executed. If not specified, all tests are executed")
parser.add_argument( "-b", "--bin"
                    , default=INTERPRETER_BIN
                    , help="compiler/interpreter path")
parser.add_argument( "-s", "--snapshot"
                    , action="store_true"
                    , default=False
                    , help="resnapshot test outcomes")
parser.add_argument( "-v", "--verbose"
                    , action="store_true"
                    , help="compiler/interpreter path")

args = parser.parse_args()
# =================================================================================

def printV(s, indent=0):
    if args.verbose: print("{}{}".format("" if indent==0 else " "*indent, s))

def fullPath(file):
    return os.path.join(TESTS_LOC, file)

def tests():
    result = []
    if (not args.test == ALL_TESTS_MAGIC_WORD): return [args.test]
    for file in os.listdir(TESTS_LOC):
        if file.endswith(SRC_FILE_EXTENSION):
            if args.verbose: print("  " + fullPath(file))
            result.append(file.removesuffix(SRC_FILE_EXTENSION))
    print(bold("Found {} tests.".format(len(result))))
    return result

def changeExtension(name, oldExt, newExt):
    r = name.removesuffix(oldExt)
    return r + newExt

def readSnapshot(test):
    filepath = fullPath(test + STDOUT_SUFFIX)
    printV("Reading snapshot from " + filepath, 2)
    lines = ""
    with open(filepath, mode='rb') as f:
        return f.read()


def writeSnapshot(test, outcome):
    filepath = fullPath(test + STDOUT_SUFFIX)
    with open(filepath, "w+b") as f:
        f.write(outcome)

def execTestPgm(interp, test):
    srcPath = fullPath(test + SRC_FILE_EXTENSION)
    printV("Source path: "+ srcPath, 2)
    return subprocess.check_output(
        [interp, srcPath],
        stderr=subprocess.PIPE,
        timeout=3)

def runTest(interp, test):
    printV("Running " + test, 2)
    try:   outcome = execTestPgm(interp, test)
    except subprocess.CalledProcessError as e:
        outcome=e.stderr
    if (args.snapshot):
        print("Updating snapshot for " + test)
        writeSnapshot(test, outcome)
        return
    snapshot = readSnapshot(test)
    if (not snapshot == outcome):
        print(uhoh("{} failed.".format(test)))
        sys.exit(1)

def runTests(tests):
    interp = os.path.join(PROJECT_PATH, INTERPRETER_BIN)
    printV(bold("Interpreter path: "+ interp))
    for t in tests: runTest(interp, t)
    print(good("All tests pass."))

def printHerald():
    print(hdr("Running test suite.{}".format((" [VERBOSE]" if args.verbose else ""))))
    print(bold("Executing test" +
        ((" '" + args.test + SRC_FILE_EXTENSION + "'")
            if args.test != ALL_TESTS_MAGIC_WORD
            else "s")))

def run():
    printHerald()
    runTests(tests())

# Execute
run()
