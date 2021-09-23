import argparse
import os
import subprocess
import sys
import json
import pprint

import pprutils

PROJECT_PATH = os.getcwd()
TESTS_LOC = os.path.join(PROJECT_PATH, "test/tests")
PERF_TESTS_LOC = os.path.join(PROJECT_PATH, "test/perf")
SRC_FILE_EXTENSION = ".hsclox"
INTERPRETER_BIN = "ihsclox"
REGRESSION = "regression"
PERF = "perf"
STANDARD_TEST_TIMEOUT = 3
PERF_TEST_TIMEOUT = 10
ALL = "ALL"
STDOUT_SUFFIX = ".stdout"
PERF_BASELINE_FILE = os.path.join(PERF_TESTS_LOC, "baseline.json")

# =================================================================================
# Instantiate args. do it here to make it global to avoid explicit passing all over
parser = argparse.ArgumentParser()
parser.add_argument( "-u", "--suite"
                    , default=ALL
                    , choices=[REGRESSION, PERF, ALL]
                    , help="test suite to run {regression,perf,ALL}")
parser.add_argument( "-t", "--test"
                    , default=ALL
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

def fullPath(file, isPerf=False):
    return os.path.join(PERF_TESTS_LOC if isPerf else TESTS_LOC, file)

def remove_suffix(text, suffix):
    return text[:-len(suffix)] if text.endswith(suffix) and len(suffix) != 0 else text

def tests():
    result = []
    if (not args.test == ALL): return [args.test]
    for file in os.listdir(TESTS_LOC):
        if file.endswith(SRC_FILE_EXTENSION):
            if args.verbose: print("  " + fullPath(file))
            result.append(remove_suffix(file, SRC_FILE_EXTENSION))
    print(pprutils.bold("Found {} tests.".format(len(result))))
    return result

def perfTests():
    result = []
    for file in os.listdir(PERF_TESTS_LOC):
        if file.endswith(SRC_FILE_EXTENSION):
            if args.verbose: print("  " + fullPath(file, isPerf=True))
            result.append(remove_suffix(file, SRC_FILE_EXTENSION))
    print(pprutils.bold("Found {} perf tests.".format(len(result))))
    return result

def changeExtension(name, oldExt, newExt):
    r = remove_suffix(name, oldExt)
    return r + newExt

def readSnapshot(test):
    filepath = fullPath(test + STDOUT_SUFFIX)
    printV("Reading snapshot from " + filepath, 2)
    with open(filepath, mode='rb') as f:
        return f.read()


def writeSnapshot(test, outcome):
    filepath = fullPath(test + STDOUT_SUFFIX)
    with open(filepath, "w+b") as f:
        f.write(outcome)

def execTestPgm(interp, test, timeout, isPerf=False):
    srcPath = fullPath(test + SRC_FILE_EXTENSION, isPerf)
    printV("Source path: "+ srcPath, 2)
    return subprocess.check_output(
        [interp, srcPath],
        stderr=subprocess.PIPE,
        timeout=timeout)

def runTest(interp, test):
    printV("Running " + test, 2)
    try:   outcome = execTestPgm(interp, test, STANDARD_TEST_TIMEOUT, isPerf=False)
    except subprocess.CalledProcessError as e:
        outcome=e.stderr
    if (args.snapshot):
        print("Updating snapshot for " + test)
        writeSnapshot(test, outcome)
        return
    snapshot = readSnapshot(test)
    if (not snapshot == outcome):
        print(pprutils.uhoh("{} failed.".format(test)))
        sys.exit(1)

def runTests(tests):
    interp = os.path.join(PROJECT_PATH, INTERPRETER_BIN)
    printV(pprutils.bold("Interpreter path: "+ interp))
    for t in tests: runTest(interp, t)
    print(pprutils.good("All tests pass."))

def get_baseline():
    with open(PERF_BASELINE_FILE) as f:
        return json.load(f)


def runPerfTest(interp, test, baselineLookup):
    global args
    printV("Running " + test, 2)

    try:   outcome = execTestPgm(interp, test, PERF_TEST_TIMEOUT, isPerf=True)
    except subprocess.CalledProcessError as e:
        outcome=e.stderr

    test_runtime = outcome.splitlines()[-1]
    # lookup baseline metrics for this test
    baseline = baselineLookup[test]
    delta = float(test_runtime) / baseline["time"] - 1

    pretty_delta = round(delta * 100, 1)
    if args.verbose:
        if delta < 0:
            printV("    runtime => " + pprutils.good(str(pretty_delta) + "%"))
        else:
            printV("    runtime => " + pprutils.warn("+" + str(pretty_delta) + "%"))

    if delta > baseline["threshold"]:
        print(pprutils.uhoh("Performance regression for " + test))
        sys.exit(1)

def runPerfTests(tests):
    interp = os.path.join(PROJECT_PATH, INTERPRETER_BIN)
    printV(pprutils.bold("Interpreter path: "+ interp))

    baselineLookup = get_baseline()

    for t in tests: runPerfTest(interp, t, baselineLookup)
    print(pprutils.good("All perf tests pass."))

def printHerald():
    print(pprutils.hdr("Running test suite.{}".format((" [VERBOSE]" if args.verbose else ""))))
    print(pprutils.bold("Executing test" +
        ((" '" + args.test + SRC_FILE_EXTENSION + "'")
            if args.test != ALL
            else "s")))

def run():
    printHerald()
    if args.suite in [REGRESSION, ALL]: runTests(tests())
    if args.suite in [PERF, ALL]:       runPerfTests(perfTests())

# Execute
run()
