#!/usr/local/bin/python3

import glob
import os
import shutil
import subprocess
import sys
import re
import statistics
import json
from tqdm import tqdm

NUM_TESTS = 5

def run_test(n, c, threads):
    cmd = "(time sph-exe -t -n {} -c {} +RTS -N{}) > timekeeper.log 2>&1".format(n, c, threads)
    result = subprocess.run(cmd, shell=True)
    if result.returncode != 0:
        raise Exception("Failed subprocess")
    with open("timekeeper.log") as f:
        for line in f:
            if "real" in line:
                m = re.search('real\t0m([^s]*)', line)
                return float(m.group(1))

def do_make():
    os.chdir('../')
    subprocess.run('stack install', shell=True)
    os.chdir('test/')

# def main_single():
#     print('running grader...')
#     filepath = sys.argv[1]
#     filename = os.path.basename(filepath)
#     dest = './llvm/lib/Transforms/PLT/'
#     shutil.copy(filepath, dest)
#     os.rename(dest + filename, dest + 'hw4-callgraph.cpp')
#     do_make()
#     run_test(filename)
#     run_dot()
#     cleanup()
    #shutil.move(f, "done/" + filename)

def seq_test(n):
    times = []
    realtimes = []
    for i in tqdm(range(NUM_TESTS)):
        cmd = "(time sph-exe -ts -n {} +RTS -N1) > timekeeper.log 2>&1".format(n)
        result = subprocess.run(cmd, shell=True)
        if result.returncode != 0:
            raise Exception("Failed subprocess")
        with open("timekeeper.log") as f:
            for line in f:
                if "real" in line:
                    m = re.search('real\t0m([^s]*)', line)
                    times.append(float(m.group(1)))
    print("avg: ", statistics.mean(times))
    print("stddev: ", statistics.stdev(times))

def main():
    nps = 500
    print('running tester...')
    do_make()
    #seq_test(nps)
    tests = {} # map: cores -> chunks -> test
    cores = [2, 4, 8, 12, 16]
    chunks = [2, 4, 8, 12, 16, 20, 25, 35, 45, 60, 75, 90, 120, 166, 250]
    stat = {}
    for c in tqdm(cores, desc='cores'):
        tests[c] = {}
        stat[c] = {}
        for chks in tqdm(chunks, desc='chunks'):
            tests[c][chks] = []
            for i in range(NUM_TESTS):
                tests[c][chks].append(run_test(nps, chks, c))
            stat[c][chks] = (statistics.mean(tests[c][chks]),
                             statistics.stdev(tests[c][chks]))

    tsave = json.dumps(tests)
    ssave = json.dumps(stat)
    with open("test2.json","w") as f:
        f.write(tsave)
    with open("stat2.json","w") as f:
        f.write(ssave)

if __name__ == '__main__':
    sys.exit(main())
