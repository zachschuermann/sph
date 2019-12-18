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
            if "elapse" in line:
                m = re.search('0:([^el]*)', line)
                m1 = re.search('([^user]*)', line)
                m2 = re.search('user ([^sys]*)', line)
                break
        return float(m.group(1)), float(m1.group(1)) + float(m2.group(1))

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
    for i in tqdm(range(NUM_TESTS)):
        cmd = "(time sph-exe -ts -n {} +RTS -N1) > timekeeper.log 2>&1".format(n)
        result = subprocess.run(cmd, shell=True)
        if result.returncode != 0:
            raise Exception("Failed subprocess")
        with open("timekeeper.log") as f:
            for line in f:
                if "elapse" in line:
                    m = re.search('0:([^el]*)', line)
                    times.append(float(m.group(1)))
    print("avg: ", statistics.mean(times))
    print("stddev: ", statistics.stdev(times))

def main():
    nps = 1000
    print('running tester...')
    do_make()
    seq_test(nps)

    cores = [2, 3, 4, 5, 6]
    chunks = [2, 4, 8, 12, 16, 20, 25, 35, 45, 60, 75, 90, 120, 150, 175, 200, 250,
              300, 333, 400, 500]

    tests = {} # map: cores -> chunks -> test
    stat = {}
    totals = {}
    for c in tqdm(cores, desc='cores'):
        tests[c] = {}
        stat[c] = {}
        totals[c] = {}
        for chks in tqdm(chunks, desc='chunks'):
            tests[c][chks] = []
            tots = []
            for i in range(NUM_TESTS):
                real, total = run_test(nps, chks, c)
                tests[c][chks].append(real)
                tots.append(total)
            stat[c][chks] = (statistics.mean(tests[c][chks]),
                             statistics.stdev(tests[c][chks]))
            totals[c][chks] = (statistics.mean(tots),
                               statistics.stdev(tots))

    totsave = json.dumps(totals)
    tsave = json.dumps(tests)
    ssave = json.dumps(stat)
    with open("test3.json","w") as f:
        f.write(tsave)
    with open("stat3.json","w") as f:
        f.write(ssave)
    with open("tots3.json","w") as f:
        f.write(totsave)

if __name__ == '__main__':
    sys.exit(main())
