#!/usr/local/bin/python3

import time
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
            m = re.search('real\t0m([^s]*)', line)
            m2 = re.search('user\t0m([^s]*)', line)
            m3 = re.search('sys\t0m([^s]*)', line)
        return float(m.group(1)), float(m2.group(1)) + float(m3.group(1))

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
    nps = 1000
    print('running tester...')
    do_make()
    #seq_test(nps)
    tests = {} # map: cores -> chunks -> test
    cores = [2, 4, 8, 12, 16]
    #chunks = [2, 4, 8, 12, 16, 20, 25, 35, 45, 60, 75, 90, 120, 166, 250]
    chunks = [2, 4, 8, 12, 16, 20, 25, 35, 45, 60, 75, 100, 150, 200, 250, 333, 400]
    stat = {}
    totals = {}
    for c in tqdm(cores, desc='cores'):
        tests[c] = {}
        stat[c] = {}
        totals[c] = {}
        for chks in tqdm(chunks, desc='chunks'):
            tests[c][chks] = []
            tot = []
            for i in range(NUM_TESTS):
                real, total = run_test(nps, chks, c)
                tests[c][chks].append(real)
                tot.append(total)
            stat[c][chks] = (statistics.mean(tests[c][chks]),
                             statistics.stdev(tests[c][chks]))
            totals[c][chks] = (statistics.mean(tot),
                               statistics.stdev(tot))

    totsave = json.dumps(totals)
    tsave = json.dumps(tests)
    ssave = json.dumps(stat)
    timestr = time.strftime("%Y%m%d-%H-%M-%S")
    with open("test-" + timestr + ".json","w") as f:
        f.write(tsave)
    with open("stat-" + timestr + ".json","w") as f:
        f.write(ssave)
    with open("total-" + timestr + ".json","w") as f:
        f.write(totsave)

if __name__ == '__main__':
    sys.exit(main())
