#!/usr/bin/python3

import glob
import os
import shutil
import subprocess
import sys

def run_test(i):
    print('test ', i)
    result = subprocess.run("time sph-exe -ts -n 1000 +RTS -N4 -s", shell=True)
    return result.returncode

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

def main():
    print('running tester...')
    do_make()
    for i in range(5):
        run_test(i)


if __name__ == '__main__':
    sys.exit(main())
