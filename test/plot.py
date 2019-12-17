#!/usr/local/bin/python3

import matplotlib.pyplot as plt
import json
import sys

def plotter(stats):
    for core, stat in stats.items():
        cleanstat = {int(k):v for k,v in stat.items()}
        plt.title("{}-core SPH".format(core))
        ls = sorted(cleanstat.items())
        x, y = zip(*ls)
        y = [i[0] for i in y]
        plt.plot(x, y)
        plt.show()

def main():
    with open("stat.json") as f:
        stats = json.load(f)
        plotter(stats)

if __name__ == '__main__':
    sys.exit(main())
