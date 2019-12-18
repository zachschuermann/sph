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


def get_stats(stats):
    for core, stat in stats.items():
        cleanstat = {int(k):v for k,v in stat.items()}
        key_min = min(cleanstat.keys(), key=(lambda k: cleanstat[k][0]))
        print("{}-core SPH: ".format(core), key_min, "chunks")
        print("{}-core SPH: ".format(core), cleanstat[key_min])
        print("{}-core SPH: ".format(core), 30/cleanstat[key_min][0]," speedup")
        print()


def main():
    with open("stat.json") as f:
        stats = json.load(f)
        plotter(stats)
        get_stats(stats)

       
if __name__ == '__main__':
    sys.exit(main())
