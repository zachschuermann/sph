#!/usr/local/bin/python3

import matplotlib.pyplot as plt
import json
import sys

### SERIAL ###
# avg:  29.849999999999998
# stddev:  0.4526035793053339

size = (6, 4)

def plotter(stats):
    for core, stat in stats.items():
        cleanstat = {int(k):v for k,v in stat.items()}
        plt.figure(figsize=size)
        plt.title("{}-core SPH".format(core))
        ls = sorted(cleanstat.items())
        x, y = zip(*ls)
        y = [i[0] for i in y]
        plt.plot(x, y)
        plt.show()


def combined_plotter(stats):
    plt.figure(figsize=size)
    plt.title("multi-core SPH".format(core))
    for core, stat in stats.items():
        cleanstat = {int(k):v for k,v in stat.items()}
        ls = sorted(cleanstat.items())
        x, y = zip(*ls)
        y = [i[0] for i in y]
        plt.plot(x, y)
    plt.show()

def plot_totals(totals, keys):
    tot_runtime = [29.85]
    for core, tot in totals.items():
        cleantot = {int(k):v for k,v in tot.items()}
        plt.figure(figsize=size)
        plt.title("{}-core SPH".format(core))
        ls = sorted(cleantot.items())
        x, y = zip(*ls)
        y = [i[0] for i in y]
        plt.plot(x, y)
        plt.show()
        tot_runtime.append(cleantot[keys[core]][0])
    return tot_runtime

def get_stats(stats):
    speedups = [1]
    runtimes = [29.85]
    keys = {}
    for core, stat in stats.items():
        cleanstat = {int(k):v for k,v in stat.items()}
        key_min = min(cleanstat.keys(), key=(lambda k: cleanstat[k][0]))
        print("{}-core SPH: ".format(core), key_min, "chunks")
        print("{}-core SPH: ".format(core), cleanstat[key_min])
        print("{}-core SPH: ".format(core), 29.85/cleanstat[key_min][0]," speedup")
        print()
        speedups.append(29.85/cleanstat[key_min][0])
        runtimes.append(cleanstat[key_min][0])
        keys[core] = key_min
    return speedups, runtimes, keys

def total_stats(stats):
    for core, stat in stats.items():
        cleanstat = {int(k):v for k,v in stat.items()}
        key_min = min(cleanstat.keys(), key=(lambda k: cleanstat[k][0]))
        print("{}-core SPH: ".format(core), key_min, "chunks")
        print("{}-core SPH: ".format(core), cleanstat[key_min])
        print()

def overall_plot(speedups, runtimes, tot_runtimes):
    cores = range(1, 7)
    # plot cores vs. speedups
    plt.figure(figsize=size)
    plt.title('Empirical vs. Ideal Speedup')
    plt.xlabel('cores')
    plt.ylabel('speedup (s/s)')
    plt.plot(cores, speedups, marker=".")
    plt.plot(cores, cores)
    plt.legend(["Empirical", "Ideal"])
    plt.show()

    plt.figure(figsize=size)
    plt.plot(cores, tot_runtimes)
    plt.show()

    plt.figure(figsize=size)
    plt.title('Cores vs. Runtime')
    plt.xlabel('cores')
    plt.ylabel('runtime (s)')
    plt.plot(cores, runtimes)
    plt.show()

def main():
    with open("stat3.json") as f:
        stats = json.load(f)
        plotter(stats)
        speedups, runtimes, keys = get_stats(stats)

    with open("tots3.json") as f:
        tots = json.load(f)
        tot_runtime = plot_totals(tots, keys)

    overall_plot(speedups, runtimes, tot_runtime)

       
if __name__ == '__main__':
    sys.exit(main())
