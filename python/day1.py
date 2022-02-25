#!/usr/bin/env python3

def num_prev_larger(path):
    larger = 0
    depths = open(path).read().split("\n")
    prev = depths[0]
    for i in range(len(depths)):
        n = depths[i]
        print("Current number: " + n)
        if n > prev:
            print(n + " is larger than " + prev)
            larger += 1
        prev = n
    return larger

def num_sum_larger(path):
    larger = 0
    depths = open(path).read().split("\n")
    # if I parsed these as ints, I could use sum(depths[0:3])
    # but they're strings, and that doesn't work. I'd rather do this
    prev = depths[0] + depths[1] + depths[2];
    for i in range(len(depths) - 1):
        # not using python index slicing, see previous comment
        current = depths[i] + depths[i+1] + depths[i+2]
        if prev < current:
            larger += 1
        prev = current
    return larger

