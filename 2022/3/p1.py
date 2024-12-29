from sys import stdin
from collections import Counter

def shared(line):
    chars = int(len(line) / 2)
    left = { char for char in line[:chars] }
    right = { char for char in line[chars:] }
    intersection = left.intersection(right)
    return next(iter(intersection))

def priority(item):
    ch = ord(item)
    return ch - ord("a") + 1 if "a" <= item <= "z" else ch - ord("A") + 27

print(sum([priority(shared(line.strip())) for line in stdin if line != ""]))
