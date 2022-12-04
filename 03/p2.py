from sys import stdin
from collections import Counter

def shared(lines):
    intersection = set.intersection(*[set(line) for line in lines])
    return next(iter(intersection))

def priority(item):
    ch = ord(item)
    return ch - ord("a") + 1 if "a" <= item <= "z" else ch - ord("A") + 27

lines = [line.strip() for line in stdin if line != ""]
print(sum([priority(shared(lines[i:i + 3])) for i in range(0, len(lines), 3)]))
