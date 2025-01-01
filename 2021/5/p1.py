from sys import stdin

map = [[0 for _ in range(1000)] for _ in range(1000)]

def birange(a, b): return range(a, b + 1) if a < b else range(b, a + 1)

for line in stdin:
    [[x1, y1], [x2, y2]] = [[int(x) for x in point.strip().split(',')] for point in line.split('->')]
    if x1 == x2:
        for y in birange(y1, y2):
            map[x1][y] += 1
    elif y1 == y2:
        for x in birange(x1, x2):
            map[x][y1] += 1

overlaps = len([cell
    for column in map
    for cell in column
    if cell > 1])

print(overlaps)
