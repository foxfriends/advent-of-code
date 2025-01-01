from sys import stdin

map = [[0 for _ in range(1000)] for _ in range(1000)]

def signum(x):
    if x < 0: return -1
    if x > 0: return 1
    return 0

def point_range(x1, y1, x2, y2):
    sx = signum(x2 - x1)
    sy = signum(y2 - y1)
    yield [x1, y1]
    while x1 != x2 or y1 != y2:
        x1 += sx
        y1 += sy
        yield [x1, y1]

for line in stdin:
    [[x1, y1], [x2, y2]] = [[int(x) for x in point.strip().split(',')] for point in line.split('->')]
    for [x, y] in point_range(x1, y1, x2, y2):
        map[x][y] += 1

overlaps = len([cell
    for column in map
    for cell in column
    if cell > 1])

print(overlaps)
