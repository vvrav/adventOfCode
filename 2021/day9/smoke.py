

import math
from typing import List, Set, Tuple

def read_file(fn: str) -> List[List[int]]:
    map = []
    with open(fn) as f:
        for line in f:
            map.append([int(c) for c in list(line) if c != '\n'])
    return map

def get_neighbors(map: List[List[int]], i,j):
    neighbors: List[Tuple[int, int]] = []
    if i > 0: neighbors.append((i-1, j))
    if j > 0: neighbors.append((i, j-1))
    if i < len(map) -1: neighbors.append((i+1, j))
    if j < len(map[i]) -1: neighbors.append((i, j+1))
    return neighbors

def get_coldpoints(map) -> List[Tuple[int, int]]:
    coldpoints = []
    for i in range(len(map)):
        for j in range(len(map[i])):
          neighbors = get_neighbors(map, i, j)
          if all(map[i][j] < map[x][y] for (x, y) in neighbors):
              coldpoints.append((i,j))
    return coldpoints

def get_basins(map) -> List[Set[Tuple[int, int]]]:
    cp = get_coldpoints(map)
    basins = []
    for (i, j) in cp:
        basin = set([(i,j)])
        nnb = [(a,b) for (x,y) in basin for (a,b) in get_neighbors(map, x, y) if (a,b) not in basin and map[a][b]<9]
        while len(nnb) > 0:
            basin |= set(nnb)
            nnb = [(a,b) for (x,y) in basin for (a,b) in get_neighbors(map, x, y) if (a,b) not in basin and map[a][b]<9]
        basins.append(basin)
    return basins

def run1(fn):
    map = read_file(fn)
    coldpoints = get_coldpoints(map)
    return sum(map[x][y]+1 for (x,y) in coldpoints)

def run2(fn):
    map = read_file(fn)
    basins = get_basins(map)
    sizes = sorted([len(s) for s in basins])
    return math.prod(sizes[-3:])
