#!/usr/bin/env python3
import sys
from sys import maxsize as INFINITY
from collections import defaultdict

NEIGHBORS = ((0, -1), (1, 0), (0, 1), (-1, 0))

class AStar:
  # Cost to move straight
  STRAIGHT_COST = 1.0
  # Cost to move diagonally,
  # this is still needed even though we can't move diagonally to determine the
  # f-score of a node
  DIAGONAL_COST = 1.414

  @staticmethod
  def heuristic (node, end):
    D1 = AStar.STRAIGHT_COST
    D2 = AStar.DIAGONAL_COST
    dx = abs (end[0] - node[0])
    dy = abs (end[1] - node[1])
    return D1 * (dx + dy) + (D2 - 2 * D1) * min (dx, dy)

  def __init__ (self, grid):
    self._grid = grid
    self._width = len (grid[0])
    self._height = len (grid)
    self._meta = None

  def gscore (self, node, v=None):
    if v is not None:
      self._meta[node]['g'] = v
    else:
      return self._meta[node]['g']

  def fscore (self, node, v=None):
    if v is not None:
      self._meta[node]['f'] = v
    else:
      return self._meta[node]['f']

  def prev (self, node, v=None):
    if v is not None:
      self._meta[node]['prev'] = v
    else:
      return self._meta[node]['prev']

  def neighbors (self, node):
    x, y = node
    return filter (lambda n: (n[0] in range (self._width)
                              and n[1] in range (self._height)),
                   ((x,y-1), (x+1, y), (x,y+1), (x-1,y)))

  def cost (self, node, to):
    return self._grid[to[1]][to[0]]

  def get_path_risk (self, start=(0, 0), goal=None):
    if goal is None:
      goal = (self._width - 1, self._height - 1)
    open_set = set ([start])
    self._meta = defaultdict (lambda: {'f': INFINITY, 'g': INFINITY})
    self._meta[start] = {'f':0, 'g':0}

    while open_set:
      current = min (open_set, key=lambda x: self._meta[x]['f'])
      if current == goal:
        break
      open_set.remove (current)
      for neighbor in self.neighbors (current):
        cost = self.cost (current, neighbor)
        gscore = self.gscore (current) + cost
        if gscore < self.gscore (neighbor):
          self.gscore (neighbor, gscore)
          self.fscore (neighbor, gscore + AStar.heuristic (neighbor, goal))
          open_set.add (neighbor)

    return self.gscore (goal)


def build_full_map (original, factor=5):
  OW, OH = len (original[0]), len (original)
  W, H = OW*factor, OH*factor

  full = [[0 for i in range (W)] for i in range (H)]

  for r in range (H):
    for c in range (W):
      full[r][c] = original[r%OH][c%OW] + (r//OH) + (c//OW)
      while full[r][c] > 9:
        full[r][c] -= 9

  return full


def get_risk (cave, show_path=True):
  pathfinder = AStar (cave)
  return pathfinder.get_path_risk ()


def main ():
  stdin = sys.stdin.read ().strip ().split ('\n')
  cave = [[int (col) for col in row] for row in stdin]
  the_cooler_cave = build_full_map (cave)

  small_risk = get_risk (cave)
  big_risk = get_risk (the_cooler_cave, False)

  print (f"Total risk for the small cave: \x1b[92m{small_risk}\x1b[0m")
  print (f"Total risk for the big cave:   \x1b[92m{big_risk}\x1b[0m")


if __name__ == "__main__":
  main ()

