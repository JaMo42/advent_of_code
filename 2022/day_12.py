#!/usr/bin/env python3
from sys import stdin, stdout
from collections import defaultdict
from sys import maxsize as INFINITY
from dataclasses import dataclass
from functools import cache

class Heightmap:
  def __init__ (self, map: list[list[int]], start: tuple[int, int], end: tuple[int, int]):
    self._map = map
    self._height = len (map)
    self._width = len (map[0])
    self._start = start
    self._end = end

  @classmethod
  def from_string (Self: type, string: str) -> 'Heightmap':
    lines = string.split ('\n')
    map = [[0]*len(lines[0]) for _ in range (len (lines))]
    start = None
    end = None
    for y in range (len (lines)):
      for x in range (len (lines[0])):
        match lines[y][x]:
          case 'S':
            map[y][x] = 0
            start = (x, y)
          case 'E':
            map[y][x] = 25
            end = (x, y)
          case h:
            map[y][x] = ord (h) - ord ('a')
    return Self (map, start, end)

  def print (self):
    def color (x: int, y: int) -> str:
      match (x, y):
        case self._start:
          return "\x1b[1;34m"
        case self._end:
          return "\x1b[1;31m"
        case p:
          h = self[p]
          c = h / 25 * 128
          return "\x1b[38;2;{0};{0};{0}m".format (127 + int (c))
    for y in range (self._height):
      for x in range (self._width):
        stdout.write (f"{color (x, y)}{self._map[y][x]:>2}\x1b[0m")
      stdout.write ('\n\x1b[0m')

  def __getitem__ (self, xy: tuple[int, int]) -> int:
    x, y = xy
    if x not in range (self._width) or y not in range (self._height):
      return 100
    return self._map[y][x]

  @cache
  def possible_moves_at (self, x: int, y: int) -> tuple[tuple[int, int], ...]:
    return tuple (filter (
      lambda p: self[p] - self[x, y] <= 1,
      map (
        lambda o: (x+o[0], y+o[1]),
        ((0, -1), (1, 0), (0, 1), (-1, 0))
      )
    ))

  def start (self) -> tuple[int, int]:
    return self._start

  def end (self) -> tuple[int, int]:
    return self._end


class AStar:
  @dataclass
  class Info:
    f: float = INFINITY
    g: int = INFINITY

  def __init__ (self):
    self._info = defaultdict (self.Info)

  def reset (self):
    self._info.clear ()

  @staticmethod
  @cache
  def heuristic (node: tuple[int, int], end: tuple[int, int]) -> float:
    D1 = 1.0
    D2 = 1.414
    dx = abs (node[0] - end[0])
    dy = abs (node[1] - end[1])
    return D1 * (dx + dy) + (D2 - 2 * D1) * min (dx, dy)

  def solve (self, map: Heightmap, start: tuple[int, int], end: tuple[int, int]) -> int:
    open_set = set ([start])
    self._info[start] = self.Info (0, 0)
    while open_set:
      current = min (open_set, key=lambda x: self._info[x].f)
      if current == end:
        break
      open_set.remove (current)
      for n in map.possible_moves_at (*current):
        g = self._info[current].g + 1
        if g < self._info[n].g:
          self._info[n].g = g
          self._info[n].f = g + self.heuristic (n, end)
          open_set.add (n)
    return self._info[end].g


def get_starting_points (heightmap: Heightmap):
  # Find all possible starting points, due to the nature of the input these
  # are always inside the same basin the starting point lies in.
  points = list ()
  seen = defaultdict (bool)
  def impl (x: int, y: int):
    for p in heightmap.possible_moves_at (x, y):
      if heightmap[p] == 0 and p not in seen:
        points.append (p)
        seen[p] = True
        impl (*p)
  impl (*heightmap.start ())
  return points


def main ():
  heightmap = Heightmap.from_string (stdin.read ().strip ())
  heightmap.print ()
  pathfinder = AStar ()
  print ("Shortest path from start:", pathfinder.solve (heightmap, heightmap.start (), heightmap.end ()))

  length = INFINITY
  for start in get_starting_points (heightmap):
    # Seems to not be neccessary, at least for my input
    #pathfinder.reset ()
    length = min (length, pathfinder.solve (heightmap, start, heightmap.end ()))
  print ("Shortest possible:", length)


if __name__ == "__main__":
  main ()
