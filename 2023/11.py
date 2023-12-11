#!/usr/bin/env python3
from itertools import combinations
from math import atan2, cos, sin
from sys import stdin, stdout
from typing import Iterator

NO = 0
ABOVE = 1
BELOW = 2

def map_expansion(Q: set[int], expansion: int) -> tuple[dict[int], int]:
  mapped = {}
  last = None
  off = 0
  for i in Q:
    if last is not None and i != last + 1:
      off += expansion
    mapped[i] = i + off
    last = i
  return mapped, last + off + 1


def distance(a: tuple[int, int], b: tuple[int, int]) -> int:
  return abs(a[0] - b[0]) + abs(a[1] - b[1])


def sign(x: int) -> int:
  return (x > 0) - (x < 0)


def line(a: tuple[int, int], b: tuple[int, int]) -> Iterator[tuple[int, int]]:
  # https://gamedev.stackexchange.com/a/182143
  difx = b[0] - a[0]
  dify = b[1] - a[1]
  xstep = sign(difx)
  ystep = sign(dify)
  dist = abs(difx) + abs(dify)
  angle = atan2(-dify, difx)
  tmaxx = 0
  tmaxy = 0
  tdx = 1 / cos(angle)
  tdy = 1 / sin(angle)
  x = a[0]
  y = a[1]
  for _ in range(dist + 1):
    yield x, y
    if abs(tmaxx) < abs(tmaxy):
      tmaxx += tdx
      x += xstep
    else:
      tmaxy += tdy
      y += ystep


def line_length(a: tuple[int, int], b: tuple[int, int]) -> int:
  return abs(b[0] - a[0]) + abs(b[1] - a[1])


def lookup_value(dict, value):
  return list(dict.keys())[list(dict.values()).index(value)]


class Universe:
  def __init__(self, mapstr: str, expansion: int):
    expansion_addition = expansion - 1
    xs = set()
    ys = set()
    galaxies = []
    for (y, row) in enumerate(mapstr.strip().split('\n')):
      for (x, cell) in enumerate(row):
        if cell == '#':
          xs.add(x)
          ys.add(y)
          galaxies.append((x, y))
    xmap, self.width = map_expansion(xs, expansion_addition)
    ymap, self.height = map_expansion(ys, expansion_addition)
    self.galaxies = {
      (xmap[x], ymap[y]): i + 1
      for (i, (x, y)) in enumerate(galaxies)
    }

  def display(self, galaxies=ABOVE, highlight=[], stream=stdout):
    galaxy_numbers = len(self.galaxies) < 10
    for y in range(self.height):
      for x in range(self.width):
        if galaxies != NO:
          try:
            galaxy = self.galaxies[(x, y)]
            if galaxy_numbers:
              galaxy = str(galaxy)
            else:
              galaxy = '#'
          except KeyError:
            galaxy = None
        else:
          galaxy = None

        if galaxies == ABOVE and galaxy is not None:
          stream.write(str(galaxy))
          continue

        for (group, sym) in highlight:
          if (x, y) in group:
            stream.write(sym)
            break
        else:
          if galaxies == NO or galaxy is None:
            stream.write('.')
          else:
            stream.write(str(galaxy))
      stream.write('\n')

  def galaxy_pairs(self) -> set[tuple[tuple[int, int], tuple[int, int]]]:
    return set(combinations(self.galaxies, 2))


def main():
  input = stdin.read()

  universe = Universe(input, 2)
  if len(universe.galaxies) < 10:
    # Example input
    path = line(lookup_value(universe.galaxies, 5),
                lookup_value(universe.galaxies, 9))
    path = list(path)
    universe.display(
      #galaxies=BELOW,
      highlight=[
        (set(path), "\x1b[33m*\x1b[m"),
      ],
    )

  length_sum = 0
  for pair in universe.galaxy_pairs():
    length_sum += line_length(*pair)
  print(f"The sum of the path lengths in the young universe is \x1b[92m{length_sum}\x1b[m")

  old_universe = Universe(input, 1000000)
  length_sum = 0
  for pair in old_universe.galaxy_pairs():
    length_sum += line_length(*pair)
  print(f"The sum of the path lengths in the old universe is \x1b[92m{length_sum}\x1b[m")

if __name__ == "__main__":
  main()
