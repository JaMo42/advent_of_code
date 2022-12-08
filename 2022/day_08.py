#!/usr/bin/env python3
from sys import stdin, stdout
from dataclasses import dataclass
from itertools import permutations

class HeightMap:
  def __init__ (self, map):
    self._map = tuple (
      tuple (
        int (cell)
        for cell in row
      )
      for row in map
    )
    assert len (self._map) == len (self._map[0])
    self._size = len (self._map)

  @classmethod
  def from_file (cls, file) -> 'HeightMap':
    m = []
    for line in file.read ().strip ().split ('\n'):
      m.append (map (int, list (line)))
    return cls (m)

  def print (self):
    DARKEST = 96
    color = lambda cell: "\x1b[38;2;{0};{0};{0}m".format (
      round ((cell / 10) * (255 - DARKEST) + DARKEST)
    )
    for row in self._map:
      for cell in row:
        stdout.write (color (cell) + f"{cell}\x1b[0m")
      stdout.write ('\n')
    stdout.flush ()

  def __getitem__ (self, xy):
    return self._map[xy[1]][xy[0]]

  def count_visible (self) -> int:
    # Need to keep track of visible instead of just counting since trees can be
    # visible from multiple directions.
    visible = [[False]*self._size for _ in range (self._size)]

    def visit (x, y, height):
      cell = self[x, y]
      if cell <= height:
        return height
      visible[y][x] = True
      return cell

    def walk (range, make_index):
      height = -1
      for i in range:
        height = visit (*make_index (i), height)
        # Nothing after a 9 can be visible
        if height == 9:
          break

    for outer in range (self._size):
      # outer is row:
      walk (range (self._size), lambda col: (col, outer))
      walk (range (self._size - 1, -1, -1), lambda col: (col, outer))
      # outer is column:
      walk (range (self._size), lambda row: (outer, row))
      walk (range (self._size - 1, -1, -1), lambda row: (outer, row))

    return sum (map (lambda r: r.count (True), visible))

  def scenic_score (self, x: int, y: int) -> int:
    h = self[x, y]
    def get_distance (range, make_index) -> int:
      distance = 0
      for i in range:
        distance += 1
        if self[make_index (i)] >= h:
          break
      return distance
    return (  get_distance (range (y - 1, -1, -1), lambda row: (x, row))
            * get_distance (range (y + 1, self._size), lambda row: (x, row))
            * get_distance (range (x - 1 , -1, -1), lambda col: (col, y))
            * get_distance (range (x + 1, self._size), lambda col: (col, y)))

  def highest_scenic_score (self):
    return max (
      map (
        lambda xy: self.scenic_score (*xy),
        permutations (range (self._size), 2)
      )
    )

def main ():
  map = HeightMap.from_file (stdin)
  map.print ()
  print ("Trees visible from the edge:", map.count_visible ())
  print ("Highest scenic score:", map.highest_scenic_score ())

if __name__ == "__main__":
  main ()
