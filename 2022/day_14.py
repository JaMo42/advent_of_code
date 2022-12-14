#!/usr/bin/env python3
from sys import stdin, stdout
from sys import maxsize as LARGE_NUMBER

class World:
  POURING_POINT = (500, 0)

  def __init__ (self):
    # Sand and rocks are treated the same since placed sand doesn't move.
    self._objects = set[tuple[int, int]] ()
    # We still keep track or what's a rock for nicer printing
    self._rocks = frozenset ()
    self._min_x = LARGE_NUMBER
    self._max_x = 0
    self._min_y = LARGE_NUMBER
    self._max_y = 0
    self._floor = None

  @classmethod
  def from_scan (cls, scan: str) -> 'World':
    self = cls ()
    for structure in scan.split ('\n'):
      points = [tuple (int (i) for i in p.split (',')) for p in structure.split (" -> ")]
      for pair in zip (points[:-1], points[1:]):
        start, end = pair
        if start[0] != end[0]:
          lo = min (start[0], end[0])
          hi = max (start[0], end[0])
          for x in range (lo, hi+1):
            self.set (x, start[1])
        else:
          lo = min (start[1], end[1])
          hi = max (start[1], end[1])
          for y in range (lo, hi+1):
            self.set (start[0], y)
    self._rocks = frozenset (self._objects)
    return self

  def add_floor (self):
    self._floor = self._max_y + 2

  def __getitem__ (self, xy: tuple[int, int]) -> bool:
    return xy in self._objects

  def set (self, x: int, y: int):
    self._objects.add ((x, y))
    self._min_x = min (self._min_x, x)
    self._max_x = max (self._max_x, x)
    self._min_y = min (self._min_y, y)
    self._max_y = max (self._max_y, y)

  def print (self):
    min_x = min (self._min_x, self.POURING_POINT[0])
    max_x = max (self._max_x, self.POURING_POINT[0])
    min_y = min (self._min_y, self.POURING_POINT[1])
    max_y = max (self._max_y, self.POURING_POINT[1], (self._floor or self._max_y))
    if self._floor is not None:
      min_x -= 2
      max_x += 2
    def char (x: int, y: int) -> str:
      if (x, y) in self._rocks or y == self._floor:
        return "\x1b[38;5;248m#"
      elif (x, y) in self._objects:
        return "\x1b[33mo"
      elif (x, y) == self.POURING_POINT:
        return "\x1b[36m+"
      else:
        return "\x1b[38;5;239m.\x1b[22m"
    stdout.write (
      '\n'.join ([
        ''.join ([
          char (x, y) for x in range (min_x, max_x + 1)
        ]) for y in range (min_y, max_y + 1)
      ])
      + "\x1b[0m\n"
    )

  def pour_sand (self) -> bool:
    x, y = self.POURING_POINT
    if self.POURING_POINT in self._objects:
      return False
    max_y = self._max_y if self._floor is None else self._floor - 1
    while True:
      if y == max_y:
        if self._floor is None:
          return False
        else:
          break
      elif (x, y + 1) not in self._objects:
        y += 1
      elif (x - 1, y + 1) not in self._objects:
        x -= 1
        y += 1
      elif (x + 1, y + 1) not in self._objects:
        x += 1
        y += 1
      else:
        break
    self.set (x, y)
    return True



def main ():
  world = World.from_scan (stdin.read ().strip ())
  print ("Scanned world:")
  world.print ()
  print ("Pouring sand...")
  sand = 0
  while world.pour_sand ():
    sand += 1
  world.print ()
  print (f"There are \x1b[92m{sand}\x1b[0m units of sand")
  print ("Adding floor and pouring again...")
  world.add_floor ()
  while world.pour_sand ():
    sand += 1
  print ("Final state:")
  world.print ()
  print (f"There are \x1b[92m{sand}\x1b[0m units of sand")

if __name__ == "__main__":
  main ()
