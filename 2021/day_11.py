#!/usr/bin/env python3
import sys

class Grid:
  def __init__ (self, data):
    self._cells = [[int (j) for j in i] for i in data]
    self._width = len (self._cells[0])
    self._height = len (self._cells)
    self._unmark_all ()

  def _unmark_all (self):
    self._marked = [[False for j in i] for i in self._cells]

  def _inc (self, x, y):
    if x < 0 or y < 0 or x >= self._width or y >= self._height:
      return -1
    if self._marked[y][x]:
      return -1
    self._cells[y][x] += 1
    if self._cells[y][x] == 10:
      self._cells[y][x] = 0
      self._marked[y][x] = True
      self._inc (x-1, y-1)
      self._inc (x,   y-1)
      self._inc (x+1, y-1)
      self._inc (x-1, y  )
      self._inc (x+1, y  )
      self._inc (x-1, y+1)
      self._inc (x,   y+1)
      self._inc (x+1, y+1)
    return self._cells[y][x]

  def width (self):
    return self._width

  def height (self):
    return self._height

  def step (self):
    self._unmark_all ()
    for y in range (self._height):
      for x in range (self._width):
        v = self._inc (x, y)
    return sum (sum (i) for i in self._marked)

  def show_step (self):
    o = ""
    d = False
    for y in range (self._height):
      for x in range (self._width):
        if self._marked[y][x]:
          o += f"\x1b[0m"
        else:
          o += f"\x1b[90m"
        o += str (self._cells[y][x])
      o += '\n'
    o += "\x1b[0m"
    print (o)


def count_flashes (initial, iterations=100):
  g = Grid (initial)
  f = 0

  for i in range (iterations):
    f += g.step ()

  print (f"After {iterations} steps, there have been \x1b[92m{f}\x1b[0m flashes")


def all_flashed (initial):
  g = Grid (initial)
  all_ = g.width () * g.height ()
  i = 0

  while 1:
    i += 1
    if g.step () == all_:
      break

  print (f"\x1b[92m{i}\x1b[0m is the first step during which all flashed")


def main ():
  stdin = sys.stdin.read ().strip ().split ("\n")
  count_flashes (stdin)
  all_flashed (stdin)

if __name__ == "__main__":
  main ()

