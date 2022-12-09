#!/usr/bin/env python3
from sys import stdin, stdout
from dataclasses import dataclass

# Parts have different example inputs so they are hardcoded here and the input
# file just contains an identifier

EXAMPLE_INPUT_1 =\
"""
R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2
"""

EXAMPLE_INPUT_2 =\
"""
R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20
"""


@dataclass
class Bounds:
  min_x: int
  max_x: int
  min_y: int
  max_y: int


@dataclass
class Point:
  x: int
  y: int

  def is_far_away_from (self, other: 'Point') -> bool:
    return abs (self.x - other.x) > 1 or abs (self.y - other.y) > 1

  def direction_toward (self, other: 'Point') -> tuple[int, int]:
    # `self` is point to move, `other` is the target to move to.
    d = lambda x: int (x > 0) - int (x < 0)
    return (
      d (other.x - self.x),
      d (other.y - self.y)
    )

  def as_tuple (self) -> tuple[int, int]:
    return (self.x, self.y)

  def move_toward (self, other: 'Point'):
    if self.is_far_away_from (other):
      dx, dy = self.direction_toward (other)
      self.x += dx
      self.y += dy

  def translated (self, tx: int, ty: int) -> 'Point':
    return Point (self.x + tx, self.y + ty)


class Rope:
  def __init__ (self, length):
    assert length > 1
    self._segments = list (Point (0, 0) for _ in range (length))
    # Used to determine range to print
    self._bounds = Bounds (0, 0, 0, 0)
    self._tail_visited = set ()

  def head (self) -> Point:
    return self._segments[0]

  def tail (self) -> Point:
    return self._segments[-1]

  def _do_move (self, direction: str):
    match direction:
      case 'U':
        self._segments[0].y -= 1
      case 'D':
        self._segments[0].y += 1
      case 'L':
        self._segments[0].x -= 1
      case 'R':
        self._segments[0].x += 1
    for i in range (len (self._segments) - 1):
      self._segments[i + 1].move_toward (self._segments[i])
    self._tail_visited.add (self.tail ().as_tuple ())
    self._bounds.min_x = min (self._bounds.min_x, self._segments[0].x)
    self._bounds.max_x = max (self._bounds.max_x, self._segments[0].x)
    self._bounds.min_y = min (self._bounds.min_y, self._segments[0].y)
    self._bounds.max_y = max (self._bounds.max_y, self._segments[0].y)

  def move_head (self, description: str):
    direction, amount = description.split ()
    for _ in range (int (amount)):
      self._do_move (direction)

  def print (self, /, tail: str = '', border: int = 1):
    tx = -self._bounds.min_x + border
    ty = -self._bounds.min_y + border
    width = self._bounds.max_x + 1 + tx + 2 * border
    height = self._bounds.max_y + 1 + ty + 2 * border
    points = list (map (lambda p: p.translated (tx, ty).as_tuple (), self._segments))
    stdout.write (f"{self._bounds.max_x+2*border:>{width}}\r")
    stdout.write (f"{0:>{tx+1}}\r")
    stdout.write (f"{self._bounds.min_x-border}\n")
    for row in range (height):
      for col in range (width):
        try:
          p = points.index ((col, row))
          if p == 0:
            stdout.write ('H')
          elif tail and p == len (points) - 1:
            stdout.write ('T')
          else:
            stdout.write (str (p))
        except ValueError:
          if (col, row) == (tx, ty):
            stdout.write ('s')
          elif (col - tx, row - ty) in self._tail_visited:
            stdout.write ('#')
          else:
            stdout.write ('.')
      if row == 0:
        stdout.write (f" {self._bounds.min_y-border: }")
      elif row == ty:
        stdout.write ("  0")
      elif row == height - 1:
        stdout.write (f" {self._bounds.max_y+2*border: }")
      stdout.write ('\n')
    stdout.flush ()

  def tail_visited_count (self) -> int:
    return len (self._tail_visited)

def main ():
  short_rope = Rope (2)
  long_rope = Rope (10)
  use_example = False
  for motion in stdin:
    if motion.startswith ("EXAMPLE"):
      use_example = True
      break
    short_rope.move_head (motion)
    long_rope.move_head (motion)
  if use_example:
    print ("Using internal example input")
    for motion in EXAMPLE_INPUT_1.strip ().split ('\n'):
      short_rope.move_head (motion)
    for motion in EXAMPLE_INPUT_2.strip ().split ('\n'):
      long_rope.move_head (motion)

  if use_example:
    short_rope.print (tail = 'T')
  print ("Short visited:", short_rope.tail_visited_count ())

  if use_example:
    print ("\n================\n")
    long_rope.print (border = 0)
  print ("Long visited:", long_rope.tail_visited_count ())

if __name__ == "__main__":
  main ()
