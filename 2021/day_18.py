#!/usr/bin/env python3
import sys
from copy import deepcopy
from math import floor, ceil
from dataclasses import dataclass
from itertools import permutations

@dataclass
class Element:
  value: int
  depth: int

  def __str__ (self):
    dc = "\x1b[33m" if self.depth >= 4 else "\x1b[90m"
    vc = "\x1b[31m" if self.value > 9 else ""
    return f"{vc}{self.value:>2}{dc}:{self.depth}\x1b[0m"
  __repr__ = __str__

  def __gt__ (self, other):
    # Required for `max` in the `magnitude` function
    return self.depth > other.depth


# Parse source into one-dimensional list where each element stores its value
# and depth
def process (src):
  src_lst = eval (src)
  lst = []
  def impl (pair, d):
    if type (pair[0]) == list:
      impl (pair[0], d+1)
    else:
      lst.append (Element (pair[0], d))
    if type (pair[1]) == list:
      impl (pair[1], d+1)
    else:
      lst.append (Element (pair[1], d))
  impl (src_lst, 0)
  return lst


# Explode pairs that are at depth 4
def explode (lst):
  L = len (lst)
  i = 0
  while i < L:
    if lst[i].depth >= 4:
      if i > 0:
        lst[i-1].value += lst[i].value
      if i+2 < L:
        lst[i+2].value += lst[i+1].value
      lst[i].value = 0
      lst[i].depth -= 1
      del lst[i+1]
      # No need to break the operation here since exploding always happens
      # before splitting so we can just process the entire current list
      L -= 1
    else:
      i += 1


# Split values that are greater than 9
def split (lst):
  for i in range (len (lst)):
    if lst[i].value > 9:
      d = lst[i].depth + 1
      left = Element (floor (lst[i].value / 2), d)
      right = Element (ceil (lst[i].value / 2), d)
      lst[i] = left
      lst.insert (i+1, right)
      # This needs to stop after the first split since we may need to explode
      # again first after the split
      return True
  return False


# Explode and split until list is fully reduced
def normalize (lst):
  while True:
    explode (lst)
    if not split (lst):
      break


# Add two lists
def add (a, b):
  for i in range (len (a)):
    a[i].depth += 1
  for i in range (len (b)):
    b[i].depth += 1
    a.append (b[i])


# Get the magnitude of the list
def magnitude (lst):
  my_lst = deepcopy (lst)
  while len (my_lst) > 1:
    idx = max (range (len (my_lst)), key=my_lst.__getitem__)
    a, b = my_lst[idx].value, my_lst[idx+1].value
    my_lst[idx] = Element (3*a + 2*b, my_lst[idx].depth - 1)
    del my_lst[idx+1]
  return my_lst[0].value


def part_one (stdin):
  lists = [process (i) for i in stdin[1:]]
  final = process (stdin[0])
  for l in lists:
    add (final, l)
    normalize (final)
  mag = magnitude (final)
  print (f"Part one: \x1b[92m{mag}\x1b[0m")


def part_two (stdin):
  lists = [process (i) for i in stdin]
  max_mag = 0
  for ai in range (len (lists)):
    for bi in range (len (lists)):
      if ai == bi:
        continue
      a = deepcopy (lists[ai])
      b = deepcopy (lists[bi])
      add (a, b)
      normalize (a)
      m = magnitude (a)
      if m > max_mag:
        max_mag = m
  print (f"Part two: \x1b[92m{max_mag}\x1b[0m")
  return max_mag


def main ():
  stdin = sys.stdin.read ().strip ().split ('\n')
  part_one (stdin)
  part_two (stdin)


if __name__ == "__main__":
  main ()

