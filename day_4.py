#!/usr/bin/env python3
import sys
from pprint import pprint
from dataclasses import dataclass

@dataclass
class Cell:
  n: int
  m: bool

  def __str__ (self):
    return f"{self.n:2}" if self.m else f"\x1b[90m{self.n:2}\x1b[0m"
  __repr__ = __str__


def is_solved (g):
  gg = [[int (c.m) for c in r] for r in g]
  if 5 in (sum (r) for r in gg) or 5 in (sum (c) for c in zip (*gg)):
    return True
  return False


def score (g, d):
  return sum (map (lambda r: sum (map (lambda c: c.n*(not c.m), r)), g)) * d


def run (g, draws):
  for i, d in enumerate (draws):
    for r in g:
      for c in r:
        if c.n == d:
          c.m = True
    if (is_solved (g)):
      return score (g, d), i
  #return 0, len (draws) + 1
  return 0, 0


def main ():
  lines = sys.stdin.read ().split ('\n')
  lines.append ([])
  draws = [int (i) for i in lines[0].split (',')]

  g = []
  s = 0
  #l = len (draws) + 1
  l = 0
  for line in lines[2:]:
    if not line:
      ss, ll = run (g, draws)
      #if ll < l:
      if ll > l:
        s = ss
        l = ll
      g = []
    else:
      g.append ([Cell (int (i), False) for i in line.split ()])

  print (s)

if __name__ == "__main__":
  main ()

