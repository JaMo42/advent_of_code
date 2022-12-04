#!/usr/bin/env python3
from sys import stdin

def contains (a: tuple[int, int], b: tuple[int, int]) -> bool:
  check = lambda a, b: a[0] <= b[0] and a[1] >= b[1]
  return check (a, b) or check (b, a)

def overlaps (a: tuple[int, int], b: tuple[int, int]) -> bool:
  check = lambda a, b: (b[0] in range (a[0], a[1]+1)
                                           or b[1] in range (a[0], a[1]+1))
  return check (a, b) or check (b, a)

def assignment_pair (s: str) -> tuple[tuple[int, int], tuple[int, int]]:
  return tuple (map (
    lambda p: tuple (int (i) for i in p.split ('-')),
    s.split (',')
  ))

def main ():
  input = stdin.read ().strip ().split ('\n')
  print (
    list (map (lambda pair: contains (*assignment_pair (pair)), input))
      .count (True)
  )
  print (
    list (map (lambda pair: overlaps (*assignment_pair (pair)), input))
      .count (True)
  )

if __name__ == "__main__":
  main ()
