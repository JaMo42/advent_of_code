#!/usr/bin/env python3
import sys
from pprint import pprint
from collections import defaultdict


def line_iter (p1, p2, d):
  x1, y1 = p1
  x2, y2 = p2
  if x1 == x2:
    for y in range (min (y1, y2), max (y1, y2) + 1):
      yield (x1, y)
  elif y1 == y2:
    for x in range (min (x1, x2), max (x1, x2) + 1):
      yield (x, y1)
  elif d:
    if x1 > x2:
      x1, x2 = x2, x1
      y1, y2 = y2, y1
    ys = 1 if y2 > y1 else -1
    for i, x in enumerate (range (x1, x2+1)):
      yield (x, y1+i*ys)
  else:
    return []


def solve (lines, allow_diagonal):
  cells = defaultdict (int)
  maxx, maxy = 0, 0

  for line in lines:
    if not line: continue
    p1, p2 = ([int (j) for j in i.split (',')] for i in line.split (" -> "))
    maxx = max (maxx, p1[0], p2[0])
    maxy = max (maxy, p1[1], p2[1])
    for p in line_iter (p1, p2, allow_diagonal):
      cells[p] += 1

  if maxx < 30 and maxy < 30:
    o = [['.' for x in range (maxx+1)] for y in range (maxy+1)]
    for c in cells:
      o[c[1]][c[0]] = str (cells[c])
    sys.stdout.write ('\n'.join (''.join (i) for i in o) + '\n')

  d = 0
  for c in cells:
    if cells[c] >= 2:
      d += 1

  return d


def main ():
  lines = sys.stdin.read ().split ('\n')
  print ("Part one: \x1b[92m{}\x1b[0m".format (solve (lines, False)))
  print ("Part two: \x1b[92m{}\x1b[0m".format (solve (lines, True)))


if __name__ == "__main__":
  main ()

