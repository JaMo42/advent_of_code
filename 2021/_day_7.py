#!/usr/bin/env python3
import sys
from pprint import pprint


def move_to (crabs, pos, accel_usage):
  if accel_usage:
    #usage = lambda d: sum (range (1, d+1))
    usage = lambda d: (d * (d + 1) // 2)
    distances = (abs (i - pos) for i in crabs)
    return sum (usage (i) for i in distances)
  else:
    return sum (abs (i - pos) for i in crabs)


def solve (crabs, accel_usage):
  moves = []
  for i in range (min (crabs), max (crabs) + 1):
    moves.append (move_to (crabs, i, accel_usage))

  best = min (moves)
  best_pos = moves.index (best) + min (crabs)
  print (f"Best move: Position {best_pos}, "\
         f"with a fuel usage of \x1b[92m{best}\x1b[0m")


def main ():
  crabs = [int (i) for i in sys.stdin.read ().split (',')]
  print ("Constant fuel usage:")
  solve (crabs, False)
  print ("Accelerated fuel usage:")
  solve (crabs, True)


if __name__ == "__main__":
  main ();

