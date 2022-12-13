#!/usr/bin/env python3
from sys import stdin
from functools import cmp_to_key

def compare (left: list, right: list) -> int:
  # Return value:
  #  < 0: left before right (correct order)
  #    0: left and right are equal
  #  > 0: right before left
  for (l, r) in zip (left, right):
    if isinstance (l, int) and isinstance (r, int):
      if l != r:
        return l - r
    else:
      if not isinstance (l, list):
        l = [l]
      if not isinstance (r, list):
        r = [r]
      if (decision := compare (l, r)) != 0:
        return decision
  return len (left) - len (right)


def p1 (input: str):
  pairs = list (map (
    lambda p: tuple (eval (lst) for lst in p.split ('\n')),
    input.split ("\n\n")
  ))
  print ("Index sum:",
    sum (
      map (
        lambda index_and_pair: index_and_pair[0] + 1,
        filter (
          lambda index_and_pair: compare (*index_and_pair[1]) < 0,
          enumerate (pairs)))))


def p2 (input: str):
  packets = [eval (p) for p in input.split ('\n') if p]
  packets += [[[2]], [[6]]]
  packets.sort (key=cmp_to_key (compare))
  a = packets.index ([[2]]) + 1
  b = packets.index ([[6]]) + 1
  print ("Decoder key:", a * b)


def main ():
  input = stdin.read ().strip ()
  p1 (input)
  p2 (input)

if __name__ == "__main__":
  main ()
