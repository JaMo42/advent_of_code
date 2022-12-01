#!/usr/bin/env python3
from sys import stdin

def main ():
  elves = map (
    lambda elf: sum (
      [int (cal) for cal in elf.split ('\n')]
    ),
    stdin.read ()
      .strip ()
      .split ("\n\n")
  )
  print (max (elves))
  print (sum (sorted (list (elves))[-3:]))

if __name__ == "__main__":
  main ()
