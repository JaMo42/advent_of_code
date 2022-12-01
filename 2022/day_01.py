#!/usr/bin/env python3
from sys import stdin

def main ():
  print (
    max (
      map (
        lambda elf: sum (
          [int (cal) for cal in elf.split ('\n')]
        ),
        stdin.read ()
          .strip ()
          .split ("\n\n")
      )
    )
  )

if __name__ == "__main__":
  main ()
