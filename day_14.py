#!/usr/bin/env python3
import sys
from collections import defaultdict

class Polymer:
  COMBINATIONS = dict ()

  @staticmethod
  def load_combinations (source):
    Polymer.COMBINATIONS = {i[:2]: i[-1] for i in source}

  def __init__ (self, template):
    self._combinations = defaultdict (int)
    self._counts = defaultdict (int)
    for i in range (len (template) - 1):
      self._combinations[template[i:i+2]] += 1
      self._counts[template[i]] += 1
    self._counts[template[-1]] += 1

  def step (self):
    new_state = defaultdict (int)
    for c in self._combinations:
      ins = self.COMBINATIONS[c]
      new_state[c[0]+ins] += self._combinations[c]
      new_state[ins+c[1]] += self._combinations[c]
      self._counts[ins] += self._combinations[c]
    self._combinations = new_state

  def proof_of_solution (self):
    return max (self._counts.values ()) - min (self._counts.values ())


def main ():
  stdin = sys.stdin.read ().strip ().split ('\n')
  Polymer.load_combinations (stdin[2:])
  p = Polymer (stdin[0])

  for i in range (10):
    p.step ()
  print (f"Part one: \x1b[92m{p.proof_of_solution ()}\x1b[0m")

  for i in range (30):
    p.step ()
  print (f"Part two: \x1b[92m{p.proof_of_solution ()}\x1b[0m")


if __name__ == "__main__":
  main ()

