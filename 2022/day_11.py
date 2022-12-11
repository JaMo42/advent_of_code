#!/usr/bin/env python3
from sys import stdin
from dataclasses import dataclass
from operator import mul as op_mul
from operator import add as op_add
from math import lcm
from copy import deepcopy

@dataclass
class Test:
  divisible_by: int
  if_true: int
  if_false: int

  def __call__ (self, x: int) -> int:
    return self.if_true if x % self.divisible_by == 0 else self.if_false


class Operation:
  def __init__ (self, op: str):
    parts = op.split ()
    self._lhs = None if parts[0] == "old" else int (parts[0])
    self._rhs = None if parts[2] == "old" else int (parts[2])
    self._op = op_mul if parts[1] == '*' else op_add

  def __call__ (self, old: int) -> int:
    return self._op (self._lhs or old, self._rhs or old)

  def __str__ (self) -> str:
    return f"{self._lhs or 'old'} {'+' if self._op == op_add else '*'} {self._rhs or 'old'}"


class Monkey:
  def __init__ (
    self,
    starting_items: list[int],
    operation: Operation,
    test: Test
  ):
    self._items = starting_items
    self._op = operation
    self._test = test
    self._inspections = 0

  @classmethod
  def from_string (Self: type, description: str) -> 'Monkey':
    lines = description.split ('\n')
    starting_items = list (map (int, lines[1].split (':')[1].split (", ")))
    operation = Operation (lines[2].split ('=')[1].strip ())
    test_div = int (lines[3].split ()[-1])
    test_true = int (lines[4].split ()[-1])
    test_false = int (lines[5].split ()[-1])
    test = Test (test_div, test_true, test_false)
    return Self (starting_items, operation, test)

  def __str__ (self):
    return f"Monkey(items={self._items}, operation='{self._op}', test={self._test})"

  def inspect_one (self, reduce: bool) -> tuple[int, int]:
    item = self._items.pop (0)
    item = self._op (item)
    if reduce:
      item //= 3
    self._inspections += 1
    return (item, self._test (item))

  def push (self, item: int):
    self._items.append (item)

  def format_items (self) -> str:
    return ", ".join (map (str, self._items))

  def __bool__ (self) -> bool:
    return bool (self._items)

  def inspections (self) -> int:
    return self._inspections

  def get_test (self) -> int:
    return self._test.divisible_by


def show (monkeys: list[Monkey]):
  for (i, m) in enumerate (monkeys):
    print (f"Monkey {i}: {m.format_items ()}")


def do_round (monkeys: list[Monkey], scale: int, reduce: bool):
  for m in monkeys:
    while m:
      item, throw_to = m.inspect_one (reduce)
      item %= scale
      monkeys[throw_to].push (item)


def do_rounds (monkeys: list[Monkey], scale: int, reduce: bool, count: int):
  for _ in range (count):
    do_round (monkeys, scale, reduce)
  inspections = sorted ([m.inspections () for m in monkeys], reverse=True)
  print ()
  print ("After {} rounds ({} reductions):".format (
    count, ["without", "with"][reduce]
  ))
  show (monkeys)
  print ("Level of business:", inspections[0] * inspections[1])


def main ():
  monkeys = list (map (Monkey.from_string, stdin.read ().strip ().split ("\n\n")))
  # Find the least common multiple of all monkeys test values, this can be used
  # to scale down values without affecting any of the divisible-by tests.
  scale = 1
  for m in monkeys:
    scale = lcm (scale, m.get_test ())
  print ("Scaling factor:", scale)

  print ("Start:")
  show (monkeys)

  do_rounds (deepcopy (monkeys), scale, True, 20)
  do_rounds (monkeys, scale, False, 10000)

if __name__ == "__main__":
  main ()
