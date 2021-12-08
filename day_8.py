#!/usr/bin/env python3
import sys

def part_one (stdin):
  occurrences = 0
  for display in stdin:
    hints, digits = display.split ('|')
    hints = hints.split ()
    digits = [set (i) for i in digits.split ()]

    # Digits with 2, 4, 3 or 7 segments set (1, 4, 7, 8)
    d = [set (i) for i in filter (lambda x: len (x) in (2, 4, 3, 7), hints)]

    occurrences += sum (digits.count (i) for i in d)

  print (f"The digits 1, 4, 7 or 8 appear \x1b[92m{occurrences}\x1b[0m times")


def part_two (stdin):
  total = 0
  for display in stdin:
    hints, output = display.split ('|')
    hints = hints.split ()
    output = [set (i) for i in output.split ()]

    # Known exactly based on length
    one = set (next (filter (lambda x: len (x) == 2, hints)))
    four = set (next (filter (lambda x: len (x) == 4, hints)))
    seven = set (next (filter (lambda x: len (x) == 3, hints)))
    eight = set (next (filter (lambda x: len (x) == 7, hints)))
    # Known to be one of based on length
    two_three_five = [set (i) for i in filter (lambda x: len (x) == 5, hints)]
    zero_six_nine = [set (i) for i in filter (lambda x: len (x) == 6, hints)]

    a = seven ^ one
    e_g = eight ^ (four | a)

    # Kown based on differences
    three = next (filter (lambda x: len (x ^ one) == 3, two_three_five))
    nine = next (filter (lambda x: len (x ^ three) == 1, zero_six_nine))
    two = next (filter (lambda x: len (x ^ e_g) == 3, two_three_five))
    five = next (filter (lambda x: len (x ^ two) == 4, two_three_five))

    b = nine ^ three

    # Composed
    six = (eight ^ three) | five
    zero = (eight ^ four) | b | one

    digits = [zero, one, two, three, four, five, six, seven, eight, nine]

    value = ""
    for i in output:
      value += str (digits.index (i))
    total += int (value)

  print (f"The sum of all values is \x1b[92m{total}\x1b[0m")


def main ():
  stdin = sys.stdin.read ().split ('\n')
  part_one (stdin)
  part_two (stdin)


if __name__ == "__main__":
  main ()

