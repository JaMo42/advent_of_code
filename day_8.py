#!/usr/bin/env python3
import sys

def segments_to_bitset (segments):
  b = 0
  for i,l in enumerate ("abcdefg"):
    b |= int (l in segments) << i
  return b


def popcount (b):
  assert b == (b & 0xff)
  nibble_lookup = [0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4]
  return nibble_lookup[b & 0xf] + nibble_lookup[b >> 4]


def part_one (stdin):
  occurrences = 0
  for display in stdin:
    hints, digits = display.split ('|')
    hints = hints.split ()
    digits = [segments_to_bitset (i) for i in digits.split ()]

    # Digits with 2, 4, 3 or 7 segments set (1, 4, 7, 8)
    d = [segments_to_bitset (i)
         for i in filter (lambda x: len (x) in (2, 4, 3, 7), hints)]

    occurrences += sum (digits.count (i) for i in d)

  print (f"The digits 1, 4, 7 or 8 appear \x1b[92m{occurrences}\x1b[0m times")


def part_two (stdin):
  total = 0
  for display in stdin:
    hints, output = display.split ('|')
    hints = hints.split ()
    output = [segments_to_bitset (i) for i in output.split ()]

    # Known exactly based on length
    one = segments_to_bitset (next (filter (lambda x: len (x) == 2, hints)))
    four = segments_to_bitset (next (filter (lambda x: len (x) == 4, hints)))
    seven = segments_to_bitset (next (filter (lambda x: len (x) == 3, hints)))
    eight = segments_to_bitset (next (filter (lambda x: len (x) == 7, hints)))
    # Known to be one of based on length
    two_three_five = [segments_to_bitset (i)
                      for i in filter (lambda x: len (x) == 5, hints)]
    zero_six_nine = [segments_to_bitset (i)
                     for i in filter (lambda x: len (x) == 6, hints)]

    a = seven ^ one  # Segment A
    e_g = eight ^ (four | a)  # Segments E and G

    # Kown based on differences
    three = next (filter (lambda x: popcount (x ^ one) == 3, two_three_five))
    nine = next (filter (lambda x: popcount (x ^ three) == 1, zero_six_nine))
    two = next (filter (lambda x: popcount (x ^ e_g) == 3, two_three_five))
    five = next (filter (lambda x: popcount (x ^ two) == 4, two_three_five))

    b = nine ^ three  # Segment B

    # Composed
    six = (eight ^ three) | five
    zero = (eight ^ four) | b | one

    digits = [zero, one, two, three, four, five, six, seven, eight, nine]

    value = 0
    for i in output:
      value = value * 10 + digits.index (i)
    total += value

  print (f"The sum of all values is \x1b[92m{total}\x1b[0m")


def main ():
  stdin = sys.stdin.read ().split ('\n')
  part_one (stdin)
  part_two (stdin)


if __name__ == "__main__":
  main ()

