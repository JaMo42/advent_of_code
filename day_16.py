#!/usr/bin/env python3
import sys
from enum import IntEnum
from collections import namedtuple
from functools import reduce
from operator import mul as op_mul

class PacketType (IntEnum):
  SUM = 0b000
  PRODUCT = 0b001
  MINIMUM = 0b010
  MAXIMUM = 0b011
  LITERAL = 0b100
  GREATER = 0b101
  LESS = 0b110
  EQUAL = 0b111


Packet = namedtuple ("Packet", ("version", "type", "value_or_subpackets"))


class Bitset:
  __slots__ = ("_bigint", "_size")

  def __init__ (self, hexstr):
    self._bigint = int (hexstr, 16)
    self._size = len (hexstr) * 4

  def read (self, off, n=1):
    # `off` starts from the most significant bit
    off = self._size - off - n
    return (self._bigint >> off) & ((1 << n) - 1)


def read_packet (bits):
  def impl (off):
    version = bits.read (off, 3)
    type_ = bits.read (off+3, 3)
    # Literal packet
    if type_ == PacketType.LITERAL:
      o = off + 6
      v = 0
      while True:
        vv = bits.read (o, 5)
        v = (v << 4) | (vv & 0b1111)
        o += 5
        if (vv & 0b10000) == 0:
          break
      return Packet (version, type_, v), o - off
    # Operator packet
    else:
      length_id = bits.read (off+6, 1)
      subs = []
      # Bit length
      if length_id == 0:
        length = bits.read (off+7, 15)
        o = off + 22
        oo = o + length
        while o < oo:
          p, r = impl (o)
          subs.append (p)
          o += r
        return Packet (version, type_, subs), o - off
      # Sub-packet count
      else:
        count = bits.read (off+7, 11)
        o = off + 18
        c = 0
        while c < count:
          p, r = impl (o)
          o += r
          subs.append (p)
          c += 1
        return Packet (version, type_, subs), o - off

  p,_ = impl (0)
  return p


def version_sum (packet):
  v = 0
  def visit (p):
    nonlocal v
    v += p.version
    if p.type != PacketType.LITERAL:
      for sp in p.value_or_subpackets:
        visit (sp)
  visit (packet)
  return v


def eval_packet (p):
  # Literal
  if p.type == PacketType.LITERAL:
    return p.value_or_subpackets
  # List operations
  s = [eval_packet (sp) for sp in p.value_or_subpackets]
  if p.type == PacketType.SUM:
    return sum (s)
  if p.type == PacketType.PRODUCT:
    return reduce (op_mul, s, 1)
  if p.type == PacketType.MINIMUM:
    return min (s)
  if p.type == PacketType.MAXIMUM:
    return max (s)
  # Binary operations
  assert (len (s) == 2), "Comparison packet with more than 2 parameters"
  if p.type == PacketType.GREATER:
    return int (s[0] > s[1])
  if p.type == PacketType.LESS:
    return int (s[0] < s[1])
  if p.type == PacketType.EQUAL:
    return int (s[0] == s[1])


def main ():
  packet_data = sys.stdin.read ().strip ()
  bits = Bitset (packet_data)
  packet = read_packet (bits)

  print (f"Sum of versions:      \x1b[92m{version_sum (packet)}\x1b[0m")
  print (f"Result of expression: \x1b[92m{eval_packet (packet)}\x1b[0m")

if __name__ == "__main__":
  main ()

