#!/usr/bin/env python3
from sys import stdin

def start_of_packet_marker (packet: bytes, count: int) -> int:
  for i in range (len (packet) - count):
    if len (set (packet[i:i+count])) == count:
      return i + count

def main ():
  for packet in stdin:
    packet = bytes (packet, "ascii")
    print (start_of_packet_marker (packet, 4))
    print (start_of_packet_marker (packet, 14))

if __name__ == "__main__":
  main ()
