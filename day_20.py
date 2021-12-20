#!/usr/bin/env python3
import sys
import numpy as np
import struct

def scale_up (img, algorithm, outside=0):
  H, W = img.shape
  img = np.pad (img, (2,2), constant_values=outside)
  out = np.zeros ((H+2, W+2), dtype=int)
  for y in range (H+2):
    for x in range (W+2):
      surrounding = img[y:y+3,x:x+3].flatten ()
      index = np.packbits(surrounding[-8:])[0] | (surrounding[0] << 8)
      out[y,x] = algorithm[index]
  return out


def scale_up_times (img, algorithm, times):
  outside_values = [0, algorithm[0]]
  o = False
  for _ in range (times):
    img = scale_up (img, algorithm, outside_values[o])
    o = not o
  return img


def main ():
  stdin = sys.stdin.read ().strip ().split ("\n\n")
  algorithm = np.array ([int (i == '#') for i in stdin[0]])
  image = np.array ([[int (j == '#') for j in i] for i in stdin[1].split ('\n')])

  part_one = 2
  part_two = 50

  intermediate = scale_up_times (image, algorithm, part_one)
  final = scale_up_times (intermediate, algorithm, part_two - part_one)

  print (f"After scaling up {part_one} times, "\
         f"there are \x1b[92m{intermediate.sum ()}\x1b[0m lit pixels")
  print (f"After scaling up {part_two} times, "\
         f"there are \x1b[92m{final.sum ()}\x1b[0m lit pixels")

if __name__ == "__main__":
  main ()

