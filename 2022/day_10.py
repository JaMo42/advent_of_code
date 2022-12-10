#!/usr/bin/env python3
from sys import stdin, stdout
from typing import Any

class CPU:
  def __init__ (self):
    self.x = 1
    self.cycle = 0
    self._devices = list ()

  def _step_cycles (self, by: int):
    for _ in range (by):
      self.cycle += 1
      for d in self._devices:
        d.on_cycle (self)

  def noop (self):
    self._step_cycles (1)

  def addx (self, value: int):
    self._step_cycles (2)
    self.x += value

  def execute (self, instruction: str):
    match instruction.split ():
      case ["noop"]:
        self.noop ()
      case ["addx", value]:
        self.addx (int (value))
      case _:
        print ("warning: illegal instruction:", instruction)

  def connect (self, device: Any):
    self._devices.append (device)


class SignalStrengthMeter:
  def __init__ (self):
    self._sum = 0

  def on_cycle (self, cpu: CPU):
    if (cpu.cycle - 20) % 40 == 0:
      self._sum += cpu.cycle * cpu.x

  def read (self) -> int:
    return self._sum


class CRT:
  WIDTH = 40
  HEIGHT = 6

  def __init__ (self):
    self._pixels = [[False]*self.WIDTH for _ in range (self.HEIGHT)]
    self._row = 0
    self._column = 0

  def on_cycle (self, cpu: CPU):
    sprite = (cpu.x - 1, cpu.x, cpu.x + 1)
    self._pixels[self._row][self._column] = self._column in sprite
    self._column += 1
    if self._column == self.WIDTH:
      self._row += 1
      self._column = 0

  def display (self):
    for line in self._pixels:
      for pixel in line:
        stdout.write (["\x1b[2m.\x1b[0m", "#"][pixel])
      stdout.write ('\n')
    stdout.flush ()


def main ():
  cpu = CPU ()
  meter = SignalStrengthMeter ()
  screen = CRT ()

  cpu.connect (meter)
  cpu.connect (screen)

  for instruction in stdin:
    cpu.execute (instruction)

  print (meter.read ())
  screen.display ()

if __name__ == "__main__":
  main ()
