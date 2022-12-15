#!/usr/bin/env python3
from sys import stdin
from typing import TextIO, Optional, Iterator
import re
from dataclasses import dataclass

LARGE_NUMBER = int (1e10)

NUMBER_PATTERN = r"(-?\d+)"
INPUT_PATTERN = re.compile (
  "Sensor at x={0}, y={0}: closest beacon is at x={0}, y={0}".format (NUMBER_PATTERN)
)


Point = tuple[int, int]

PointSet = frozenset[Point]

@dataclass
class Sensor:
  at: Point
  range: int

  def __hash__ (self) -> int:
    return hash (self.at)

SensorSet = frozenset[Sensor]


def read_input (stream: TextIO) -> tuple[SensorSet, PointSet]:
  sensors = list ()
  beacons = list ()
  for s in stream.read ().strip ().split ('\n'):
    if (m := INPUT_PATTERN.match (s)) is not None:
      sx, sy, bx, by = m.groups ()
      sensor_at = (int (sx), int (sy))
      beacon_at = (int (bx), int (by))
      range = manhattan_distance (sensor_at, beacon_at)
      sensors.append (Sensor (sensor_at, range))
      beacons.append (beacon_at)
    else:
      raise ValueError (f"Invalid input: {s}")
  return frozenset (sensors), frozenset (beacons)


def manhattan_distance (a: Point, b: Point) -> int:
  return abs (a[0] - b[0]) + abs (a[1] - b[1])


# Returns [low, high] pair of x coordinates
def all_seen_points_on_line (sensor: Sensor, line: int) -> Optional[tuple[int, int]]:
  check_point = (sensor.at[0], line)
  distance = manhattan_distance (sensor.at, check_point)
  if distance > sensor.range:
    return None
  points = set (check_point)
  spread = sensor.range - distance
  return (check_point[0] - spread, check_point[0] + spread)


def known_non_beacons_in_line (sensors: SensorSet, beacons: PointSet, line: int) -> int:
  cells = set[Point] ()
  first = LARGE_NUMBER
  last = -LARGE_NUMBER
  for sensor in sensors:
    if (range_ := all_seen_points_on_line (sensor, line)) is not None:
      lo, hi = range_
      first = min (first, lo)
      last = max (last, hi)
  count = last - first + 1
  range_ = range (first, last + 1)
  for beacon in beacons:
    if beacon[1] == line and beacon[0] in range_:
      count -= 1
  return count


def average_position (sensors: SensorSet) -> Point:
  return (
    round (sum (map (lambda s: s.at[0], sensors)) / len (sensors)),
    round (sum (map (lambda s: s.at[1], sensors)) / len (sensors)),
  )


def point_is_not_covered (x: int, y: int, sensors: SensorSet) -> bool:
  return all (map (lambda s: manhattan_distance (s.at, (x, y)) > s.range, sensors))


def walk_edge (sensor: Sensor, bound: int) -> Iterator[Point]:
  from sys import stderr
  print ("Walking:", sensor, file=stderr)
  def walk_single_edge (a: Point, b: Point) -> Iterator[Point]:
    x_lo =  min (a[0], b[0])
    x_hi = max (a[0], b[0])
    y_lo = min (a[1], b[1])
    y_hi= max (a[1], b[1])
    for p in zip (range (x_lo, x_hi), range (y_lo, y_hi)):
      if p[0] not in range (bound + 1) or p[1] not in range (bound + 1):
        continue
      yield p
  x, y = sensor.at
  top = (x, y - sensor.range - 1)
  right = (x + sensor.range + 1, y)
  bottom = (x, y + sensor.range + 1)
  left = (x - sensor.range - 1, y)
  print ("  Edge: top -> right", file=stderr)
  yield from walk_single_edge (top, right)
  print ("  Edge: right -> bottom", file=stderr)
  yield from walk_single_edge (right, bottom)
  print ("  Edge: bottom -> left", file=stderr)
  yield from walk_single_edge (bottom, left)
  print ("  Edge: left -> top", file=stderr)
  yield from walk_single_edge (left, top)


def find_distress_beacon (sensors: SensorSet, beacons: PointSet, bound: int) -> Point:
  for s in sensors:
    for (x, y) in walk_edge (s, bound):
      if point_is_not_covered (x, y, sensors):
        return (x, y)
  return (0, 0)


def tuning_frequency (point: Point) -> int:
  return point[0] * 4000000 + point[1]


def main ():
  sensors, beacons = read_input (stdin)
  IS_EXAMPLE = Sensor ((2, 18), 7) in sensors
  line = 10 if IS_EXAMPLE else 2000000
  print ("On line {}, \x1b[92m{}\x1b[0m positions cannot contain a beacon.".format (
    line, known_non_beacons_in_line (sensors, beacons, line)
  ))
  bound = 20 if IS_EXAMPLE else 4000000
  distress_beacon = find_distress_beacon (sensors, beacons, bound)
  print ("The distress beacons is at {} with a tuning frequency of \x1b[92m{}\x1b[0m.".format (
    distress_beacon, tuning_frequency (distress_beacon)
  ))

if __name__ == "__main__":
  main ()
