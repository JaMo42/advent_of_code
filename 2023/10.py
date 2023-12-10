#!/usr/bin/env python3
from dataclasses import dataclass
from sys import stdin
from typing import Optional

#        01234567
TILES = ".|-LJ7FS"

def tile_has_left_connection(tile: int):
  return tile in (2, 4, 5)

def tile_has_right_connection(tile: int):
  return tile in (2, 3, 6)

def tile_has_top_connection(tile: int):
  return tile in (1, 3, 4)

def tile_has_bottom_connection(tile: int):
  return tile in (1, 5, 6)

BIG_TILES = [
  # .
  ("...",
   "...",
   "..."),
  # |
  (".|.",
   ".|.",
   ".|."),
  # -
  ("...",
   "---",
   "..."),
  # L
  (".|.",
   ".+-",
   "..."),
  # J
  (".|.",
   "-+.",
   "..."),
  # 7
  ("...",
   "-+.",
   ".|."),
  # F
  ("...",
   ".+-",
   ".|."),
]

def big_tile_start_point(tile: int) -> tuple[int, int]:
  return [None, None, None, (2, 0), (0, 0), (0, 2), (2, 2)][tile]

@dataclass
class Grid:
  tiles: list[list[int]]
  start: tuple[int, int] = (0, 0)
  _pipe_tiles: Optional[set[tuple[int, int]]] = None

  @classmethod
  def parse(cls, s: str):
    grid = cls([[TILES.index(c) for c in line] for line in s.strip().split('\n')])
    for y, row in enumerate(grid.tiles):
      for x, tile in enumerate(row):
        if tile == 7:
          has_left_connection = tile_has_right_connection(grid.tiles[y][x - 1])
          has_right_connection = tile_has_left_connection(grid.tiles[y][x + 1])
          has_top_connection = tile_has_bottom_connection(grid.tiles[y - 1][x])
          has_bottom_connection = tile_has_top_connection(grid.tiles[y + 1][x])
          #ic(has_left_connection, has_right_connection, has_top_connection, has_bottom_connection)
          if has_bottom_connection and has_right_connection:
            grid.tiles[y][x] = 6
          elif has_bottom_connection and has_left_connection:
            grid.tiles[y][x] = 5
          elif has_top_connection and has_right_connection:
            grid.tiles[y][x] = 3
          elif has_top_connection and has_left_connection:
            grid.tiles[y][x] = 4
          #print("start is", grid.tiles[y][x])
          grid.start = (x, y)
    return grid

  def width(self) -> int:
    return len(self.tiles[0])

  def height(self) -> int:
    return len(self.tiles)

  def __str__(self):
    return '\n'.join(''.join(TILES[tile] for tile in row) for row in self.tiles)

  def follow_pipe(self, tile_pos: tuple[int, int], from_pos: tuple[int, int]) -> tuple[int, int]:
    x, y = tile_pos
    tile = self.tiles[y][x]
    from_top = from_pos[1] < y
    from_bottom = from_pos[1] > y
    from_left = from_pos[0] < x
    #from_right = from_pos[0] > x
    TABLE = [
      None, # .
      # (predicate, location_if_predicate_is_true, location_otherwise)
      (from_top, (x, y + 1), (x, y - 1)), # |
      (from_left, (x + 1, y), (x - 1, y)), # -
      (from_top, (x + 1, y), (x, y - 1)), # L
      (from_top, (x - 1, y), (x, y - 1)), # J
      (from_bottom, (x - 1, y), (x, y + 1)), # 7
      (from_bottom, (x + 1, y), (x, y + 1)), # F
      None, # S
    ]
    p, t, f = TABLE[tile]
    return t if p else f

  def max_dist_in_pipe(self) -> int:
    starting_points = list()
    for ((dx, dy), pred) in (
      ((1, 0), tile_has_right_connection),
      ((-1, 0), tile_has_left_connection),
      ((0, 1), tile_has_bottom_connection),
      ((0, -1), tile_has_top_connection),
    ):
      x, y = self.start
      if pred(self.tiles[y][x]):
        starting_points.append((x + dx, y + dy))
    a, b = starting_points
    a_from = self.start
    b_from = self.start
    d = 1
    self._pipe_tiles = {self.start,}
    while a != b:
      self._pipe_tiles.add(a)
      self._pipe_tiles.add(b)
      a_from, a = a, self.follow_pipe(a, a_from)
      b_from, b = b, self.follow_pipe(b, b_from)
      d += 1
    self._pipe_tiles.add(a) # a == b
    return d

  def pipe_tiles(self) -> set[tuple[int, int]]:
    assert self._pipe_tiles is not None
    return self._pipe_tiles


@dataclass
class BigGrid:
  tiles: list[list[int]]
  # start in big coordinate space
  local_start: tuple[int, int] = (0, 0)

  def __init__(self, base: Grid):
    self.tiles = [[False for _x in range(base.width() * 3)] for _y in range(base.height() * 3)]
    for (y, row) in enumerate(base.tiles):
      for (x, tile) in enumerate(row):
        big_tile = BIG_TILES[tile]
        for ty, line in enumerate(big_tile):
          for tx, c in enumerate(line):
            self.tiles[y * 3 + ty][x * 3 + tx] = c != '.'
    start_tile = base.tiles[base.start[1]][base.start[0]]
    big_start_offset = big_tile_start_point(start_tile)
    self.local_start = (
      base.start[0] * 3 + big_start_offset[0],
      base.start[1] * 3 + big_start_offset[1]
    )

  def __str__(self):
    #return '\n'.join(''.join('#' if tile else '.' for tile in row) for row in self.tiles)
    s = list()
    for y, row in enumerate(self.tiles):
      l = list()
      for x, tile in enumerate(row):
        if (x, y) == self.local_start:
          l.append("\x1b[92m@\x1b[m")
        else:
          l.append("\x1b[91m#\x1b[m" if tile else '.')
      s.append("".join(l))
    return "\n".join(s)

  def enclosed_base_tiles(self) -> set[tuple[int, int]]:
    filled = {self.local_start,}
    edge = {self.local_start,}
    while edge:
      new_edge = set()
      for point in edge:
        for (dx, dy) in ((1, 0), (-1, 0), (0, 1), (0, -1)):
          tx, ty = point[0] + dx, point[1] + dy
          # Note: every point in `edge` is also in `filled` so this checks
          # prevents all revisits
          if (tx, ty) not in filled and not self.tiles[ty][tx]:
            new_edge.add((tx, ty))
            filled.add((tx, ty))
      edge = new_edge
    # Scale points back down to base grid coordinates
    return {(x // 3, y // 3) for (x, y) in filled}

def main():
  grid = Grid.parse(stdin.read())
  #print(grid)
  x = grid.max_dist_in_pipe()
  print(f"It takes \x1b[92m{x}\x1b[m steps to get to the farthest point")
  big_grid = BigGrid(grid)
  #print(big_grid)
  enclosed = big_grid.enclosed_base_tiles()
  # `enclosed` now includes the tiles where we sequeeze between two pipes
  # so we need to remove all tiles that are part of the pipe loop (junk pipes
  # still count as enclosed space).
  enclosed = {p for p in enclosed if p not in grid.pipe_tiles()}
  print(f"There are \x1b[92m{len(enclosed)}\x1b[m enclosed tiles inside the loop")


if __name__ == "__main__":
  main()
