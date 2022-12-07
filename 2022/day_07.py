#!/usr/bin/env python3
from sys import stdin
from typing import Iterator, cast
from functools import cmp_to_key

class Node:
  FILE = 0
  DIRECTORY = 1

  def __init__ (self, type: int, name: str, size_or_children: int | list['Node']) -> None:
    self._type = type
    self._name = name
    self._size_or_children = size_or_children
    self._cached_total_size = self.size () if type == self.FILE else None

  @classmethod
  def file (_, size: int, name: str) -> 'Node':
    return Node (Node.FILE, name, size)

  @classmethod
  def directory (_, name: str) -> 'Node':
    return Node (Node.DIRECTORY, name, list ())

  def is_file (self) -> bool:
    return self._type == self.FILE

  def is_directory (self) -> bool:
    return self._type == self.DIRECTORY

  def name (self) -> str:
    return self._name

  def size (self) -> int:
    assert (self.is_file ())
    # cast is for typing since it's stored as a union
    return cast (int, self._size_or_children)

  def children (self) -> list['Node']:
    assert (self.is_directory ())
    return cast (list, self._size_or_children)

  def sorted_children (self, group_dirs: bool) -> list['Node']:
    assert (self.is_directory ())
    def compare (a: 'Node', b: 'Node') -> int:
      if a._type == b._type or not group_dirs:
        return (a._name > b._name) - (a._name < b._name)
      else:
        return b._type - a._type
    return sorted (cast (list, self._size_or_children), key=cmp_to_key (compare))

  def find_child (self, name: str) -> 'Node':
    assert (self.is_directory ())
    for dir in self.children ():
      if dir.name () == name:
        return dir
    raise ValueError (f"Directory not found: {name}")

  def add_child (self, child: 'Node'):
    assert (self.is_directory ())
    self.children ().append (child)

  def format_type (self) -> str:
    return ["File", "Directory"][self._type ]

  def format (self) -> str:
    return [self._name, f"\x1b[94m{self._name}\x1b[34m/\x1b[0m"][self._type]

  def total_size (self) -> int:
    if self._cached_total_size is None:
      self._cached_total_size = sum (map (lambda c: c.total_size (), self.children ()))
    return self._cached_total_size


class FileSystem:
  def __init__ (self):
    self._root = Node.directory ("")
    # The current path, without the root element
    self._path = list[str] ()
    # The node at the end of the path, or the root node if it's empty
    self._cwd = self._root

  def root (self) -> Node:
    return self._root

  def change_directory (self, to: str):
    match to:
      case '/':
        self._path.clear ()
        self._cwd = self._root
      case "..":
        self._path.pop ()
        self._cwd = self._path[-1] if self._path else self._root
      case name:
        try:
          node = self._cwd.find_child (name)
          self._path.append (node)
          self._cwd = node
        except ValueError as ex:
          print (f"! cd failed: {ex}")
          print (f"! current path is: /{'/'.join (self._path)}")
          print ("! valid children are:")
          for c in self._cwd.children ():
            print (f"!   {c.name ()} ({c.format_type ()})")
          exit (1)

  def add_item (self, line: str):
    match line.split ():
      case ("dir", name):
        #print ("Directory:", name)
        c = Node.directory (name)
      case (size, name):
        #print ("File:", size, name)
        c = Node.file (int (size), name)
      case _:
        print ("! invalid input:", line)
        exit (1)
    self._cwd.add_child (c)

  def print_tree (self, group_dirs: bool = True):
    INNER_MID = "├─ "
    INNER_LAST = "╰─ "
    OUTER_MID = "│   "
    OUTER_LAST = "    "
    def impl (node: Node, prefix: list[str]):
      children = node.sorted_children (group_dirs)
      for (i, child) in enumerate (children):
        is_last = i == len (children) - 1
        prefix_str = ''.join (prefix)
        my_prefix = INNER_LAST if is_last else INNER_MID
        print (f"\x1b[2m{prefix_str}{my_prefix}\x1b[22m{child.format ()}")
        if child.is_directory ():
          impl (child, prefix + [OUTER_LAST if is_last else OUTER_MID])
    print (self._root.format ())
    impl (self._root, [])


def walk (fs: FileSystem, /, directories: bool=False) -> Iterator[Node]:
  def impl (node: Node, directories: bool):
    for c in node.children ():
      if c.is_directory ():
        if directories:
          yield c
        yield from impl (c, directories)
      else:
        if directories:
          continue
        yield c
  yield from impl (fs._root, directories)


def main ():
  fs = FileSystem ()
  for i in stdin:
    if i.startswith ("$ cd"):
      fs.change_directory (i.split ()[2])
    elif i.startswith ("$ ls"):
      ...
    else:
      fs.add_item (i)

  fs.print_tree (group_dirs=False)

  print ("Total size of small directories:",
    sum (
      map (
        lambda d: d.total_size (),
        filter (
          lambda d: d.total_size () <= 100000,
          walk (fs, directories=True)
        )
      )
    )
  )

  TOTAL = 70000000
  REQUIRED = 30000000
  size = fs.root ().total_size ()
  free = TOTAL - size

  print ("Size of smallest to delete:",
    min (
      filter (
        lambda d: free + d.total_size () >= REQUIRED,
        walk (fs, directories=True)
      ),
      key=lambda d: d.total_size ()
    )
    .total_size ()
  )


if __name__ == "__main__":
  main ()
