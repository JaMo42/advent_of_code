#!/usr/bin/env ocaml

let rec read_lines acc =
  try
    let line = input_line stdin in
    read_lines (line :: acc)
  with End_of_file -> List.rev acc

let input = read_lines []
