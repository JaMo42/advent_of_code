#!/usr/bin/env ocaml

let rec read_lines acc =
  try
    let line = input_line stdin in
    read_lines (line :: acc)
  with End_of_file -> List.rev acc

(* add an extra empty line to make map parsing easier *)
let input = read_lines [] @ [ "" ]

type range = { src_start : int; src_end : int; dst_start : int }

let parse_range line =
  Scanf.sscanf line "%d %d %d" (fun dst_start src_start len ->
      { src_start; src_end = src_start + len; dst_start })

let range_contains x range = x >= range.src_start && x < range.src_end
let range_map_unchecked x range = x - range.src_start + range.dst_start

type map = { label : string; ranges : range list }

let parse_map lines =
  let label = List.hd (String.split_on_char ' ' (List.hd lines)) in
  let rec inner acc lines =
    let line = List.hd lines in
    if String.length line = 0 then (acc, lines)
    else inner (parse_range line :: acc) (List.tl lines)
  in
  let ranges, rest = inner [] (List.tl lines) in
  let ranges = List.sort (fun a b -> compare a.src_start b.src_start) ranges in
  ({ label; ranges }, rest)

let map_get map x =
  try List.find (range_contains x) map.ranges |> range_map_unchecked x
  with Not_found -> x

let rec parse_maps acc lines =
  if List.length lines = 0 then List.rev acc
  else
    let map, rest = parse_map lines in
    parse_maps (map :: acc) (List.tl rest)

let seeds =
  let first_line = List.hd input in
  String.sub first_line 7 (String.length first_line - 7)
  |> String.split_on_char ' ' |> List.map int_of_string

let maps = parse_maps [] (List.tl (List.tl input))
let min_of_list l = List.fold_left min (List.hd l) l

let location_of_seed seed =
  List.fold_left (fun acc map -> map_get map acc) seed maps

let min_loc = seeds |> List.map location_of_seed |> min_of_list;;

Printf.printf "The lowest location is \x1b[92m%d\x1b[m\n" min_loc
