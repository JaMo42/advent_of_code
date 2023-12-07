#!/usr/bin/env ocaml

let rec read_lines acc =
  try
    let line = input_line stdin in
    read_lines (line :: acc)
  with End_of_file -> List.rev acc

let input = read_lines []

let parse_line line =
  String.split_on_char ' ' line |> List.tl |> List.filter_map int_of_string_opt

type race = { time : int; record_distance : int }

let parse_races input =
  let times_and_distances = List.map parse_line input in
  List.combine (List.nth times_and_distances 0) (List.nth times_and_distances 1)
  |> List.map (fun (time, record_distance) -> { time; record_distance })

let simulate_race race hold_down_time =
  let mm_per_ms = hold_down_time in
  let travel_ms = race.time - hold_down_time in
  mm_per_ms * travel_ms

let race_win_ways race =
  List.init race.time (fun i -> i)
  |> List.filter_map (fun hold_down_time ->
         let distance = simulate_race race hold_down_time in
         if distance > race.record_distance then Some hold_down_time else None)

let races = parse_races input

let solution1 =
  races |> List.map race_win_ways |> List.map List.length
  |> List.fold_left ( * ) 1
;;

Printf.printf "Part 1: \x1b[92m%d\x1b[0m\n" solution1

let parse_line_as_single_number line =
  String.to_seq line
  |> Seq.fold_left
       (fun acc c ->
         match c with
         | '0' .. '9' -> (acc * 10) + (int_of_char c - int_of_char '0')
         | _ -> acc)
       0

let time = parse_line_as_single_number (List.nth input 0)
let record_distance = parse_line_as_single_number (List.nth input 1);;

Printf.printf "time:            %d\nrecord_distance: %d\n" time record_distance

let min_hold_time = ref 0
let max_hold_time = ref 0

exception Break;;

try
  for i = 1 to time do
    if (time - i) * i > record_distance then (
      min_hold_time := i;
      raise Break)
  done
with Break -> ()
;;

try
  for i = 1 to time do
    let i = time - i in
    if (time - i) * i > record_distance then (
      max_hold_time := i;
      raise Break)
  done
with Break -> ()
;;

Printf.printf "min_hold_time:   %d\n" !min_hold_time;;
Printf.printf "max_hold_time:   %d\n" !max_hold_time

let delta = !max_hold_time - !min_hold_time + 1;;

Printf.printf "Part 2: \x1b[92m%d\x1b[0m\n" delta
