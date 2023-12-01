#!/usr/bin/env ocaml

let rec read_lines acc =
  try
    let line = input_line stdin in
    read_lines (line :: acc)
  with End_of_file -> List.rev acc

let input = read_lines []

(** Returns the first and last item of the given list.
    If the list only contais a single item, that item is used for both. *)
let rec first_and_last lst =
  match lst with
  | [] -> failwith "first_and_last: empty list"
  | [ only ] -> (only, only)
  | [ first; last ] -> (first, last)
  | first :: _ :: after_second -> first_and_last (first :: after_second)

let int_of_digit c =
  match c with
  | '0' .. '9' -> int_of_char c - int_of_char '0'
  | _ -> failwith "int_of_digit: not a digit"
;;

Printf.printf "Part 1: \x1b[92m%d\x1b[m\n"
  (input
  |> List.map (fun line ->
         String.to_seq line
         |> Seq.filter (fun c -> match c with '0' .. '9' -> true | _ -> false)
         |> List.of_seq |> first_and_last
         |> fun (first, last) -> (int_of_digit first * 10) + int_of_digit last)
  |> List.fold_left ( + ) 0)

(** Returns shrinking slices of the given string,
    starting with the full string and removing characters from the front *)
let slices str =
  let rec inner acc str start len =
    if len > 0 then
      let slice = String.sub str start len in
      inner (slice :: acc) str (start + 1) (len - 1)
    else acc
  in
  List.rev (inner [] str 0 (String.length str))

let _DIGIT_TABLE =
  [
    ("0", "zero");
    ("1", "one");
    ("2", "two");
    ("3", "three");
    ("4", "four");
    ("5", "five");
    ("6", "six");
    ("7", "seven");
    ("8", "eight");
    ("9", "nine");
  ]

(** If the given string starts with a digit, either as a
    digit character or as a word, returns that digit. *)
let digit_at_start_of_string str =
  _DIGIT_TABLE
  |> List.find_index (fun (digit, word) ->
         String.starts_with ~prefix:digit str
         || String.starts_with ~prefix:word str)

(** Converts a list of options to a list of just the values. *)
let flat_map lst = List.map Option.to_list lst |> List.flatten

let extract_digits str =
  slices str |> List.map digit_at_start_of_string |> flat_map
;;

Printf.printf "Part 2: \x1b[92m%d\x1b[m\n"
  (input |> List.map extract_digits
  |> List.map (fun digits ->
         let first, last = first_and_last digits in
         (first * 10) + last)
  |> List.fold_left ( + ) 0)
