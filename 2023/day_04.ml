#!/usr/bin/env ocaml

module IntSet = Set.Make (Int)

let rec read_lines acc =
  try
    let line = input_line stdin in
    read_lines (line :: acc)
  with End_of_file -> List.rev acc

let input = read_lines []

type card = { number : int; winning : IntSet.t; have : IntSet.t }

let parse_numbers s =
  String.split_on_char ' ' s
  |> List.filter_map (fun s ->
         if String.length s > 0 then Some (int_of_string s) else None)

let parse_card line =
  Scanf.sscanf line "Card %d:%[^\t\n]" (fun number both_lists ->
      let winning_and_have = String.split_on_char '|' both_lists in
      let winning =
        IntSet.of_list (parse_numbers (List.nth winning_and_have 0))
      in
      let have = IntSet.of_list (parse_numbers (List.nth winning_and_have 1)) in
      { number; winning; have })

let pow2 n = Int.shift_left 1 n

let card_match_count card =
  IntSet.inter card.winning card.have |> IntSet.cardinal

let card_points card =
  (* Note: would not be correct with a real power but shifting by -1 gives us 0 here
     when `matches` is 0 which is what we want *)
  pow2 (card_match_count card - 1)

let cards = List.map parse_card input;;

let total_points = List.map card_points cards |> List.fold_left ( + ) 0;;

Printf.printf "The cards are worth \x1b[92m%d\x1b[m points in total\n"
  total_points

(* start off with 1 of each card *)
let table = Array.make (List.length cards) 1;;

cards
|> List.mapi (fun i card ->
       let count = table.(i) in
       let matches = card_match_count card in
       let copies = List.init matches (fun j -> i + 1 + j) in
       List.iter
         (fun copy_idx -> table.(copy_idx) <- table.(copy_idx) + count)
         copies)

let total = Array.fold_left ( + ) 0 table;;

Printf.printf "I ended up with \x1b[92m%d\x1b[m cards\n" total
