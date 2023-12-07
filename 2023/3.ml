#!/usr/bin/env ocaml

let rec read_lines acc =
  try
    let line = input_line stdin in
    read_lines (line :: acc)
  with End_of_file -> List.rev acc

let input = read_lines []

type point = { x : int; y : int }
type schematic_number = { start : point; end_ : point; value : int }

let points_are_adjacent a b =
  let dx = abs (a.x - b.x) in
  let dy = abs (a.y - b.y) in
  dx < 2 && dy < 2

let number_is_adjacent_to number point =
  (* Note: numbers are never longer than 3 digits so only checking the start
     and end points is sufficient. *)
  points_are_adjacent number.start point
  || points_are_adjacent number.end_ point

let number_is_adjacent_to_any number points =
  List.exists (number_is_adjacent_to number) points

let symbols : point list =
  input
  |> List.mapi (fun y row ->
         row |> String.to_seq |> List.of_seq
         |> List.mapi (fun x c ->
                match c with
                | '.' -> []
                | '0' .. '9' -> []
                | otherwise -> [ { x; y } ]))
  |> List.flatten |> List.flatten

(** Returns the character at the given index, or if it's out of bounds the
    given default *)
let str_at_or s i o = if i < String.length s then s.[i] else o

let numbers : schematic_number list =
  input
  |> List.mapi (fun y row ->
         (* we can't skip over elements since we are using List.map so we use
            this variable to check if we should ignore an index. *)
         let ignore_until = ref 0 in
         row |> String.to_seq |> List.of_seq
         |> List.mapi (fun x c ->
                if x < !ignore_until then []
                else
                  match c with
                  | '0' .. '9' ->
                      let start = { x; y } in
                      (* Scans the row until the end of the number, updating
                         `ignore_until` and returns the number *)
                      let rec find_end_and_get_number x =
                        match str_at_or row x '\x00' with
                        | '0' .. '9' -> find_end_and_get_number (x + 1)
                        | _ ->
                            let value =
                              int_of_string
                                (String.sub row start.x (x - start.x))
                            in
                            ignore_until := x;
                            { start; end_ = { x = x - 1; y }; value }
                      in
                      [ find_end_and_get_number (x + 1) ]
                  | _ -> []))
  |> List.flatten |> List.flatten

let part_number_sum =
  numbers
  |> List.filter (fun n -> number_is_adjacent_to_any n symbols)
  |> List.fold_left (fun acc n -> acc + n.value) 0
;;

Printf.printf "The sum of all part numbers is \x1b[92m%d\x1b[m\n"
  part_number_sum

let potential_gears =
  symbols |> List.filter (fun p -> (List.nth input p.y).[p.x] = '*')

let find_adjacent_numbers point =
  numbers |> List.filter (fun n -> number_is_adjacent_to n point)

let gear_ratio_sum =
  potential_gears
  |> List.map (fun p ->
         let nums = find_adjacent_numbers p in
         if List.length nums = 2 then
           List.fold_left (fun acc n -> acc * n.value) 1 nums
         else 0)
  |> List.fold_left ( + ) 0
;;

Printf.printf "The sum of all gear ratios is \x1b[92m%d\x1b[m\n" gear_ratio_sum
