#!/usr/bin/env ocaml

let rec read_lines acc =
  try
    let line = input_line stdin in
    read_lines (line :: acc)
  with End_of_file -> List.rev acc

let input = read_lines []

let real_values =
  input
  |> List.map (String.split_on_char ' ')
  |> List.map (fun l -> List.map int_of_string l)

let differences list =
  List.fold_left
    (fun (acc, prev) x -> ((x - prev) :: acc, x))
    ([], List.hd list)
    (List.tl list)
  |> fun (acc, _) -> List.rev acc

let difference_tree values =
  let rec inner acc =
    if List.for_all (fun x -> x = 0) (List.hd acc) then acc
    else inner (differences (List.hd acc) :: acc)
  in
  inner [ values ]

let first_and_last list = (List.hd list, List.hd (List.rev list))
let edges difftree = List.map first_and_last difftree |> List.split

let predict_next_value right_edge =
  List.fold_left
    (fun last_on_prev_level last_on_current_level ->
      last_on_prev_level + last_on_current_level)
    0 (List.tl right_edge)

let predict_prev_value left_edge =
  List.fold_left
    (fun first_on_prev_level first_on_current_level ->
      first_on_current_level - first_on_prev_level)
    0 (List.tl left_edge)

let sum_of_prev, sum_of_next =
  real_values
  |> List.map (fun values -> edges (difference_tree values))
  |> List.map (fun (left_edge, right_edge) ->
         (predict_prev_value left_edge, predict_next_value right_edge))
  |> List.fold_left
       (fun (sum_of_prev, sum_of_next) (prev, next) ->
         (sum_of_prev + prev, sum_of_next + next))
       (0, 0)

let () =
  Printf.printf "The sum of the next values is \x1b[92m%d\x1b[m\n" sum_of_next;
  Printf.printf "The sum of the previous values is \x1b[92m%d\x1b[m\n" sum_of_prev
