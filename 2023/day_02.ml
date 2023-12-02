#!/usr/bin/env ocaml

let rec read_lines acc =
  try
    let line = input_line stdin in
    read_lines (line :: acc)
  with End_of_file -> List.rev acc

let input = read_lines []

type subset = { red : int; green : int; blue : int }
type game = { id : int; subsets : subset list }

let parse_subset subset =
  subset |> String.split_on_char ','
  |> List.map (fun line ->
         Scanf.sscanf (String.trim line) "%d %s" (fun count color ->
             match color with
             | "red" -> { red = count; green = 0; blue = 0 }
             | "green" -> { red = 0; green = count; blue = 0 }
             | "blue" -> { red = 0; green = 0; blue = count }
             | _ -> failwith (Printf.sprintf "invalid color: %s" color)))
  |> List.fold_left
       (fun acc subset ->
         {
           red = acc.red + subset.red;
           green = acc.green + subset.green;
           blue = acc.blue + subset.blue;
         })
       { red = 0; green = 0; blue = 0 }

let games =
  input
  |> List.map (fun line ->
         Scanf.sscanf line "Game %d: %[^\t\n]" (fun id subsets ->
             let subsets =
               subsets |> String.split_on_char ';' |> List.map parse_subset
             in
             { id; subsets }))

(** Returns `true` if the subset can be played with the given number of cubes. *)
let check_subset subset avail =
  subset.red <= avail.red
  && subset.green <= avail.green
  && subset.blue <= avail.blue

(** Returns `true` if the game can be played with the given number of cubes. *)
let check_game game avail =
  game.subsets |> List.for_all (fun subset -> check_subset subset avail)

let id_sum =
  games
  |> List.filter (fun game ->
         check_game game { red = 12; green = 13; blue = 14 })
  |> List.fold_left (fun acc game -> acc + game.id) 0
;;

Printf.printf "Sum of IDs of valid games: \x1b[92m%d\x1b[m\n" id_sum

(** Returns the lowest possible number of red, green, and blue cubes required
    for the given subsets of a game *)
let find_lowest subsets =
  List.fold_left
    (fun acc subset ->
      {
        red = max acc.red subset.red;
        green = max acc.green subset.green;
        blue = max acc.blue subset.blue;
      })
    { red = 0; green = 0; blue = 0 }
    subsets

let power_sum =
  games
  |> List.map (fun game -> find_lowest game.subsets)
  |> List.fold_left
       (fun acc subset -> acc + (subset.red * subset.green * subset.blue))
       0
;;

Printf.printf "The power sum is: \x1b[92m%d\x1b[m\n" power_sum
