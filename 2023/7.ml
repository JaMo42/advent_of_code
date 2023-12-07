#!/usr/bin/env ocaml

module CardCounter = Map.Make (Char)

let rec read_lines acc =
  try
    let line = input_line stdin in
    read_lines (line :: acc)
  with End_of_file -> List.rev acc

let input = read_lines []

type hand = { cards : char array; bid : int }

let parse_hand line =
  Scanf.sscanf line "%s %d" (fun cards bid ->
      { cards = String.to_seq cards |> Array.of_seq; bid })

let card_counter_add counter card =
  let count = try CardCounter.find card counter with Not_found -> 0 in
  CardCounter.add card (count + 1) counter

let count_cards hand =
  hand.cards
  |> Array.fold_left
       (fun acc card -> card_counter_add acc card)
       CardCounter.empty

let card_strength ~joker card =
  match card with
  | '2' .. '9' -> int_of_char card - int_of_char '0'
  | 'T' -> 10
  | 'J' -> joker
  | 'Q' -> 12
  | 'K' -> 13
  | 'A' -> 14
  | _ -> failwith "invalid card"

let special_hand_strength_generic count =
  if count = [ 5 ] then 7 (* Five of a kind *)
  else if count = [ 4; 1 ] then 6 (* Four of a kind *)
  else if count = [ 3; 2 ] then 5 (* Full house *)
  else if List.hd count = 3 then 4 (* Three of a kind *)
  else if count = [ 2; 2; 1 ] then 3 (* Two pair *)
  else if List.hd count = 2 then 2
    (* One pair *)
    (* High card needs special handling *)
  else 0

let special_hand_strength hand =
  let count =
    count_cards hand |> CardCounter.to_list
    (* sort largest count first *)
    |> List.sort (fun (_, a) (_, b) -> compare b a)
    |> List.map (fun (card, count) -> count)
  in
  let generic = special_hand_strength_generic count in
  if generic = 0 && Array.length hand.cards = 5 then 1 else generic

let compare_hands_normal a b =
  Array.combine a b
  |> Array.find_map (fun (a, b) -> if a <> b then Some (a - b) else None)

let sort_game hands hand_str card_str =
  hands
  |> List.map (fun hand -> (hand, hand_str hand, Array.map card_str hand.cards))
  |> List.sort (fun (_, hsa, csa) (_, hsb, csb) ->
         if hsa <> hsb then hsa - hsb
         else compare_hands_normal csa csb |> Option.value ~default:0)
  |> List.map (fun (hand, _, _) -> hand)

let winnings_of_game sorted_gamge =
  sorted_gamge
  |> List.mapi (fun i hand -> hand.bid * (i + 1))
  |> List.fold_left ( + ) 0

let all_hands = input |> List.map parse_hand
let game = sort_game all_hands special_hand_strength (card_strength ~joker:11)
let winnings = winnings_of_game game
let () = Printf.printf "The total winnings are \x1b[92m%d\x1b[m\n" winnings

let count_cards_and_jokers hand =
  hand.cards
  |> Array.fold_left
       (fun (counter, jokers) card ->
         if card = 'J' then (counter, jokers + 1)
         else (card_counter_add counter card, jokers))
       (CardCounter.empty, 0)

let apply_jokers sorted_card_counts jokers =
  match sorted_card_counts with
  | [] -> [ jokers ]
  | [ x ] -> [ x + jokers ]
  | x :: xs -> (x + jokers) :: xs

let special_hand_strength_with_jokers hand =
  let counter, jokers = count_cards_and_jokers hand in
  let count =
    let count_without_jokers =
      counter |> CardCounter.to_list
      (* sort largest count first *)
      |> List.map (fun (_, count) -> count)
      |> List.sort (fun a b -> compare b a)
    in
    apply_jokers count_without_jokers jokers
  in
  let generic = special_hand_strength_generic count in
  (* jokers can't be part of a high card since they can't turn into themself *)
  if generic = 0 && Array.length hand.cards = 5 && jokers = 0 then 1
  else generic

let game_with_jokers =
  sort_game all_hands special_hand_strength_with_jokers (card_strength ~joker:1)

let winnings_with_jokers = winnings_of_game game_with_jokers

let () =
  Printf.printf "The total winnings with jokers are \x1b[92m%d\x1b[m\n"
    winnings_with_jokers
