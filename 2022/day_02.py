#!/usr/bin/env python3
from sys import stdin

ROCK = 1
PAPER = 2
SCISSORS = 3

WINNING = (
  (ROCK, SCISSORS),
  (PAPER, ROCK),
  (SCISSORS, PAPER),
)

def translate_move (c):
  match c:
    case "A" | "X":
      return ROCK
    case "B" | "Y":
      return PAPER
    case "C" | "Z":
      return SCISSORS

def play (round):
  round = list (round)
  opponent = round[0]
  me = round[1]
  if me == opponent:
    return 3 + me
  for w in WINNING:
    if w == (me, opponent):
      return 6 + me
  return me

def get_my_move (opponent, outcome):
  match outcome:
    # Lose
    case "X":
      for w in WINNING:
        if opponent == w[0]:
          return w[1]
    # Draw
    case "Y":
      return opponent
    # Win
    case "Z":
      for w in WINNING:
        if opponent == w[1]:
          return w[0]

def get_moves (round):
  opponent = translate_move (round[0])
  return opponent, get_my_move (opponent, round[1])

def main ():
  input = stdin.read ().strip ().split ('\n')

  print (sum ([
    play (map (translate_move, round.split (' ')))
    for round in input
  ]))

  print (sum ([
    play (get_moves (round.split (' ')))
    for round in input
  ]))

if __name__ == "__main__":
  main ()

