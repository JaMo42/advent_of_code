#!/usr/bin/env python3
from sys import stdin

def predict_values(history: list[int]) -> tuple[int, int]:
  diffs = [history]
  while True:
    prev = diffs[-1]
    nxt = []
    sum = 0
    for i in range(1, len(prev)):
      d = prev[i] - prev[i - 1]
      sum += d
      nxt.append(d)
    diffs.append(nxt)
    if sum == 0:
      break
  #print(diffs)

  diffs[-1].append(0)
  for i in range(len(diffs) - 2, -1, -1):
    diffs[i].append(diffs[i][-1] + diffs[i + 1][-1])

  diffs[-1].insert(0, 0)
  for i in range(len(diffs) - 2, -1, -1):
    diffs[i].insert(0, diffs[i][0] - diffs[i + 1][0])

  #print(diffs)
  return (diffs[0][0], diffs[0][-1])

def main():
  histories = list(map(
    lambda x: list(map(int, x.split())),
    stdin.read().strip().split('\n'),
  ))
  #print(histories)
  before, after= map(sum, zip(*map(predict_values, histories)))
  print(f"The sum of the next values is \x1b[92m{after}\x1b[m")
  print(f"The sum of the previous values is \x1b[92m{before}\x1b[m")

if __name__ == "__main__":
  main()
