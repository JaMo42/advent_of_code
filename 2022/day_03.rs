use std::collections::BTreeSet;
use std::io::{stdin, BufRead};

const fn priority (ch: char) -> i32 {
  if ch.is_ascii_lowercase () {
    ch as i32 - 'a' as i32 + 1
  } else {
    ch as i32 - 'A' as i32 + 27
  }
}

fn main () {
  let input: Vec<_> = stdin ()
    .lock ()
    .lines ()
    .filter_map (|maybe_line| {
      maybe_line
        .ok ()
        .and_then (|line| if line.is_empty () { None } else { Some (line) })
    })
    .collect ();

  let priority_sum: i32 = input
    .iter ()
    .map (|line| {
      let len = line.chars ().count ();
      let first: BTreeSet<_> = line[..(len / 2)].chars ().collect ();
      let second: BTreeSet<_> = line[(len / 2)..].chars ().collect ();
      (first, second)
    })
    .map (|(first, second)| {
      first
        .intersection (&second)
        .map (|item| priority (*item))
        .fold (0, |a, b| i32::max (a, b))
    })
    .sum ();
  println! ("{priority_sum}");

  let sticker_sum: i32 = input
    .as_slice ()
    .chunks (3)
    .map (|group| {
      let rucksacks: Vec<BTreeSet<_>> = group
        .iter ()
        .map (|line| line.chars ().collect ())
        .collect ();
      priority (
        *rucksacks
          .iter ()
          .fold (rucksacks[0].clone (), |a, b| {
            a.intersection (b).cloned ().collect ()
          })
          .iter ()
          .next ()
          .unwrap (),
      )
    })
    .sum ();
  println! ("{sticker_sum}");
}
