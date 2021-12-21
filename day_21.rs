use std::io::{self, BufRead};

#[derive(Clone)]
struct Player
{
  position: u64,
  score: u64
}

struct Dice
{
  state: u64,
  rolls: u64
}

impl Dice
{
  fn new () -> Dice
  {
    Dice {state: 0, rolls: 0}
  }

  fn next (&mut self) -> u64
  {
    self.state = self.state % 100 + 1;
    self.rolls += 1;
    self.state
  }

  fn roll (&mut self, times: u8) -> u64
  {
    let mut r = 0;
    for _ in 0..times {
      r += self.next ();
    }
    r
  }
}

impl Player
{
  fn new (initial_position: u64) -> Player
  {
    Player {position: initial_position, score: 0}
  }

  fn r#move (&mut self, roll: u64) -> bool
  {
    self.position = self.position + roll;
    if self.position > 10 {
      self.position = (self.position - 1) % 10 + 1;
    }
    self.score += self.position;
    self.score >= 1000
  }
}

fn get_players () -> Vec<Player>
{
  return io::stdin ().lock ().lines ().map (|x| {
    Player::new (x.unwrap ()[28..].parse::<u64> ().unwrap ())
  }).collect::<Vec<Player>> ();
}

fn part_one (players: &mut Vec<Player>) -> u64
{
  let mut dice = Dice::new ();
  let loser: usize;
  loop {
    if players[0].r#move (dice.roll (3)) {
      loser = 1;
      break;
    }
    else if players[1].r#move (dice.roll (3)) {
      loser = 0;
      break;
    }
  }
  players[loser].score * dice.rolls
}

// Combinations of position-1, score-1, position-2, score-2, turn (from right
// to left), storing the number of wins for each player
type Universes = [[[[[(i64, i64); 2]; 21]; 10]; 21]; 10];

// Number of rolls that sum up to 0~9 (rolling 3-sided dice 3 times)
const POSSIBILITIES_FOR_ROLL: [i64; 10] = [0, 0, 0, 1, 3, 6, 7, 6, 3, 1];

unsafe fn part_two_impl (universes: &mut Universes, p1: usize, s1: usize,
                         p2: usize, s2: usize, turn: usize) -> (i64, i64)
{
  // No need to care for higher scores since we just need the number of wins,
  // not their scored. Also need to limit this for array size
  if s1 >= 21 {
    return (1, 0);
  }
  if s2 >= 21 {
    return (0, 1);
  }
  let mut my_wins = universes[p1][s1][p2][s2][turn];
  // Already processed this universe
  if my_wins.0 != -1 || my_wins.1 != -1 {
    return my_wins;
  }
  my_wins = (0, 0);
  let mut result: (i64, i64);
  for roll in 3..=9 {
    if turn == 1 {
      let next_p = (p1 + roll) % 10;
      let next_s = s1 + next_p + 1;  // Add 1 back since we are using 0-based positions
      result = part_two_impl (universes, next_p, next_s, p2, s2, 0);
    }
    else {
      let next_p = (p2 + roll) % 10;
      let next_s = s2 + next_p + 1;
      result = part_two_impl (universes, p1, s1, next_p, next_s, 1);
    }
    my_wins.0 += result.0 * POSSIBILITIES_FOR_ROLL[roll];
    my_wins.1 += result.1 * POSSIBILITIES_FOR_ROLL[roll];
  }
  universes[p1][s1][p2][s2][turn] = my_wins;
  return my_wins;
}

unsafe fn part_two (players: &mut Vec<Player>) -> u64
{
  // Note: this makes the binary pretty large but I don't want to deal with
  // dyanmically allocating this many dimensions
  static mut UNIVERSES: Universes = [[[[[(-1, -1); 2]; 21]; 10]; 21]; 10];
  // Uncomment to call multiple times:
  //UNIVERSES = [[[[[(-1, -1); 2]; 21]; 10]; 21]; 10];
  // using zero-based positions for simplicity
  let (p1_wins, p2_wins) = part_two_impl (&mut UNIVERSES,
                                          players[0].position as usize - 1, 0,
                                          players[1].position as usize - 1, 0,
                                          1);
  return (if p1_wins > p2_wins { p1_wins } else { p2_wins }) as u64;
}

fn main ()
{
  let players = get_players ();
  println! ("Part one: \x1b[92m{}\x1b[0m", part_one (&mut players.clone ()));
  unsafe {
    println! ("Part two: \x1b[92m{}\x1b[0m", part_two (&mut players.clone ()));
  }
}

