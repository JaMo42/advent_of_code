use std::io::{self, BufRead};
use std::collections::HashSet;
use std::env;

struct SeaFloor
{
  east_facing: HashSet<(usize, usize)>,
  south_facing: HashSet<(usize, usize)>,
  width: usize,
  height: usize
}

impl SeaFloor
{
  fn new (source: &Vec<String>) -> SeaFloor
  {
    let h = source.len ();
    let w = source[0].len ();
    let mut this = SeaFloor {
      east_facing: HashSet::new (),
      south_facing: HashSet::new (),
      width: w,
      height: h
    };
    for y in 0..h {
      for x in 0..w {
        match source[y].as_bytes ()[x] {
          b'.' => {},
          b'>' => { this.east_facing.insert ((x, y)); }
          b'v' => { this.south_facing.insert ((x, y)); }
          _ => { panic! ("Invalid input"); }
        }
      }
    }
    this
  }

  fn show (&self)
  {
    for y in 0..self.height {
      for x in 0..self.width {
        if self.east_facing.contains (&(x, y)) {
          print! (">");
        }
        else if self.south_facing.contains (&(x, y)) {
          print! ("v");
        }
        else {
          print! (".");
        }
      }
      print! ("\n");
    }
  }

  fn occupied (&self, x: usize, y: usize) -> bool
  {
    return self.east_facing.contains (&(x, y)) || self.south_facing.contains (&(x, y));
  }

  fn step (&mut self) -> bool
  {
    unsafe {
      let mut did_move = false;
      static mut TO_MOVE: Vec::<(usize, usize)> = Vec::new ();
      // East facing
      TO_MOVE.clear ();
      for cell in &self.east_facing {
        if !self.occupied ((cell.0 + 1) % self.width, cell.1) {
          TO_MOVE.push (*cell);
        }
      }
      if TO_MOVE.len () > 0 {
        did_move = true;
      }
      for &cell in &TO_MOVE {
        self.east_facing.remove (&(cell.0, cell.1));
        self.east_facing.insert (((cell.0 + 1) % self.width, cell.1));
      }
      // South facing
      TO_MOVE.clear ();
      for cell in &self.south_facing {
        if !self.occupied (cell.0, (cell.1 + 1) % self.height) {
          TO_MOVE.push (*cell);
        }
      }
      if TO_MOVE.len () > 0 {
        did_move = true;
      }
      for &cell in &TO_MOVE {
        self.south_facing.remove (&(cell.0, cell.1));
        self.south_facing.insert ((cell.0, (cell.1 + 1) % self.height));
      }
      did_move
    }
  }
}

fn main ()
{
  let input = io::stdin ()
              .lock ()
              .lines ()
              .map (|x| x.unwrap ())
              .collect::<Vec<String>> ();
  let args = env::args ().collect::<Vec<String>> ();
  let show_sea_floor = args.contains (&"show".to_string ());
  let mut sea_floor = SeaFloor::new (&input);
  if show_sea_floor {
    println! ("Initial state:");
    sea_floor.show ();
  }
  let mut i = 1;
  while sea_floor.step () {
    i += 1;
  }
  println! ("Finished after \x1b[92m{}\x1b[0m steps", i);
  if show_sea_floor {
    sea_floor.show ();
  }
}

