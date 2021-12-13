use std::io::{self, BufRead};
use std::collections::HashSet;

fn parse_input (input: &Vec<String>, dots: &mut HashSet<(u64, u64)>,
                instructions: &mut Vec<(char, u64)>)
{
  let mut i: usize = 0;
  // Read dots until we hit the separating empty line
  while input[i].len () != 0 {
    let mut xy = input[i].split (',').map (|x| x.parse::<u64> ().unwrap ());
    dots.insert ((xy.next ().unwrap (), xy.next ().unwrap ()));
    i += 1;
  }
  // Then read instructions until input is exhausted
  i += 1;
  while i < input.len () {
    let mut axis_and_value = input[i].split (' ').last ().unwrap ().split ('=');
    let axis = axis_and_value.next ().unwrap ().chars ().next ().unwrap ();
    let value = axis_and_value.next ().unwrap ().parse::<u64> ().unwrap ();
    instructions.push ((axis, value));
    i += 1;
  }
}

fn do_fold (dots: &mut HashSet<(u64, u64)>, (axis, pos): &(char, u64))
{
  let before = dots.clone ();
  *dots = HashSet::new ();

  for dot in before {
    let (x, y) = dot;
    match axis {
      'x' => {
        dots.insert ((if x > *pos { *pos - (x - *pos)} else { x }, y));
      },
      'y' => {
        dots.insert ((x, if y > *pos { *pos - (y - *pos)} else { y }));
      },
      _ => panic! ("Invalid fold axis: {}", axis)
    }
  }
}


fn print_paper (dots: &HashSet<(u64, u64)>)
{
  let mut width = 0;
  let mut height = 0;
  // Get dimensions
  for d in dots {
    let (x, y) = d;
    if *x > width {
      width = *x;
    }
    if *y > height {
      height = *y;
    }
  }
  // Print dots
  for y in 0..=height {
    for x in 0..=width {
      if dots.contains (&(x, y)) {
        print! ("\x1b[92m#\x1b[0m");
      }
      else {
        print! (" ");
      }
    }
    print! ("\n");
  }
}

fn main ()
{
  let stdin = io::stdin ()
              .lock ()
              .lines ()
              .map (|x| x.unwrap ())
              .collect::<Vec<String>> ();
  let mut dots = HashSet::<(u64, u64)>::new ();
  let mut instructions_list = Vec::<(char, u64)>::new ();
  parse_input (&stdin, &mut dots, &mut instructions_list);
  let mut instructions = instructions_list.iter ();

  do_fold (&mut dots, instructions.next ().unwrap ());
  println! ("There \x1b[92m{}\x1b[0m dots after the first fold", dots.len ());

  for i in instructions {
    do_fold (&mut dots, i);
  }

  println! ("The code is:");
  print_paper (&dots);
}

