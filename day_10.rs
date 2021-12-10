use std::io::{self, BufRead};

const OPENING: &str = "([{<";
const CLOSING: &str = ")]}>";

fn closing_for_chunk_type (chunk_type: char) -> char
{
  match chunk_type
    {
      '(' => ')',
      '[' => ']',
      '{' => '}',
      '<' => '>',
      _ => panic! ("Invalid chunk type: {}", chunk_type)
    }
}


fn illegal_character_value (c: char) -> u64
{
  match c
    {
      ')' => 3,
      ']' => 57,
      '}' => 1197,
      '>' => 25137,
      _ => panic! ("Invalid illegal character: {}", c)
    }
}


fn completion_character_value (c: char) -> u64
{
  match c
    {
      '(' => 1,
      '[' => 2,
      '{' => 3,
      '<' => 4,
      _ => panic! ("Invalid completion character: {}", c)
    }
}


fn syntax_error_score (line: &String) -> u64
{
  let mut type_stack = Vec::<char>::new ();

  for c in line.chars ()
    {
      if OPENING.contains (c)
        {
          type_stack.push (c);
        }
      else if CLOSING.contains (c)
        {
          if c != closing_for_chunk_type (type_stack.pop ().unwrap ())
            {
              return illegal_character_value (c);
            }
        }
      else
        {
          panic! ("Invalid character in input: {}", c);
        }
    }
  0
}


fn completion_score (line: &String) -> u64
{
  let mut type_stack = Vec::<char>::new ();

  for c in line.chars ()
    {
      if OPENING.contains (c)
        {
          type_stack.push (c);
        }
      else
        {
          // This function is only called on lines with an error score of 0
          // so we do not need to do any checks here
          type_stack.pop ();
        }
    }

  return type_stack
         .iter ()
         .rev ()
         .fold (0u64, |s, t| s * 5 + completion_character_value (*t));
}


fn main ()
{
  let stdin = io::stdin ()
              .lock ()
              .lines ()
              .map (|x| x.unwrap ())
              .collect::<Vec<String>> ();

  let mut total_error_score = 0;
  let mut completion_scores = Vec::<u64>::new ();

  for line in stdin
    {
      let error_score = syntax_error_score (&line);
      total_error_score += error_score;
      if error_score == 0
        {
          completion_scores.push (completion_score (&line));
        }
    }

  completion_scores.sort_unstable ();
  let completion_middle = completion_scores[completion_scores.len () / 2];

  println! ("The total syntax error score is \x1b[92m{}\x1b[0m",
            total_error_score);
  println! ("The middle autocomplete score is \x1b[92m{}\x1b[0m",
            completion_middle);
}

