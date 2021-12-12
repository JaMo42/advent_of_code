use std::io::{self, BufRead};
use std::collections::BTreeMap;

type NodeId = u64;
type NodeMap = BTreeMap::<NodeId, Vec<NodeId>>;
type Constraint = fn (&NodeId, &Vec<NodeId>) -> bool;

const START_NODE_ID: NodeId = 495873782388u64;
const END_NODE_ID: NodeId = 6647396u64;

fn make_node_id (name: &str) -> NodeId
{
  let mut id: NodeId = 0;
  for c in name.chars () {
    id = id << 8 | (c as u64)
  }
  id
}

fn node_is_small (n: &NodeId) -> bool
{
  // Check if the first character of the node was lower case, all characters
  // in the node names have the same case.
  return (n & 32) == 32;
}

fn count_paths_impl (node_map: &NodeMap, path_count: &mut u64, node: NodeId,
                     path: Vec<NodeId>, constraint: Constraint)
{
  if node == END_NODE_ID {
    *path_count += 1;
    return;
  }
  for n in &node_map[&node] {
    if *n != START_NODE_ID && constraint (&n, &path) {
      let mut new_path = path.clone ();
      new_path.push (*n);
      count_paths_impl (node_map, path_count, *n, new_path, constraint);
    }
  }
}

fn count_paths (node_map: &NodeMap, constraint: Constraint) -> u64
{
  let mut path_count = 0u64;
  count_paths_impl (&node_map, &mut path_count, START_NODE_ID,
                    vec![START_NODE_ID], constraint);
  return path_count;
}

fn part_one_constraint (node: &NodeId, path: &Vec<NodeId>) -> bool
{
  return !(node_is_small (node) && path.contains (node));
}

fn count<T: std::cmp::PartialEq> (x: &T, v: &Vec<T>) -> usize
{
  let mut c: usize = 0;
  for i in v {
    if i == x {
      c += 1;
    }
  }
  return c;
}

fn part_two_constraint (node: &NodeId, path: &Vec<NodeId>) -> bool
{
  if node_is_small (node) && path.contains (node) {
    for n in path {
      if node_is_small (n) && count (n, path) > 1 {
        return false;
      }
    }
  }
  return true;
}

fn main () {
  let stdin = io::stdin ()
              .lock ()
              .lines ()
              .map (|x| x.unwrap ())
              .collect::<Vec<String>> ();
  let mut node_map = NodeMap::new ();

  for line in stdin {
    let mut split = line.split ("-");
    let from = make_node_id (split.next ().unwrap ());
    let to = make_node_id (split.next ().unwrap ());
    // Ensure both nodes exist in the map
    if !node_map.contains_key (&from) {
      node_map.insert (from, Vec::<NodeId>::new ());
    }
    if !node_map.contains_key (&to) {
      node_map.insert (to, Vec::<NodeId>::new ());
    }
    // Add each as neighbor of the other
    node_map.get_mut (&from).unwrap ().push (to);
    node_map.get_mut (&to).unwrap ().push (from);
  }

  let part_one = count_paths (&node_map, part_one_constraint);
  let part_two = count_paths (&node_map, part_two_constraint);

  println! ("Part one: \x1b[92m{}\x1b[0m", part_one);
  println! ("Part two: \x1b[92m{}\x1b[0m", part_two);
}

