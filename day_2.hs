module Main where

-- Tuple data: (Position, Depth)

readInputLines :: IO [String]
readInputLines = lines <$> getContents

-- Read lines into tuple containing position and depth changes

processLine :: String -> (Int, Int)
processLine line =
  let [command, amount] = words line
  in
    if command == "forward" then (read amount, 0)
    else if command == "down" then (0, read amount)
    else if command == "up" then (0, - read amount)
    else error "Invalid input"

processLines :: [String] -> [(Int, Int)]
processLines lines = map processLine lines

-- Add the position and depth values together

addTuple :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTuple (a, b) (x, y) = (a+x, b+y)

mySum :: [(Int, Int)] -> (Int, Int)
mySum xs = foldr addTuple (0, 0) xs

-- Multiply position and depth for solution

myMultiply :: (Int, Int) -> Int
myMultiply (a, b) = a * b

main :: IO ()
main = do lines <- readInputLines
          print (myMultiply (mySum (processLines lines)))

