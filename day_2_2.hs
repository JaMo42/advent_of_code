module Main where

-- Tuple data: (position, depth, aim)
-- The depth is unused while parsing and gets evaluated when summing the
-- lines together

readInputLines :: IO [String]
readInputLines = lines <$> getContents

-- Read commands

processLine :: String -> (Int, Int, Int)
processLine line =
  let [command, amount] = words line
  in
    if command == "forward" then (read amount, 0, 0)
    else if command == "down" then (0, 0, read amount)
    else if command == "up" then (0, 0, - read amount)
    else error "Invalid input"

processLines :: [String] -> [(Int, Int, Int)]
processLines lines = map processLine lines

-- Sum up

addTuple :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
addTuple (a, b, c) (x, y, z) = (a+x, b+x*c, c+z)

mySum :: [(Int, Int, Int)] -> (Int, Int, Int)
mySum xs = foldl addTuple (0, 0, 0) xs

-- Multiply

myMultiply :: (Int, Int, Int) -> Int
myMultiply (a, b, c) = a * b

main :: IO ()
main = do lines <- readInputLines
          print (myMultiply (mySum (processLines lines)))

