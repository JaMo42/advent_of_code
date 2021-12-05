module Main where

readInputLines :: IO [String]
readInputLines = lines <$> getContents

{-------- Part 1 --------}

-- Process commands

processLine1 :: String -> (Int, Int)
processLine1 line =
  let [command, amount] = words line
  in
    if command == "forward" then (read amount, 0)
    else if command == "down" then (0, read amount)
    else if command == "up" then (0, - read amount)
    else error "Invalid input"

processLines1 :: [String] -> [(Int, Int)]
processLines1 lines = map processLine1 lines

-- Sum values

addTuple1 :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTuple1 (a, b) (x, y) = (a+x, b+y)

sum1 :: [(Int, Int)] -> (Int, Int)
sum1 xs = foldl addTuple1 (0, 0) xs

{-------- Part 2 --------}

-- Process commands

processLine2 :: String -> (Int, Int, Int)
processLine2 line =
  let [command, amount] = words line
  in
    if command == "forward" then (read amount, 0, 0)
    else if command == "down" then (0, 0, read amount)
    else if command == "up" then (0, 0, - read amount)
    else error "Invalid input"

processLines2 :: [String] -> [(Int, Int, Int)]
processLines2 lines = map processLine2 lines

-- Sum values

addTuple2 :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
addTuple2 (a, b, c) (x, y, z) = (a+x, b+x*c, c+z)

sum2 :: [(Int, Int, Int)] -> (Int, Int, Int)
sum2 xs = foldl addTuple2 (0, 0, 0) xs

multiply1 :: (Int, Int) -> Int
multiply1 (a, b) = a * b

multiply2 :: (Int, Int, Int) -> Int
multiply2 (a, b, c) = a * b

{-------- Main --------}

-- Run and output both parts

partOne :: [String] -> Int
partOne lines = multiply1 (sum1 (processLines1 lines))

partTwo :: [String] -> Int
partTwo lines = multiply2 (sum2 (processLines2 lines))

runParts :: [String] -> (Int, Int)
runParts lines = (partOne lines, partTwo lines)

formatOutput :: (Int, Int) -> String
formatOutput (p1, p2) = ("Part one: \x1b[92m" ++ show p1 ++ "\x1b[0m\n"
                         ++ "Part two: \x1b[92m" ++ show p2 ++ "\x1b[0m")

main :: IO ()
main = do lines <- readInputLines
          putStrLn (formatOutput (runParts lines))

