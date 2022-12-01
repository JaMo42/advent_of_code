module Main where

readInputLines :: IO [String]
readInputLines = lines <$> getContents

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

addTuple :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
addTuple (a, b, c) (x, y, z) = (a+x, b+x*c, c+z)

sumData :: [(Int, Int, Int)] -> (Int, Int, Int)
sumData xs = foldl addTuple (0, 0, 0) xs

{-------- Part 1 --------}

partOne :: (Int, Int, Int) -> Int
partOne (pos, _, depth) = pos * depth

{-------- Part 2 --------}

partTwo :: (Int, Int, Int) -> Int
partTwo (pos, depth, aim) = pos * depth

{-------- Main --------}

runParts :: (Int, Int, Int) -> (Int, Int)
runParts input = (partOne input, partTwo input)

formatOutput :: (Int, Int) -> String
formatOutput (p1, p2) = ("Part one: \x1b[92m" ++ show p1 ++ "\x1b[0m\n"
                         ++ "Part two: \x1b[92m" ++ show p2 ++ "\x1b[0m")

main :: IO ()
main = do lines <- readInputLines
          putStrLn (formatOutput (runParts (sumData (processLines lines))))

