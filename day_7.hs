module Main where

{- Input -}

splitToInts :: (Char -> Bool) -> String -> [Int]
splitToInts pred str = case dropWhile pred str of
                            "" -> []
                            s' -> read word : splitToInts pred s''
                                  where (word, s'') = break pred s'

splitToIntsOnComma = splitToInts (== ',')

readInput :: IO [Int]
readInput = splitToIntsOnComma <$> getContents

{- Helpers -}

-- Sum all elements in list
sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs

-- Get the index of an element in a list
indexOf :: [Int] -> Int -> Int
indexOf xs v = head $ filter ((== v) . (xs !!)) [0..]

-- Calculate constant fuel usage
doConstantMove :: Int -> Int -> Int
doConstantMove from to = abs (from - to)

-- Calculate sum of list [1..n] without actually creating a list and summing it
acceleratedFuelUsage :: Int -> Int
acceleratedFuelUsage n = (n * (n + 1) `div` 2)

-- Calculate accelerated fuel usage
doAcceleratedMove :: Int -> Int -> Int
doAcceleratedMove from to = acceleratedFuelUsage $ abs (from - to)

-- Calculate the costs for each crab to move to a given position
moveTo :: Bool -> [Int] -> Int -> Int
moveTo False crabs pos = sumList $ map (doConstantMove pos) crabs
moveTo True crabs pos = sumList $ map (doAcceleratedMove pos) crabs

-- Calculate the costs of the moves for each crab to the small position of
-- any crab to the biggest
getMoves :: [Int] -> Bool -> [Int]
getMoves crabs accel = (map (moveTo accel crabs)
                            ([(minimum crabs) .. (maximum crabs)]))

{- Parts -}

partOne :: [Int] -> (Int, Int)
partOne crabs = (best, best_pos)
                where
                  moves = getMoves crabs False
                  best = minimum moves
                  best_pos = (indexOf moves best) + (minimum crabs)

partTwo :: [Int] -> (Int, Int)
partTwo crabs = (best, best_pos)
                where
                  moves = getMoves crabs True
                  best = minimum moves
                  best_pos = (indexOf moves best) + (minimum crabs)

{- Main -}

formatResult :: (Int, Int) -> String
formatResult (usage, pos) = ("Best move: Position " ++ show pos
                             ++ ", with a fuel usage of \x1b[92m" ++ show usage
                             ++ "\x1b[0m")

formatOutput :: Int -> Int -> String
formatOutput part1 part2 = ("Part one: \x1b[92m" ++ show part1 ++ "\x1b[0m\n"
                            ++ "Part two: \x1b[92m" ++ show part2 ++ "\x1b[0m")


main :: IO ()
main = do crabs <- readInput
          putStrLn ("Constant fuel usage:\n"
                    ++ (formatResult (partOne crabs))
                    ++ "\nAccelerated fuel usage:\n"
                    ++ (formatResult (partTwo crabs)))

