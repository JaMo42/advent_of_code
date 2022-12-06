module Main where
import Control.Monad
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.List as List
import qualified Data.Maybe as Maybe

substr :: String -> Int -> Int -> String
substr str start len = Text.unpack $ (Text.take (len) . Text.drop start) (Text.pack str)

validStartMarker :: String -> Bool
validStartMarker bytes = Set.size (Set.fromList bytes) == length bytes

startOfPacketMarker :: String -> Int -> Int
startOfPacketMarker packet count =
  Maybe.fromJust maybeIndex + count
  where
   indices = [0 .. ((length packet) - count)]
   pred start = validStartMarker $ substr packet start count
   maybeIndex = List.findIndex pred indices

main :: IO ()
main = do input <- getContents
          forM_ (lines input) $ \packet -> do
            putStrLn $ "1: " ++ show (startOfPacketMarker packet 4)
            putStrLn $ "2: " ++ show (startOfPacketMarker packet 14)
