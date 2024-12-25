import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOn)

import Data.Bits ((.&.))
import Data.Either (partitionEithers)
import Data.List (foldl')

lockOrKey :: [String] -> Either Int Int
lockOrKey s = typ (foldl' (\acc b -> acc * 2 + bit b) 0 (concat (init (tail s))))
  where typ | all (== '#') (head s) && all (== '.') (last s) = Left
            | all (== '#') (last s) && all (== '.') (head s) = Right
            | otherwise = error ("bad " ++ unlines s)
        bit '#' = 1
        bit '.' = 0
        bit c = error (c : " bad bit")

main :: IO ()
main = do
  s <- readInputFile
  let schema = splitOn "" (lines s)
      (locks, keys) = partitionEithers (map lockOrKey schema)
  print (length [() | l <- locks, k <- keys, l .&. k == 0])
