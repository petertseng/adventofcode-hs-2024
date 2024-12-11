{-# LANGUAGE TupleSections #-}

import AdventOfCode (readInputFile)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

blink :: IntMap Int -> IntMap Int
blink m = IntMap.fromListWith (+) (concatMap (seconds blink1) (IntMap.assocs m))
  where seconds f (a, b) = map (, b) (f a)

blink1 :: Int -> [Int]
blink1 0 = [1]
blink1 x | even len = [l, r]
  where s = show x
        len = length s
        p10 = 10 ^ (len `quot` 2)
        (l, r) = x `quotRem` p10
blink1 x = [x * 2024]

main :: IO ()
main = do
  s <- readInputFile
  let stones = map read (words s)
      freq = IntMap.fromListWith (+) (map (, 1) stones)
      blinks = iterate blink freq
  print (sum (IntMap.elems (blinks !! 25)))
  print (sum (IntMap.elems (blinks !! 75)))
