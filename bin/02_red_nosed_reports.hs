import AdventOfCode (readInputFile)

import Data.List (inits, tails)

good1 :: [Int] -> Bool
good1 [] = True
good1 [_] = True
good1 (a:b:_) | a == b = False
good1 xs@(a:b:_) = all (\(x, y) -> let d = y - x in mn <= d && d <= mx) (zip xs (drop 1 xs))
  where mn = if a < b then 1 else -3
        mx = if a < b then 3 else -1

good2 :: [Int] -> Bool
good2 xs = any good1 candidates
  where candidates = zipWith (++) (inits xs) (drop 1 (tails xs))

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

main :: IO ()
main = do
  s <- readInputFile
  let reports = map (map read . words) (lines s)
  print (count good1 reports)
  print (count good2 reports)
