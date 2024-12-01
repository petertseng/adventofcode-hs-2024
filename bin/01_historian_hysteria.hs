import AdventOfCode (readInputFile)

import Data.List (sort, transpose)

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

main :: IO ()
main = do
  s <- readInputFile
  let (as, bs) = case transpose (map (map read . words) (lines s)) of
        [a, b] -> (a :: [Int], b :: [Int])
        [] -> error "no lists"
        [_] -> error "only one list"
        _:_:_ -> error "too many lists"
  print (sum (zipWith (\a b -> abs (a - b)) (sort as) (sort bs)))
  print (sum (map (\a -> a * count (== a) bs) as))
