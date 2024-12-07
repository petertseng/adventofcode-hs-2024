import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOn, splitOnOne)

import Data.Set (Set)
import qualified Data.Set as Set

import Control.Arrow ((***))
import Data.List (partition, sortBy, tails)

sorted :: Ord a => Set (a, a) -> [a] -> Bool
sorted orders xs = all (`Set.notMember` orders) [(y, x) | x:ys <- tails xs, y <- ys]

sortByOrders :: Ord a => Set (a, a) -> [a] -> [a]
-- strictly speaking we should say EQ if x == y,
-- by the contract of sortBy (should be reflexive)
-- but it doesn't matter for today's problem.
sortByOrders orders = sortBy (\x y -> if (x, y) `Set.member` orders then LT else GT)

middle :: [a] -> a
middle xs = xs !! (length xs `quot` 2)

main :: IO ()
main = do
  s <- readInputFile
  let (orders, pages) = case splitOn "" (lines s) of
        [o, p] -> (Set.fromList (map ((read *** read) . splitOnOne '|') o), map (map read . splitOn ',') p :: [[Int]])
        _ -> error ("bad orders/pages " ++ s)
      (good, bad) = partition (sorted orders) pages
  print (sum (map middle good))
  print (sum (map (middle . sortByOrders orders) bad))
