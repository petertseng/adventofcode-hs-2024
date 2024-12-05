import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOn, splitOnOne)

-- Array implementation is possible (100x100 array),
-- but doesn't handle arbitrary page numbers,
-- and doesn't make enough of a difference in runtime.

import Control.Arrow ((***))
import Data.List (partition, sortBy, tails)
import Data.Set (Set)
import qualified Data.Set as Set

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
  let (orders, pages) = (Set.fromList . map ((read *** read) . splitOnOne '|') *** map (map read . splitOn ',')) (splitOnOne "" (lines s)) :: (Set (Int, Int), [[Int]])
      (good, bad) = partition (sorted orders) pages
  print (sum (map middle good))
  print (sum (map (middle . sortByOrders orders) bad))
