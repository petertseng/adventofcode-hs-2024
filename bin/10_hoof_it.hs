import AdventOfCode (readInputFile)

import Data.Char (digitToInt)
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Pos = (Int, Int)

trailsFrom :: [Set Pos] -> Pos -> Map Pos Int
trailsFrom valids trailhead = foldl' riseNeigh (Map.singleton trailhead 1) valids
  where riseNeigh poses valid = Map.fromListWith (+) [(pos', n) | (pos, n) <- Map.assocs poses, pos' <- neigh pos, pos' `Set.member` valid]

neigh :: Pos -> [Pos]
neigh (y, x) = [(y - 1, x), (y, x - 1), (y, x + 1), (y + 1, x)]

enumGrid :: [[a]] -> [(Pos, a)]
enumGrid = concat . zipWith enumRow [0..]
  where enumRow y = zipWith (\x cell -> ((y, x), cell)) [0..]

main :: IO ()
main = do
  s <- readInputFile
  let grid = enumGrid (map (map (\c -> if c == '.' then -1 else digitToInt c)) (lines s))
      coords c = map fst (filter ((== c) . snd) grid)
      trailheads = coords 0
      valids = map (Set.fromAscList . coords) [1 .. 9]
      trails = map (trailsFrom valids) trailheads
  print (sum (map Map.size trails))
  print (sum (concatMap Map.elems trails))
