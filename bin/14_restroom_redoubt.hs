{-# LANGUAGE TupleSections #-}

import AdventOfCode (readInputFileAndFlags)
import AdventOfCode.Split (splitOnOne)

import Control.Monad (when)
import qualified Data.Map as Map
import Control.Arrow ((***))
import Data.List (find, transpose)
import Data.Maybe (fromJust)

type Robot = ((Int, Int), (Int, Int))

poses :: Int -> Int -> Robot -> [(Int, Int)]
poses height width (pos, (vx, vy)) = iterate (((`mod` width) . (+ vx)) *** ((`mod` height) . (+ vy))) pos

robot :: String -> Robot
robot s = case words s of
  ['p':'=':p, 'v':'=':v] -> (vec p, vec v)
  _ -> error ("bad robot " ++ s)

vec :: String -> (Int, Int)
vec = (read *** read) . splitOnOne ','

-- how many times the mode appears,
-- freqOfMode [1, 1, 1, 1, 1] == 5
freqOfMode :: Ord a => [a] -> Int
freqOfMode xs = maximum (Map.elems freq)
  where freq = Map.fromListWith (+) (map (, 1) xs)

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

main :: IO ()
main = do
  (s, flags) <- readInputFileAndFlags
  let robots = map robot (lines s)
      width = maybe 101 read (lookup 'w' flags)
      height = maybe 103 read (lookup 'h' flags)
      t100 = map ((!! 100) . poses height width) robots
      quadrants = map (compare (width `quot` 2) *** compare (height `quot` 2)) t100
      quadrant q = count (== q) quadrants
  print (quadrant (LT, LT) * quadrant (LT, GT) * quadrant (GT, LT) * quadrant (GT, GT))
  let posesOverTime = transpose (map (poses height width) robots) :: [[(Int, Int)]]
      (_, yt) = maximum (zip (map (freqOfMode . map snd) (take height posesOverTime)) [0..])
      (_, xt) = maximum (zip (map (freqOfMode . map fst) (take width posesOverTime)) [0..])
  when (length robots > 12) $ print (fromJust (find ((== xt) . (`rem` width)) [yt, yt + height..]))
