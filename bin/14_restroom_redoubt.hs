{-# LANGUAGE TupleSections #-}

import AdventOfCode (readInputFileAndFlags)
import AdventOfCode.Split (splitOnOne)

import Control.Arrow ((***))
import Control.Monad (when)
import Data.Array.Unboxed (UArray, accumArray, elems)
import Data.List (find, maximumBy)
import Data.Maybe (fromJust)
import Data.Ord (comparing)

type Robot = ((Int, Int), (Int, Int))

posesAt :: Int -> Int -> [Robot] -> Int -> [(Int, Int)]
posesAt height width robots t = map posAt robots
  where posAt ((x, y), (vx, vy)) = ((x + vx * t) `mod` width, (y + vy * t) `mod` height)

robot :: String -> Robot
robot s = case words s of
  ['p':'=':p, 'v':'=':v] -> (vec p, vec v)
  _ -> error ("bad robot " ++ s)

vec :: String -> (Int, Int)
vec = (read *** read) . splitOnOne ','

-- how many times the mode appears,
-- freqOfMode [1, 1, 1, 1, 1] == 5
freqOfMode :: Int -> [Int] -> Int
freqOfMode sz xs = maximum (elems freq)
  where freq = accumArray (+) 0 (0, sz - 1) (map (, 1) xs) :: UArray Int Int

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

main :: IO ()
main = do
  (s, flags) <- readInputFileAndFlags
  let robots = map robot (lines s)
      width = maybe 101 read (lookup 'w' flags)
      height = maybe 103 read (lookup 'h' flags)
      quadrants = map (compare (width `quot` 2) *** compare (height `quot` 2)) (posesAt height width robots 100)
      quadrant q = count (== q) quadrants
  print (quadrant (LT, LT) * quadrant (LT, GT) * quadrant (GT, LT) * quadrant (GT, GT))
  let yt = maximumBy (comparing (freqOfMode height . map snd . posesAt height width robots)) [0 .. height - 1]
      xt = maximumBy (comparing (freqOfMode width . map fst . posesAt height width robots)) [0 .. width - 1]
  when (length robots > 12) $ print (fromJust (find ((== xt) . (`rem` width)) [yt, yt + height..]))
  -- doesn't make a difference in runtime since this isn't the slow part
  -- print ((yt + height * (xt - yt) * gcdExtPos width height) `mod` (width * height))
