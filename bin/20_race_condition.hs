{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

import AdventOfCode (readInputFile)

import Data.Array.Unboxed ((!), (!?), IArray, UArray, array, assocs, bounds, listArray)
import qualified Data.IntMap as IntMap
import Data.Maybe (fromMaybe)

type Pos = (Int, Int)

path :: UArray Pos Bool -> Pos -> Pos -> [Pos]
path walk forbid start = start : case neigh of
  [] -> []
  [x] -> path walk start x
  (_:_) -> error "intersection"
  where neigh = filter (\n -> walk ! n && n /= forbid) (adj4 start)

adj4 :: Pos -> [Pos]
adj4 (y, x) = [(y - 1, x), (y, x - 1), (y, x + 1), (y + 1, x)]

adj2 :: Pos -> [Pos]
adj2 (y, x) = [(y, x + 2), (y + 2, x)]

dposes20 :: [(Int, Int)]
dposes20 = [(dy, dx) | dy <- [0 .. 20], dx <- [-(if dy == 0 then 1 else 20 - dy) .. 20 - dy]]

within20 :: Pos -> [(Pos, Int)]
within20 (y, x) = [((y + dy, x + dx), abs dy + abs dx) | (dy, dx) <- dposes20]

posArr :: IArray UArray e => (Char -> e) -> [String] -> UArray Pos e
posArr f rows = listArray ((1, 1), (height, width)) (map f (concat rows))
  where height = length rows
        width = uniform length rows

uniform :: Eq b => (a -> b) -> [a] -> b
uniform _ [] = error "empty uniform"
uniform f (x:xs) | any ((/= f x) . f) xs = error "inconsistent uniform"
uniform f (x:_) = f x

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

freq :: [Int] -> [(Int, Int)]
freq = IntMap.assocs . IntMap.fromListWith (+) . map (, 1)

main :: IO ()
main = do
  s <- readInputFile
  let walk = posArr (/= '#') (lines s)
      cs = posArr id (lines s)
      coords c = map fst (filter ((== c) . snd) (assocs cs))
      coord c = case coords c of
        [x] -> x
        [] -> error ("no " ++ show c)
        (_:_) -> error ("too many " ++ show c)
      end = coord 'E'
      -- maybe need better name. distsFromEnd is the list, distFromEnd is the array
      distsFromEnd = zip (path walk (-1, -1) end) [0..]
      distFromEnd = array (bounds walk) distsFromEnd :: UArray Pos Int
      printSav savs = if any (>= 100) savs then print (count (>= 100) savs) else print (freq savs)
  printSav [sav | (a, d1) <- distsFromEnd,  b     <-     adj2 a, fromMaybe False (walk !? b), let sav = abs (d1 - distFromEnd ! b) - 2, sav > 0]
  printSav [sav | (a, d1) <- distsFromEnd, (b, d) <- within20 a, fromMaybe False (walk !? b), let sav = abs (d1 - distFromEnd ! b) - d, sav > 0]
