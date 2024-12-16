import AdventOfCode (readInputFile)
import AdventOfCode.Search (astarInt)

import Data.Containers.ListUtils (nubOrd)
import Data.Array.Unboxed ((!), UArray, assocs, listArray)
import Data.Maybe (fromJust)

type Pos = (Int, Int)

posArr :: [String] -> UArray Pos Char
posArr rows = listArray ((1, 1), (height, width)) (concat rows)
  where height = length rows
        width = uniform length rows

uniform :: Eq b => (a -> b) -> [a] -> b
uniform _ [] = error "empty uniform"
uniform f (x:xs) | any ((/= f x) . f) xs = error "inconsistent uniform"
uniform f (x:_) = f x

main :: IO ()
main = do
  s <- readInputFile
  let a = posArr (lines s)
      coords c = map fst (filter ((== c) . snd) (assocs a))
      coord c = case coords c of
        [x] -> x
        [] -> error ("no " ++ show c)
        (_:_) -> error ("too many " ++ show c)
      start = coord 'S'
      end = coord 'E'
      specialEnd = ((0, 0), (0, 0))
      neigh (pos, _) | pos == end = [(1, specialEnd)]
      neigh ((y, x), (dy, dx)) = [(tcost + 1, (np, (ndy, ndx))) | (tcost, ndy, ndx) <- [(0, dy, dx), (1000, -dx, dy), (1000, dx, -dy)], let np = (y + ndy, x + ndx), a ! np /= '#']
      compress ((y, x), (dy, dx)) = y * 9000 + x * 9 + (dy + 1) * 3 + dx + 1
      (cost, paths) = fromJust (astarInt compress neigh (const 0) (== specialEnd) (start, (0, 1)))
  print (cost - 1)
  print (length (nubOrd (map fst (concat paths))) - 1)
