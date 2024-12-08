import AdventOfCode (readInputFile)

import Control.Arrow ((***))
import Data.Containers.ListUtils (nubOrd)
import qualified Data.Map as Map

type Pos = (Int, Int)

resonant :: Pos -> Pos -> [Pos]
resonant (y1, x1) p2@(y2, x2) = iterate ((+ dy) *** (+ dx)) p2
  where dy = y2 - y1
        dx = x2 - x1

enumGrid :: [[a]] -> [(Pos, a)]
enumGrid = concat . zipWith enumRow [0..]
  where enumRow y = zipWith (\x cell -> ((y, x), cell)) [0..]

main :: IO ()
main = do
  s <- readInputFile
  let city = enumGrid (lines s)
      notDot = filter ((/= '.') . snd) city

      byFreq = Map.fromListWith (++) (map (\(pos, freq) -> (freq, [pos])) notDot)
      res = [resonant a1 a2 | ants <- Map.elems byFreq, a1 <- ants, a2 <- ants, a1 /= a2]
      -- could just do this, but it is in fact slower.
      --res = [resonant a1 a2 | (a1, c1) <- notDot, (a2, c2) <- notDot, a1 /= a2, c1 == c2]

      ((yMin, xMin), (yMax, xMax)) = (fst (head city), fst (last city))
      inBounds (y, x) = yMin <= y && y <= yMax && xMin <= x && x <= xMax

  print (length (nubOrd (filter inBounds (map (!! 1) res))))
  print (length (nubOrd (concatMap (takeWhile inBounds) res)))
