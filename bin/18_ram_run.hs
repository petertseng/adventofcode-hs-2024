import AdventOfCode (readInputFileAndFlags)
import AdventOfCode.Search (bfs)
import AdventOfCode.Split (splitOnOne)

import Control.Arrow ((***))
import Data.Either (fromRight, isLeft)
import qualified Data.Set as Set

type Pos = (Int, Int)

neigh :: Pos -> [Pos]
neigh (y, x) = [(y - 1, x), (y, x - 1), (y, x + 1), (y + 1, x)]

bsearch :: (Int -> Bool) -> Int -> Int -> Int
bsearch f low high
  | low > high = high
  | f mid = bsearch f low (mid - 1)
  | otherwise = bsearch f (mid + 1) high
  where mid = low + ((high - low) `div` 2)

main :: IO ()
main = do
  (s, flags) <- readInputFileAndFlags
  let pts = map ((read *** read) . splitOnOne ',') (lines s)
      size = maybe 70 read (lookup 's' flags)
      nbytes = maybe 1024 read (lookup 'n' flags)
      ok (y, x) = 0 <= y && y <= size && 0 <= x && x <= size
      tryWalk n = head (bfs (filter (\v -> ok v && v `Set.notMember` Set.fromList (take n pts)) . neigh) (== (size, size)) (0, 0))
      (dist, _) = fromRight (error "can't reach") (tryWalk nbytes)
  print dist
  -- I'd consider doing a union-find implementation or the resuming flood-fill,
  -- but I'm a litte lazy to do either of them
  let blocked = isLeft . tryWalk
      (y, x) = pts !! bsearch blocked 0 (length pts)
  putStrLn (show y ++ ',' : show x)
