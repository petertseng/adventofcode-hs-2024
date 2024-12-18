import AdventOfCode (readInputFileAndFlags)
import AdventOfCode.Search (bfs)
import AdventOfCode.Split (splitOnOne)

import Control.Arrow ((***))
import Data.Either (fromRight, rights)
import qualified Data.Set as Set

type Pos = (Int, Int)

neigh :: Pos -> [Pos]
neigh (y, x) = [(y - 1, x), (y, x - 1), (y, x + 1), (y + 1, x)]

adj4 :: Pos -> [Pos]
adj4 (y, x) = [(y - 1, x), (y, x - 1), (y, x + 1), (y + 1, x)]

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

  let reachableFrom wall already = Set.fromList . map snd . rights . bfs (filter (\v -> ok v && v `Set.notMember` wall && v `Set.notMember` already) . neigh) (const True)
      blocker (x:xs) wall already | any (`Set.member` already) (adj4 x) = if (0, 0) `Set.member` new then x else blocker xs (Set.delete x wall) (Set.union already new)
        where new = reachableFrom wall already x
      blocker (x:xs) wall already = blocker xs (Set.delete x wall) already
      blocker [] _ _ = error "never reached"
  let (y, x) = blocker (reverse pts) (Set.fromList pts) (reachableFrom (Set.fromList pts) Set.empty (size, size))
  putStrLn (show y ++ ',' : show x)
