import AdventOfCode (readInputFile)

import Prelude hiding (Left, Right)

import Data.Containers.ListUtils (nubOrd)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

type Pos = (Int, Int)
data Dir = Up | Down | Left | Right deriving (Eq)

patrol :: IntMap IntSet -> IntMap IntSet -> (Pos, Pos) -> Pos -> Dir -> [(Pos, Dir)]
patrol rows cols bounds@((yMin, xMin), (yMax, xMax)) guard@(y, x) dir = (guard, dir) : case bump of
  Just b -> patrol rows cols bounds b (turn dir)
  Nothing -> [(exit, dir)]
  where bump = case dir of
          Up    -> fmap (\y' -> (y' + 1, x)) (IntSet.lookupLT y (cols IntMap.! x))
          Down  -> fmap (\y' -> (y' - 1, x)) (IntSet.lookupGT y (cols IntMap.! x))
          Left  -> fmap (\x' -> (y, x' + 1)) (IntSet.lookupLT x (rows IntMap.! y))
          Right -> fmap (\x' -> (y, x' - 1)) (IntSet.lookupGT x (rows IntMap.! y))
        exit = case dir of
          Up    -> (yMin, x)
          Down  -> (yMax, x)
          Left  -> (y, xMin)
          Right -> (y, xMax)

loops :: Eq a => [a] -> Bool
loops xs = race (drop 2 xs) (drop 1 xs)
  where race [] _  = False
        race [_] _ = False
        race _ []  = error "slow ended before fast"
        race (fast:_) (slow:_) | fast == slow = True
        race (_:_:fasts) (_:slows) = race fasts slows

{-
-- slower!
loops2 :: Ord a => [a] -> Bool
loops2 = loops' Set.empty
  where loops' _ [] = False
        loops' s (x:_) | x `Set.member` s = True
        loops' s (x:xs) = loops' (Set.insert x s) xs
-}

expandSegments :: [(Pos, Dir)] -> [Pos]
expandSegments [] = []
expandSegments [_] = []
expandSegments (((y, x), dir):rest@(((toy, tox), _):_)) = seg ++ expandSegments rest
  where seg = case dir of
          Up    -> [(cury, x) | cury <- [y, y - 1 .. toy]]
          Down  -> [(cury, x) | cury <- [y .. toy]]
          Left  -> [(y, curx) | curx <- [x, x - 1 .. tox]]
          Right -> [(y, curx) | curx <- [x .. tox]]

turn :: Dir -> Dir
turn Up    = Right
turn Right = Down
turn Down  = Left
turn Left  = Up

enumGrid :: [[a]] -> [(Pos, a)]
enumGrid = concat . zipWith enumRow [0..]
  where enumRow y = zipWith (\x cell -> ((y, x), cell)) [0..]

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

main :: IO ()
main = do
  s <- readInputFile
  let grid = enumGrid (lines s)
      bounds@((yMin, xMin), (yMax, xMax)) = (fst (head grid), fst (last grid))
      coords c = map fst (filter ((== c) . snd) grid)
      blocks = coords '#'
      rows = IntMap.fromAscList [(y, IntSet.fromAscList (map snd (filter ((== y) . fst) blocks))) | y <- [yMin .. yMax]]
      cols = IntMap.fromAscList [(x, IntSet.fromAscList (map fst (filter ((== x) . snd) blocks))) | x <- [xMin .. xMax]]
      guard = case coords '^' of
        [x] -> x
        [] -> error "no guards"
        (_:_) -> error "too many guards"
      guardPath = nubOrd (expandSegments (patrol rows cols bounds guard Up))
      loopsGuard (obsy, obsx) = loops (patrol (IntMap.adjust (IntSet.insert obsx) obsy rows) (IntMap.adjust (IntSet.insert obsy) obsx cols) bounds guard Up)
  print (length guardPath)
  print (count loopsGuard guardPath)
