{-# LANGUAGE TupleSections #-}

import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOnOne)

import Control.Arrow (second)
import Control.Monad (foldM)
import Data.Foldable (toList)
import Data.List (foldl')
import Data.Set (Set)
import qualified Data.Set as Set

type Pos = (Int, Int)
data Dir = North | South | West | East

push :: Set Pos -> (Set Pos, Pos) -> Dir -> (Set Pos, Pos)
push wall (box, robot) d | robot' `Set.member` wall = (box, robot)
                         | robot' `Set.member` box = maybe (box, robot) ((, robot') . Set.delete robot') (tryBox robot')
                         | otherwise = (box, robot')
  where robot' = step d robot
        tryBox b | b' `Set.member` wall = Nothing
                 | b' `Set.member` box = tryBox b'
                 | otherwise = Just (Set.insert b' box)
          where b' = step d b

widePush :: Set Pos -> (Set Pos, Pos) -> Dir -> (Set Pos, Pos)
widePush wall (box, robot) d | robot' `Set.member` wall = (box, robot)
                             | robot' `Set.member` box = maybe (box, robot) (, robot') (tryBox box robot')
                             | step West robot' `Set.member` box = maybe (box, robot) (, robot') (tryBox box (step West robot'))
                             | otherwise = (box, robot')
  where robot' = step d robot
        tryBox bs b | any (`Set.member` wall) potentialWallCollisions = Nothing
                    | otherwise = fmap (Set.insert b' . Set.delete b) (foldM tryBox bs boxCollisions)
          where boxCollisions = filter (`Set.member` box) potentialBoxCollisions
                potentialWallCollisions = case d of
                  East  -> [step East b']
                  West  -> [b']
                  North -> [b', step East b']
                  South -> [b', step East b']
                potentialBoxCollisions = case d of
                  East  -> [step East b']
                  West  -> [step West b']
                  North -> [step West b', b', step East b']
                  South -> [step West b', b', step East b']
                b' = step d b

gps :: Foldable f => f Pos -> Int
gps = sum . map (\(y, x) -> y * 100 + x) . toList

step :: Dir -> Pos -> Pos
step North (y, x) = (y - 1, x)
step South (y, x) = (y + 1, x)
step East  (y, x) = (y, x + 1)
step West  (y, x) = (y, x - 1)

dir :: Char -> Dir
dir '^' = North
dir '>' = East
dir '<' = West
dir 'v' = South
dir c = error (c : " bad char")

enumGrid :: [[a]] -> [(Pos, a)]
enumGrid = concat . zipWith enumRow [0..]
  where enumRow y = zipWith (\x cell -> ((y, x), cell)) [0..]

main :: IO ()
main = do
  s <- readInputFile
  let (ware, ds) = splitOnOne "" (lines s)
      dirs = map dir (concat ds)
      grid = enumGrid ware
      coords c = map fst (filter ((== c) . snd) grid)
      wall = Set.fromAscList (coords '#')
      box = Set.fromAscList (coords 'O')
      robot = case coords '@' of
        [r] -> r
        [] -> error "no robot"
        (_:_) -> error "too many robot"
  print (gps (fst (foldl' (push wall) (box, robot) dirs)))
  let wide = second (* 2)
      halfWall = Set.mapMonotonic wide wall
      wall' = Set.union halfWall (Set.mapMonotonic (second succ) halfWall)
  print (gps (fst (foldl' (widePush wall') (Set.mapMonotonic wide box, wide robot) dirs)))
