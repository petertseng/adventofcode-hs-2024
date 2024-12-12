{-# LANGUAGE FlexibleContexts #-}

import AdventOfCode (readInputFile)

import Control.Monad (when)
import Control.Monad.ST (ST)
import Data.Array.MArray (newArray, newListArray, readArray, writeArray)
import Data.Array.ST (STArray, runSTArray)
import Data.Array.Unboxed ((!?), UArray, assocs, indices, listArray)
import Data.Foldable (for_)
import qualified Data.Map as Map
import qualified Data.Set as Set

type Pos = (Int, Int)

groups :: [Pos] -> [(Pos, Pos)] -> [[Pos]]
groups points pairs = Map.elems $ Map.fromListWith (++) $ map (\(a, b) -> (b, [a])) $ assocs $ runSTArray $ do
  let bounds = (head points, last points)
  parent <- newListArray bounds points :: ST s (STArray s Pos Pos)
  rank   <- newArray bounds 0 :: ST s (STArray s Pos Int)

  let find x = do
        parX <- readArray parent x
        if x /= parX
          then do
            parParX <- find parX
            writeArray parent x parParX
            return parParX
          else return x

  let union x y = do
        parX <- find x
        parY <- find y
        when (parX /= parY) $ do
          rankX <- readArray rank parX
          rankY <- readArray rank parY
          case compare rankX rankY of
            LT -> writeArray parent parX parY
            GT -> writeArray parent parY parX
            EQ -> do
              writeArray parent parY parX
              writeArray rank parX (rankX + 1)
        return (parX /= parY)

  for_ pairs (uncurry union)
  for_ points find
  return parent

price1 :: [Pos] -> Int
price1 s = length s * perimeter s
  where perimeter = sum . map exposedSides
        set = Set.fromList s
        exposedSides pos = 4 - 2 * count (`Set.member` set) (adj2 pos)

price2 :: [Pos] -> Int
price2 s = length s * numSides s
  where numSides = sum . map numCorners
        set = Set.fromList s
        numCorners (y, x) = count id [not l && (not u || ul), not u && (not r || ur), not r && (not d || dr), not d && (not l || dl)]
          where ul = (y - 1, x - 1) `Set.member` set
                u  = (y - 1, x    ) `Set.member` set
                ur = (y - 1, x + 1) `Set.member` set
                dl = (y + 1, x - 1) `Set.member` set
                l  = (y    , x - 1) `Set.member` set
                r  = (y    , x + 1) `Set.member` set
                d  = (y + 1, x    ) `Set.member` set
                dr = (y + 1, x + 1) `Set.member` set

adj2 :: Pos -> [Pos]
adj2 (y, x) = [(y - 1, x), (y, x - 1)]

posArr :: [String] -> UArray Pos Char
posArr rows = listArray ((1, 1), (height, width)) (concat rows)
  where height = length rows
        width = uniform length rows

uniform :: Eq b => (a -> b) -> [a] -> b
uniform _ [] = error "empty uniform"
uniform f (x:xs) | any ((/= f x) . f) xs = error "inconsistent uniform"
uniform f (x:_) = f x

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

main :: IO ()
main = do
  s <- readInputFile
  let garden = posArr (lines s)
      pairs = [(pos, n) | (pos, c) <- assocs garden, n <- adj2 pos, garden !? n == Just c]
      gs = groups (indices garden) pairs
  print (sum (map price1 gs))
  print (sum (map price2 gs))
