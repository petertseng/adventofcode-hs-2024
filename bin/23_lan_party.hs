{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOnOne)

import Control.Arrow (second)
import Data.Array.Unboxed ((!), Array, Ix, UArray, accumArray)
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (toList)
import Data.List (mapAccumL, maximumBy, sort, tails)
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as Set

type Computer = (Char, Char)

cliques :: (Ix a) => Array a (Set a) -> [a] -> [[a]]
cliques neigh nodes = bk Set.empty (Set.fromList nodes) Set.empty
  where bk r p x | Set.null p && Set.null x = [toList r]
        bk r0 p0 x0 = concat (snd (mapAccumL step (p0, x0) (toList (p0 `Set.difference` (neigh ! pivot)))))
          where step (p, x) v = ((Set.delete v p, Set.insert v x), bk (Set.insert v r0) (p `Set.intersection` (neigh ! v)) (x `Set.intersection` (neigh ! v)))
                pivot = head (toList x0 ++ toList p0)

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

main :: IO ()
main = do
  s <- readInputFile
  let pairs = concatMap ((\case ([a, b], [c, d]) -> [((a, b), (c, d)), ((c, d), (a, b))]; p -> error ("bad " ++ show p)) . splitOnOne '-') (lines s)
      computers = nubOrd (map fst pairs)
      exist = accumArray (||) False ((('a', 'a'), ('a', 'a')), (('z', 'z'), ('z', 'z'))) (map (, True) pairs) :: UArray (Computer, Computer) Bool
      neigh = accumArray Set.union Set.empty (('a', 'a'), ('z', 'z')) (map (second Set.singleton) pairs) :: Array Computer (Set Computer)
      triples = [(a, b, c) | a:xs <- tails computers, b:ys <- tails xs, exist ! (a, b), c <- ys, exist ! (a, c), exist ! (b, c)]
      t (('t', _), _, _) = True
      t (_, ('t', _), _) = True
      t (_, _, ('t', _)) = True
      t _ = False
  print (count t triples)
  let pw = tail . concatMap (\(a, b) -> [',', a, b]) . sort
  putStrLn (pw (maximumBy (comparing length) (cliques neigh computers)))
