{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module AdventOfCode.Search (
  astar,
  astarInt,
  bfs,
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.List (foldl')
import Data.Set (Set)
import qualified Data.Set as Set

bfs :: Ord a => (a -> [a]) -> (a -> Bool) -> a -> [Either Int (Int, a)]
bfs neigh goal start = bfs' 0 Set.empty (Set.singleton start)
  where bfs' gen _ s | Set.null s = [Left (gen - 1)]
        bfs' gen seen front = map (Right . (gen,)) goals ++ bfs' (gen + 1) seen' front'
          where goals = filter goal (Set.toList front)
                seen' = seen `Set.union` front
                front' = Set.fromList (filter (`Set.notMember` seen') (concatMap neigh front))

astar :: forall a. Ord a => (a -> [(Int, a)]) -> (a -> Int) -> (a -> Bool) -> a -> Maybe (Int, [[a]])
astar neigh heur goal start = astar' Set.empty Map.empty (pqSingleton (heur start) (0, start))
  where astar' known prev tentative = pqPop tentative >>= (\((dist, cur), tent) -> expand known prev dist cur tent)
        expand :: Set a -> Map a (Int, [a]) -> Int -> a -> PQ (Int, a) -> Maybe (Int, [[a]])
        expand _ prev dist current _ | goal current = Just (dist, paths prev current)
        expand known prev _ current tentative | current `Set.member` known = astar' known prev tentative
        expand known prev dist current tentative = astar' (Set.insert current known) prev' tentative'
          where tentative' = pqUnion tentative (map estimateDist news)
                news = filter unknown (neigh current)
                prev' = foldl' bestDist prev news
                bestDist m (stepDist, n) = case Map.lookup n m of
                  Nothing -> Map.insert n (dist + stepDist, [current]) m
                  Just (d, _) | d > dist + stepDist -> Map.insert n (dist + stepDist, [current]) m
                  Just (d, ns) | d == dist + stepDist -> Map.insert n (d, current : ns) m
                  Just (_, _) -> m
                unknown (_, n) = n `Set.notMember` known
                estimateDist (stepDist, n) = (dist + stepDist + heur n, (dist + stepDist, n))
        paths :: Map a (Int, [a]) -> a -> [[a]]
        paths prev n = case Map.lookup n prev of
          Nothing -> [[n]]
          Just (_, ns) -> concatMap (map (n:) . paths prev) ns

astarInt :: forall a. (a -> Int) -> (a -> [(Int, a)]) -> (a -> Int) -> (a -> Bool) -> a -> Maybe (Int, [[a]])
astarInt compress neigh heur goal start = astar' IntSet.empty IntMap.empty (pqSingleton (heur start) (0, start))
  where astar' known prev tentative = pqPop tentative >>= (\((dist, cur), tent) -> expand known prev dist cur tent)
        expand :: IntSet -> IntMap (Int, [a]) -> Int -> a -> PQ (Int, a) -> Maybe (Int, [[a]])
        expand _ prev dist current _ | goal current = Just (dist, paths prev current)
        expand known prev _ current tentative | compress current `IntSet.member` known = astar' known prev tentative
        expand known prev dist current tentative = astar' (IntSet.insert (compress current) known) prev' tentative'
          where tentative' = pqUnion tentative (map estimateDist news)
                news = filter unknown (neigh current)
                prev' = foldl' bestDist prev news
                bestDist m (stepDist, n) = case IntMap.lookup (compress n) m of
                  Nothing -> IntMap.insert (compress n) (dist + stepDist, [current]) m
                  Just (d, _) | d > dist + stepDist -> IntMap.insert (compress n) (dist + stepDist, [current]) m
                  Just (d, ns) | d == dist + stepDist -> IntMap.insert (compress n) (d, current : ns) m
                  Just (_, _) -> m
                unknown (_, n) = compress n `IntSet.notMember` known
                estimateDist (stepDist, n) = (dist + stepDist + heur n, (dist + stepDist, n))
        paths :: IntMap (Int, [a]) -> a -> [[a]]
        paths prev n = case IntMap.lookup (compress n) prev of
          Nothing -> [[n]]
          Just (_, ns) -> concatMap (map (n:) . paths prev) ns

type PQ a = (Int, [a], IntMap [a])

pqSingleton :: Int -> a -> PQ a
pqSingleton pri x = (pri, [x], IntMap.empty)

pqInsert :: Int -> a -> PQ a -> PQ a
pqInsert pri _ (curPri, _, _) | pri < curPri = error ("non-monotonic add " ++ show pri ++ " vs " ++ show curPri)
pqInsert pri x (curPri, xs, qs) | pri == curPri = (curPri, x:xs, qs)
pqInsert pri x (curPri, xs, qs) = (curPri, xs, IntMap.alter (maybe (Just [x]) (Just . (x:))) pri qs)

pqUnion :: PQ a -> [(Int, a)] -> PQ a
pqUnion = foldl' (\pq' (pri, x) -> pqInsert pri x pq')

pqPop :: PQ a -> Maybe (a, PQ a)
pqPop (pri, x:xs, qs) = Just (x, (pri, xs, qs))
pqPop (_, [], qs) = case IntMap.minViewWithKey qs of
  Nothing -> Nothing
  Just ((pri', []), _) -> error ("bad queue " ++ show pri')
  Just ((pri', x:xs), qs') -> Just (x, (pri', xs, qs'))
