import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOn)

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (inits, isPrefixOf)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State (State, evalState, get, modify)

prefixSize :: Int
prefixSize = 4

waysToMake :: Map String [String] -> Int -> String -> State (IntMap Int) Int
waysToMake supply i want = do
  cache <- get
  case IntMap.lookup i cache of
    Just prev -> return prev
    Nothing -> do
      let ways w s | w == s = return 1
          ways "" _ = error "never happen"
          ways w s | s `isPrefixOf` w = waysToMake supply (i + length s) (drop (length s) w)
          ways _ _ = return 0
      v <- mapM (ways want) (concat [Map.findWithDefault [] prefix supply | prefix <- take prefixSize (drop 1 (inits want))])
      modify (IntMap.insert i (sum v))
      return (sum v)

groupBy :: Ord k => (a -> k) -> [a] -> Map k [a]
groupBy f = Map.fromListWith (++) . map (\x -> (f x, [x]))

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

main :: IO ()
main = do
  s <- readInputFile
  let (supply, want) = case lines s of
        sup:"":w -> (groupBy (take prefixSize) (map (dropWhile (== ' ')) (splitOn ',' sup)), w)
        _ -> error ("bad " ++ s)
      ways = map (\w -> evalState (waysToMake supply 0 w) IntMap.empty) want
  print (count (> 0) ways)
  print (sum ways)
