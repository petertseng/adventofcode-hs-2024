import AdventOfCode (readInputFile)

import Data.Char (digitToInt)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (dropWhileEnd, find, scanl')
import Data.Maybe (mapMaybe)

move1 :: [(Maybe Int, Int, Int)] -> Int
move1 x = move x (reverse x)
  where move [] _ = error "left shouldn't be empty"
        move _ [] = error "right shouldn't be empty"
        move ls ((Nothing, _, _):rs) = move ls rs
        move ((Nothing, szl, loff):ls)    ((Just ir, szr, roff):rs) = if roff < loff then 0 else checksum ir (min szr szl) loff + move ls' rs'
          where ls' = if szl <= szr then ls else (Nothing, szl - szr, loff + szr) : ls
                rs' = if szr <= szl then rs else (Just ir, szr - szl, roff) : rs
        move ((Just l,  szl, off):ls)  rs@((Just r, szr,     _):_) | l == r = checksum l szr off -- szr not szl: this file could have moved before
                                                                   | l > r = 0
                                                                   | otherwise = checksum l szl off + move ls rs

move2 :: (Int, Int, Int) -> (IntMap Int, Int) -> (IntMap Int, Int)
move2 (fid, sz, off) (frees, acc) = (frees', acc + checksum fid sz off')
  where (off', frees') = case find ((>= sz) . snd) (IntMap.assocs frees) of
          Just (freeoff, freesz) | freeoff < off -> (freeoff, if freesz' == 0 then without else IntMap.insert freeoff' freesz' without)
            where without = IntMap.delete freeoff frees
                  freesz' = freesz - sz
                  freeoff' = freeoff + sz
          _ -> (off, frees)

checksum :: Int -> Int -> Int -> Int
-- can't tell whether this makes any difference in runtime
--checksum fid sz off = fid * sum [off .. (off + sz - 1)]
checksum fid sz off = fid * (2 * off + sz - 1) * sz `div` 2

interleave :: [a] -> [a] -> [a]
interleave (x:xs) ys = x : interleave ys xs
interleave [] _ = []

main :: IO ()
main = do
  s <- readInputFile
  let sizes = map digitToInt (dropWhileEnd (== '\n') s)
      withId = zip (interleave (map Just [0..]) (repeat Nothing)) sizes
      noZero = filter ((/= 0) . snd) withId
      offsets = scanl' (+) 0 . map snd
      withOffsets = zipWith (\(a, b) c -> (a, b, c)) noZero (offsets noZero)
  print (move1 withOffsets)
  let freesOnly (Nothing, sz, off) = Just (off, sz)
      freesOnly (Just _, _, _) = Nothing
      frees = IntMap.fromAscList (mapMaybe freesOnly withOffsets)
      filesOnly (Nothing, _, _) = Nothing
      filesOnly (Just fid, sz, off) = Just (fid, sz, off)
      files = mapMaybe filesOnly withOffsets
  print (snd (foldr move2 (frees, 0) files))
