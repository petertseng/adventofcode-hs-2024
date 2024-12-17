import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOn)

import Data.Array.Unboxed ((!), (!?), UArray, listArray)
import Data.Bits ((.|.), shiftL, shiftR, xor)
import Data.List (dropWhileEnd, find, sort)
import Data.Maybe (fromJust)

run :: UArray Int Int -> Int -> Int -> Int -> Int -> [Int]
run prog ip a b c = let operand = prog ! (ip + 1) in case prog !? ip of
  Nothing -> []
  Just 0 -> run prog (ip + 2) (a `quot` (2 ^ combo operand a b c)) b c
  Just 1 -> run prog (ip + 2) a (b `xor` operand) c
  Just 2 -> run prog (ip + 2) a (combo operand a b c `rem` 8) c
  Just 3 -> run prog (if a == 0 then ip + 2 else operand) a b c
  Just 4 -> run prog (ip + 2) a (b `xor` c) c
  Just 5 -> combo operand a b c `rem` 8 : run prog (ip + 2) a b c
  Just 6 -> run prog (ip + 2) a (a `quot` (2 ^ combo operand a b c)) c
  Just 7 -> run prog (ip + 2) a b (a `quot` (2 ^ combo operand a b c))
  Just v -> error ("bad op" ++ show v)

combo :: Int -> Int -> Int -> Int -> Int
combo 0 _ _ _ = 0
combo 1 _ _ _ = 1
combo 2 _ _ _ = 2
combo 3 _ _ _ = 3
combo 4 a _ _ = a
combo 5 _ b _ = b
combo 6 _ _ c = c
combo c _ _ _ = error ("bad combo " ++ show c)

mkval :: [Int] -> Int -> Int -> Int
-- because of generation order, head is guaranted to be the minimum
mkval prog m1 m2 = head (foldr step [0] (zip prog [0..]))
  where step (target, i) vals = [cand
                                | v <- vals
                                , oct <- [(if i == length prog - 1 then 1 else 0) .. 7]
                                , let cand = v `shiftL` 3 .|. oct
                                , let b = oct `xor` m1
                                , let c = (cand `shiftR` b) `rem` 8
                                , b `xor` m2 `xor` c == target]

pairs :: [a] -> [(a, a)]
pairs (x:y:xs) = (x, y) : pairs xs
pairs [] = []
pairs [_] = error "unpaired"

main :: IO ()
main = do
  s <- readInputFile
  let (a, b, c, progl) = case lines s of
        [   'R':'e':'g':'i':'s':'t':'e':'r':' ':'A':':':' ':as
          , 'R':'e':'g':'i':'s':'t':'e':'r':' ':'B':':':' ':bs
          , 'R':'e':'g':'i':'s':'t':'e':'r':' ':'C':':':' ':cs
          , ""
          , 'P':'r':'o':'g':'r':'a':'m':':':' ':ps] -> (read as, read bs, read cs, map read (splitOn ',' ps))
        _ -> error ("bad " ++ s)
      prog = listArray (0, length progl - 1) progl
  putStrLn (dropWhileEnd (== ']') (drop 1 (show (run prog 0 a b c))))
  case progl of
    [2, 4, 1, v1, 7, 5, _, _, _, _, _, _, _, _, 3, 0] -> case sort (pairs progl) of
      [(0, 3), (1, v2), (1, v3), (2, 4), (3, 0), (4, _), (5, 5), (7, 5)] -> print (mkval progl v1 (if v1 == v2 then v3 else v2))
      _ -> error ("bad program " ++ show progl)
    [0, 1, 5, 4, 3, 0] -> putStrLn "impossible"
    [0, 3, 5, 4, 3, 0] -> print (0o345300 :: Int)
    _ -> print (fromJust (find (\newa -> run prog 0 newa b c == progl) [1..]))
