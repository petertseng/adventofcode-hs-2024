import AdventOfCode (readInputFile)

import Data.Array (accumArray, assocs, elems)
import Data.Bits (shiftL, shiftR, xor)

nextSecret :: Int -> Int
nextSecret s = (s2 `xor` (s2 `shiftL` 11)) `rem` 16777216
  where s1 = (s `xor` (s `shiftL` 6)) `rem` 16777216
        s2 = s1 `xor` (s1 `shiftR` 5)

maxWindow :: Int
maxWindow = 18 * (19 * 19 * 19 + 19 * 19 + 19 + 1)

main :: IO ()
main = do
  s <- readInputFile
  let monkeys = map read (lines s)
  print (sum (map ((!! 2000) . iterate nextSecret) monkeys))
  let prices = map (map (`rem` 10) . take 2001 . iterate nextSecret) monkeys
      diffs xs = zipWith (-) xs (drop 1 xs)
      quads = scanl (\acc x -> ((x + 9) * 19 * 19 * 19) + (acc `quot` 19)) 0
      bananas xs = drop 4 (zip (quads (diffs xs)) xs)
      bestBananas = accumArray (\a b -> if a < 0 then b else a) (-1) (0, maxWindow) . bananas
      bestBananases = map bestBananas prices
      bananasForWindow = accumArray (+) 0 (0, maxWindow) [(win, banan) | bests <- bestBananases, (win, banan) <- assocs bests, banan > 0]
  print (maximum (elems bananasForWindow))
