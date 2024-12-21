{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

import AdventOfCode (readInputFile)

import Data.Array.IArray (Array, Ix, accumArray, assocs, elems)
import Data.Char (digitToInt)

data Numpad = N0 | N1 | N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9 | NA deriving (Enum, Show)
data Directional = North | South | East | West | Activate deriving (Bounded, Eq, Ix, Ord)

complexity :: Int -> Int -> Int -> Int -> Int
complexity n a b c = (a * 100 + b * 10 + c) * sum (elems (iterate expandDir pairs0 !! n))
  where pairs0 = accumArray (+) 0 (minBound, maxBound) (map (, 1) (pairs (Activate : expandNum [toEnum a, toEnum b, toEnum c, NA])))

expandDir :: Array (Directional, Directional) Int -> Array (Directional, Directional) Int
expandDir freq = accumArray (+) 0 (minBound, maxBound) [((c, d), n) | ((a, b), n) <- assocs freq, (c, d) <- pairs (Activate : dir a b ++ [Activate])]

expandNum :: [Numpad] -> [Directional]
expandNum xs = concat [num a b ++ [Activate] | (a, b) <- zip (NA : xs) xs]

num :: Numpad -> Numpad -> [Directional]
num N0 N2 = [North]
num N0 N8 = [North, North, North]
num N0 NA = [East]

num N1 N2 = [East]
num N1 N7 = [North, North]

num N2 N4 = [West, North]
num N2 N8 = [North, North]
num N2 N9 = [North, North, East]

num N3 N7 = [West, West, North, North]
num N3 N8 = [West, North, North]
num N3 NA = [South]

num N4 N5 = [East]
num N4 N6 = [East, East]

num N5 N0 = [South, South]
num N5 N2 = [South]
num N5 N6 = [East]
num N5 N9 = [North, East]
num N5 NA = [South, South, East]

num N6 NA = [South, South]

num N7 N0 = [East, South, South, South]
num N7 N6 = [South, East, East]
num N7 N9 = [East, East]

num N8 N0 = [South, South, South]
num N8 N5 = [South]
num N8 N6 = [South, East]
num N8 NA = [South, South, South, East]

num N9 N3 = [South, South]
num N9 N8 = [West]
num N9 NA = [South, South, South]

num NA N0 = [West]
num NA N1 = [North, West, West]
num NA N2 = [West, North]
num NA N3 = [North]
num NA N4 = [North, North, West, West]
num NA N5 = [West, North, North]
num NA N9 = [North, North, North]

num a b = error ("unimplemented num " ++ show (a, b))

dir :: Directional -> Directional -> [Directional]
dir North    North    = []
dir North    South    = [South]
dir North    East     = [South, East]
dir North    West     = [South, West]
dir North    Activate = [East]
dir South    North    = [North]
dir South    South    = []
dir South    East     = [East]
dir South    West     = [West]
dir South    Activate = [North, East]
dir East     North    = [West, North]
dir East     South    = [West]
dir East     East     = []
dir East     West     = [West, West]
dir East     Activate = [North]
dir West     North    = [East, North]
dir West     South    = [East]
dir West     East     = [East, East]
dir West     West     = []
dir West     Activate = [East, East, North]
dir Activate North    = [West]
dir Activate South    = [West, South]
dir Activate West     = [South, West, West]
dir Activate East     = [South]
dir Activate Activate = []

pairs :: [a] -> [(a, a)]
pairs xs = zip xs (drop 1 xs)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

main :: IO ()
main = do
  s <- readInputFile
  let codes = map (\case [a, b, c, 'A'] -> (digitToInt a, digitToInt b, digitToInt c); c -> error ("bad " ++ c)) (lines s)
  print (sum (map (uncurry3 (complexity 2)) codes))
  print (sum (map (uncurry3 (complexity 25)) codes))
