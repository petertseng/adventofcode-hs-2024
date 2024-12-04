import AdventOfCode (readInputFile)

import Data.Array.Unboxed ((!?), UArray, assocs, listArray)

type Pos = (Int, Int)
newtype Dir = Dir (Int, Int) deriving (Eq)

step :: Pos -> Dir -> Int -> Pos
step (y, x) (Dir (dy, dx)) n = (y + dy * n, x + dx * n)

diag :: [Dir]
diag = [Dir (-1, -1), Dir (-1, 1), Dir (1, -1), Dir (1, 1)]

dirs8 :: [Dir]
dirs8 = diag ++ [Dir (-1, 0), Dir (0, -1), Dir (0, 1), Dir (1, 0)]

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
  let grid = posArr (lines s)
      coords c = map fst (filter ((== c) . snd) (assocs grid))
      goodx = [(x, d) | x <- coords 'X', d <- dirs8]
      goodm = [(x, d) | (x, d) <- goodx, grid !? step x d 1 == Just 'M']
      gooda = [(x, d) | (x, d) <- goodm, grid !? step x d 2 == Just 'A']
      goods = [(x, d) | (x, d) <- gooda, grid !? step x d 3 == Just 'S']
  print (length goods)

  let xmas2 a = count (mas2 a) diag == 2
      mas2 a d = grid !? step a d 1 == Just 'M' && grid !? step a d (-1) == Just 'S'
  print (count xmas2 (coords 'A'))
