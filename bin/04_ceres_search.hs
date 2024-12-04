import AdventOfCode (readInputFile)

import qualified Data.Set as Set

type Pos = (Int, Int)
newtype Dir = Dir (Int, Int) deriving (Eq)

step :: Pos -> Dir -> Int -> Pos
step (y, x) (Dir (dy, dx)) n = (y + dy * n, x + dx * n)

diag :: [Dir]
diag = [Dir (-1, -1), Dir (-1, 1), Dir (1, -1), Dir (1, 1)]

dirs8 :: [Dir]
dirs8 = diag ++ [Dir (-1, 0), Dir (0, -1), Dir (0, 1), Dir (1, 0)]

enumGrid :: [[a]] -> [(Pos, a)]
enumGrid = concat . zipWith enumRow [0..]
  where enumRow y = zipWith (\x cell -> ((y, x), cell)) [0..]

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

main :: IO ()
main = do
  s <- readInputFile
  let grid = enumGrid (lines s)
      coords c = map fst (filter ((== c) . snd) grid)
      coordSet = Set.fromAscList . coords
      ms = coordSet 'M'
      as = coordSet 'A'
      ss = coordSet 'S'
      goodx = [(x, d) | x <- coords 'X', d <- dirs8]
      goodm = [(x, d) | (x, d) <- goodx, step x d 1 `Set.member` ms]
      gooda = [(x, d) | (x, d) <- goodm, step x d 2 `Set.member` as]
      goods = [(x, d) | (x, d) <- gooda, step x d 3 `Set.member` ss]
  print (length goods)

  let xmas2 a = count (mas2 a) diag == 2
      mas2 a d = step a d 1 `Set.member` ms && step a d (-1) `Set.member` ss
  print (count xmas2 (coords 'A'))
