import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOn)

import Data.List (dropWhileEnd)
import Data.Maybe (mapMaybe)
import Data.Ratio (denominator, numerator)

type Claw = (Int, Int, Int, Int, Int, Int)

tokens :: Claw -> Maybe Integer
tokens (ax, ay, bx, by, px, py) = if denominator a == 1 && denominator b == 1 && a > 0 && b > 0 then Just (3 * numerator a + numerator b) else Nothing
  where b = (fromIntegral py - fromIntegral ay * fromIntegral px / fromIntegral ax) / (fromIntegral by - fromIntegral ay * fromIntegral bx / fromIntegral ax) :: Rational
        a = (fromIntegral px - b * fromIntegral bx) / fromIntegral ax

big :: Claw -> Claw
big (ax, ay, bx, by, px, py) = (ax, ay, bx, by, px + n, py + n)
  where n = 10000000000000

claw :: [String] -> Claw
claw s = case map words s of
  [ ["Button", "A:", 'X':'+':ax, 'Y':'+':ay]
   ,["Button", "B:", 'X':'+':bx, 'Y':'+':by]
   ,["Prize:", 'X':'=':px, 'Y':'=':py]] -> (read (uncomma ax), read ay, read (uncomma bx), read by, read (uncomma px), read py)
    where uncomma = dropWhileEnd (== ',')
  _ -> error ("bad claw " ++ unlines s)

main :: IO ()
main = do
  s <- readInputFile
  let claws = map claw (splitOn "" (lines s))
  print (sum (mapMaybe tokens claws))
  print (sum (mapMaybe (tokens . big) claws))
