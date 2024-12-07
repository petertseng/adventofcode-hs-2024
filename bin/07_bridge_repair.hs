import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOnOne)

import Control.Arrow ((***), second)
import Data.List (partition)

canMake :: Bool -> (Int, [Int]) -> Bool
canMake canConcat (targ0, v) = cm targ0 v
  where cm _ [] = False
        cm targ [x] = x == targ
        cm targ (x:xs) = canConcat && targ `rem` p10 == x && cm (targ `quot` p10) xs || targ `rem` x == 0 && cm (targ `quot` x) xs || targ >= x && cm (targ - x) xs
          where p10 = 10 ^ length (show x)

main :: IO ()
main = do
  s <- readInputFile
  let calibrations = map ((read *** (map read . words)) . splitOnOne ':') (lines s)
      revCalib = map (second reverse) calibrations
      (good, bad) = partition (canMake False) revCalib
      ans1 = sum (map fst good)
  print ans1
  print (ans1 + sum (map fst (filter (canMake True) bad)))
