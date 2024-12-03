import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOnOne)

import Control.Applicative (liftA2)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

muls :: String -> Int
muls "" = 0
muls ('m':'u':'l':'(':s) = mul args + muls s
  where (args, _) = splitOnOne ')' s
muls (_:s) = muls s

mul :: String -> Int
mul s = if length a <= 3 && length b <= 3 then fromMaybe 0 (liftA2 (*) (readMaybe a) (readMaybe b)) else 0
  where (a, b) = splitOnOne ',' s

mulEnabled :: String -> Int
mulEnabled "" = 0
mulEnabled ('d':'o':'n':'\'':'t':'(':')':s) = mulDisabled s
mulEnabled ('m':'u':'l':'(':s) = mul args + mulEnabled s
  where (args, _) = splitOnOne ')' s
mulEnabled (_:s) = mulEnabled s

mulDisabled :: String -> Int
mulDisabled "" = 0
mulDisabled ('d':'o':'(':')':s) = mulEnabled s
mulDisabled (_:s) = mulDisabled s

main :: IO ()
main = do
  s <- readInputFile
  print (muls s)
  print (mulEnabled s)
