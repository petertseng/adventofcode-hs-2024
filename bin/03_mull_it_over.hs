import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOnOne)

import Control.Applicative (liftA2)
import Data.Maybe (mapMaybe)
import Data.List (isPrefixOf, isSuffixOf)
import Text.Read (readMaybe)

data Cmd = Do | Dont | Mul Int

muls :: [Cmd] -> Int
muls [] = 0
muls (Mul n : cs) = n + muls cs
muls (Do    : cs) = muls cs
muls (Dont  : cs) = muls cs

mulEnabled :: [Cmd] -> Int
mulEnabled [] = 0
mulEnabled (Mul n : cs) = n + mulEnabled cs
mulEnabled (Do    : cs) = mulEnabled cs
mulEnabled (Dont  : cs) = mulDisabled cs

mulDisabled :: [Cmd] -> Int
mulDisabled [] = 0
mulDisabled (Mul _ : cs) = mulDisabled cs
mulDisabled (Do    : cs) = mulEnabled cs
mulDisabled (Dont  : cs) = mulDisabled cs

cmds :: String -> [Cmd]
cmds = mapMaybe cmd . parens

cmd :: (String, String) -> Maybe Cmd
cmd (name, "") | "do" `isSuffixOf` name = Just Do
               | "don't" `isSuffixOf` name = Just Dont
cmd (name, args) | "mul" `isSuffixOf` name = fmap Mul prod
  where prod = if length l <= 3 && length r <= 3 then liftA2 (*) (readMaybe l) (readMaybe r) else Nothing
        (l, r) = splitOnOne ',' args
cmd _ = Nothing

-- before left paren, between parens
parens :: String -> [(String, String)]
parens "" = []
parens s = if ")" `isPrefixOf` rest then (name, args) : parens (drop 1 rest) else parens afterOpen
  where (name, afterOpen) = splitOnOne '(' s
        (args, rest) = break (`elem` "()") afterOpen

main :: IO ()
main = do
  s <- readInputFile
  let prog = cmds s
  print (muls prog)
  print (mulEnabled prog)
