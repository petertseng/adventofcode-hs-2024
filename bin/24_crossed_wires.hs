import AdventOfCode (readInputFile)
import AdventOfCode.Split (splitOnOne)

import Data.Bits (xor)
import Data.List (isPrefixOf, mapAccumL, sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Text.Printf (printf)

type Circuit = Map String (Op, String, String)
type Vals = Map String Bool
data Op = And | Or | Xor deriving (Eq, Ord, Show)

runCircuit :: Circuit -> Vals -> Vals
runCircuit circ inputs = outputs
  where outputs = Map.fromList [(k, resolve k) | k <- Map.keys circ]
        resolve k = fromMaybe (gate k) (Map.lookup k inputs)
        gate k = let (o, a, b) = circ Map.! k in apply o (resolve a) (resolve b)

zval :: Vals -> Int
zval vals = foldr (\z acc -> acc * 2 + if vals Map.! z then 1 else 0) 0 zs
  where zs = filter ("z" `isPrefixOf`) (Map.keys vals)

apply :: Op -> Bool -> Bool -> Bool
apply And = (&&)
apply Or = (||)
apply Xor = xor

op :: String -> Op
op "AND" = And
op "OR" = Or
op "XOR" = Xor
op s = error ("bad op " ++ s)

oneOrZero :: String -> Bool
oneOrZero "0" = False
oneOrZero "1" = True
oneOrZero s = error ("bad bit " ++ s)

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

main :: IO ()
main = do
  s <- readInputFile
  let (insStr, circStr) = splitOnOne "" (lines s)
      ins = Map.fromList [(k, oneOrZero (dropWhile (== ' ') v)) | (k, v) <- map (splitOnOne ':') insStr]
      circ = Map.fromList [(dest, (op ops, a, b)) | [a, ops, b, "->", dest] <- map words circStr]
      outs = runCircuit circ ins
  print (zval outs)

  let revCirc2 = Map.fromList (concat [[((o, a, b), dest), ((o, b, a), dest)] | (dest, (o, a, b)) <- Map.assocs circ])
      revCirc1 = Map.fromList (concat [[((o, a), (dest, b)), ((o, b), (dest, a))] | (dest, (o, a, b)) <- Map.assocs circ])
      fixBit cin i = (cout, swap1 ++ swap2)
        where xAndY0 = revCirc2 Map.! (And, x, y)
              xXorY = revCirc2 Map.! (Xor, x, y)
              (zop, _, _) = circ Map.! z
              (cinAndXXorY, swap1, xAndY) = case Map.lookup (And, cin, xAndY0) revCirc2 of
                Just c -> (c, [xAndY0, xXorY], xXorY)
                Nothing -> (revCirc2 Map.! (And, cin, xXorY), [], xAndY0)
              (cout, swap2) = case zop of
                Or -> let co = revCirc2 Map.! (Xor, cin, xXorY) in (co, [co, z])
                And | xAndY == z -> let (co, swp) = revCirc1 Map.! (Or, cinAndXXorY) in (co, [swp, z])
                And -> let (co, swp) = revCirc1 Map.! (Or, xAndY) in (co, [swp, z])
                Xor -> (revCirc2 Map.! (Or, xAndY, cinAndXXorY), [])
              x = printf "x%02d" i
              y = printf "y%02d" i
              z = printf "z%02d" i
  case (Map.lookup (And, "x00", "y00") revCirc2, Map.lookup (Xor, "x00", "y00") revCirc2) of
    (Just xAndY, Just xXorY) -> do
      let width = count ("z" `isPrefixOf`) (Map.keys circ)
          (cin0, swaps0) = case Map.lookup "z00" circ of
            Just (Xor, "x00", "y00") -> (xAndY, [])
            Just (Xor, "y00", "x00") -> (xAndY, [])
            Just (And, "x00", "y00") -> (xXorY, [xAndY, xXorY])
            Just (And, "y00", "x00") -> (xXorY, [xAndY, xXorY])
            z0 -> error ("bad " ++ show z0)
          (_, swaps) = mapAccumL fixBit cin0 [1 .. width - 2]
      putStrLn (drop 1 (concatMap (',' :) (sort (swaps0 ++ concat swaps))))
    (_, _) -> return ()
