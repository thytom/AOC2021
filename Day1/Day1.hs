module Day1.Day1 where

part1 :: String -> String
part1 = p1 . parse

part2 :: String -> String
part2 = p1 . (flip sumtrip) [] . parse

p1 :: [Int] -> String
p1 x = show $ length $ filter (>0) $ zipWith (-) (tail x) x

parse :: String -> [Int]
parse = map (\x -> read x :: Int) . lines

sumtrip :: [Int] -> [Int] -> [Int]
sumtrip (b:c:[])   ys = reverse ys
sumtrip (a:b:c:xs) ys = sumtrip (b:c:xs) (a+b+c:ys)
