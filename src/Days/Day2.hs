module Days.Day2 where

import Control.Monad.State.Lazy

import TestT

tests = [ Test {name="Day 2 Part 1", input="day2_test.txt"  , subject=part1, assert=Just "150"}
        , Test {name="Day 2 Part 2", input="day2_test.txt"  , subject=part2, assert=Just "900"} ]
run   = [ Test {name="Day 2 Part 1", input="day2_actual.txt", subject=part1, assert=Just "2117664"}
        , Test {name="Day 2 Part 2", input="day2_actual.txt", subject=part2, assert=Just "2073416724"} ]
all = tests ++ run

parse :: String -> ([Int], [Int])
parse = (\x-> p x ([],[])) . lines

p :: [String] -> ([Int], [Int]) -> ([Int], [Int])
p [] ys = ys
p (x:xs) (yas, ybs) = case a of
             "forward"  -> p xs (b:yas, ybs) 
             "up"       -> p xs (yas, -b:ybs) 
             "down"     -> p xs (yas, b:ybs) 
        where (a, b) = (head $ words x, (\x-> read x :: Int) $ last $ words x)

tupletoArr :: (a, a) -> [a]
tupletoArr (x, y) = [x, y]

part1 :: String -> String
part1 =  show . foldl (*) 1 . map (foldl (+) 0) . tupletoArr . parse

part2 :: String -> String
part2 = show . (\(l, r) -> l*r) . snd . snd . 
        (flip runState) (0, (0, 0)) . sequence . map p2 . lines

type SubmarineState = (Int, (Int, Int))

p2 :: String -> State SubmarineState ()
p2 x = do (aim, (horiz, depth)) <- get 
          let (dir, val) = (\x -> (head x , (read (last x) :: Int))) $ words x
          case dir of 
                  "forward" -> put (aim, (horiz+val, depth+(aim*val)))
                  "up"      -> put (aim-val, (horiz, depth))
                  "down"    -> put (aim+val, (horiz, depth))
