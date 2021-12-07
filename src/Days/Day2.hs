module Days.Day2 where

import Control.Monad.State.Lazy

import qualified TestT as T
import qualified Days.Day as D

day = D.Day { D.name = "Day 2"
            , D.part1 = part1
            , D.part2 = part2
            , D.testinput = T.mkAssertion "day2_test.txt" "150" "900"
            , D.input     = T.mkAssertion "day2_actual.txt" "2117664" "2073416724"}

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
