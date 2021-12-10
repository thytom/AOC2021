module Days.Day10 where

import qualified TestT as T
import qualified Days.Day as D

import Data.Maybe (fromJust)
import Data.List (sort)

day = D.Day { D.name = "Day 10"
            , D.part1 = part1
            , D.part2 = part2
            , D.testinput = T.mkAssertion "day10_test.txt"   "26397" "288957"
            , D.input     = T.mkAssertion "day10_actual.txt" "345441" "3235371166"}

pointstable = [ (')',     3)
              , (']',    57)
              , ('}',  1197)
              , ('>', 25137)
              ]

opentoclose '(' = ')'
opentoclose '{' = '}'
opentoclose '[' = ']'
opentoclose '<' = '>'

is_opening = (`elem` "([{<")

is_closing = (`elem` ")]}>")

type Stack = String

-- Return the char that the string fails on, if any
find_error :: String -> Stack -> Int -> Maybe (Char, Int, Stack)
find_error [] _ _ = Nothing
find_error (s:ss) [] n 
  | is_opening s = find_error ss [s] (n+1)
  | is_closing s = Just (s, n, [])
find_error (s:ss) (x:st) n
  | is_opening s = find_error ss (s:x:st) (n+1)
  | is_closing s = if s == opentoclose x then find_error ss (st) (n+1)
                                         else Just (s, n, (x:st))
  | otherwise = error $ "Illegal character at " ++ show n

part1 :: String -> String
part1 input = show . sum . map (fromJust . (\(x, _, _) -> lookup x pointstable) . fromJust) . filter (/=Nothing) . map (\s -> find_error s [] 0) $ lines input

pointstable' = [ (')', 1)
              , (']', 2)
              , ('}', 3)
              , ('>', 4)
              ]

autoclose :: Stack -> String -> String
autoclose st [] = map (opentoclose) st
autoclose [] (s:ss)
  | is_opening s = autoclose [s] ss
  | is_closing s = error "Bad String."
autoclose (x:st) (s:ss)
  | is_opening s = autoclose (s:x:st) ss
  | is_closing s = if s == opentoclose x 
                      then autoclose st ss
                      else error "Bad String."

getscore :: Int -> String  -> Int
getscore n [] = n
getscore n (s:ss) = getscore ((5 * n) + (fromJust $ lookup s pointstable')) ss

part2 :: String -> String
part2 input = show . (\l -> head $ drop (length l `div` 2) l). sort . map (getscore 0 . autoclose []) $ incompletes
        where incompletes = filter (\s -> find_error s [] 0 == Nothing)$ lines input
