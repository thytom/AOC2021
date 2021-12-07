module Days.Day where

import qualified TestT as T

data Day = Day { name              :: String
               , part1             :: (String -> String)
               , part2             :: (String -> String)
               , testinput         :: T.Assertion
               , input             :: T.Assertion
               }
