module Day where

data Assertion = Assertion {filename      :: String
                           , part1_assert :: String
                           , part2_assert :: String}

data Day = Day { name              :: String
               , part1             :: (String -> String)
               , part2             :: (String -> String)
               , testinput         :: Assertion
               , input             :: Assertion
               }
