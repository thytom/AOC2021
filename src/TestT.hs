module TestT where

data Test = Test { name      :: String
                 , input     :: String
                 , subject   :: (String -> String)
                 , assert    :: Maybe String
                 }
                 -- deriving (Show)

-- instance Show (a->b) where
--         show f = "<Function>"

data Assertion = Assertion {filename      :: String
                           , part1_assert :: String
                           , part2_assert :: String}

mkAssertion :: String -> String -> String -> Assertion
mkAssertion fn p1 p2 = Assertion {filename=fn , part1_assert=p1, part2_assert=p2}
