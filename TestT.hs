module TestT where

data Test = Test { name      :: String
                 , input     :: String
                 , subject   :: (String -> String)
                 , assert    :: Maybe String
                 }
