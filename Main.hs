import Day1.Day1 as Day1
import Day2.Day2 as Day2
--import Day3.Day3 as Day3

type TestAssert = (String, String, AOCFunc, ExpectedValue)
type AOCFunc = (String -> String)
type ExpectedValue = Maybe String

testMap = [ 
            ("Day 1 Part 1 Test"  , "inputs/day1_test.txt"  , Day1.part1, Just "7")
          , ("Day 1 Part 1 Actual", "inputs/day1_actual.txt", Day1.part1, Just "1681")
          , ("Day 1 Part 2 Test"  , "inputs/day1_test.txt"  , Day1.part2, Just "5")
          , ("Day 1 Part 2 Actual", "inputs/day1_actual.txt", Day1.part2, Just "1704")

          , ("Day 2 Part 1 Test"  , "inputs/day2_test.txt"  , Day2.part1, Just "150")
          , ("Day 2 Part 1 Actual", "inputs/day2_actual.txt", Day2.part1, Just "2117664")
          , ("Day 2 Part 2 Test"  , "inputs/day2_test.txt"  , Day2.part2, Just "900")
          , ("Day 2 Part 2 Actual", "inputs/day2_actual.txt", Day2.part2, Just "2073416724")

          -- , ("Day 3 Part 1 Test"  , "inputs/day3_test.txt"  , Day2.part1, Nothing)
          -- , ("Day 3 Part 1 Actual", "inputs/day3_actual.txt", Day2.part1, Nothing)
          -- , ("Day 3 Part 2 Test"  , "inputs/day3_test.txt"  , Day2.part2, Nothing)
          -- , ("Day 3 Part 2 Actual", "inputs/day3_actual.txt", Day2.part2, Nothing)
          ]

main :: IO ()
main = do test

runLast :: IO ()
runLast = do _ <- runTest $ last testmap
             return ()

test :: IO ()
test = do results <- sequence $ map (runTest) testMap 
          let count  = length results
          let passes = length $ filter (==True) results
          let fails  = length $ filter (==False) results
          putStrLn $ "\n" ++ show count ++ " tests completed with " 
           ++ show passes ++ " passes and " ++ show fails ++ " failures."

-- Read the input, get an output
runTest :: TestAssert -> IO Bool
runTest (day, file, f, e) = do 
        input <- readFile $ file
        -- Run the function
        let res = f input
        case e of 
          Nothing -> do putStrLn $ "Untested Result of " ++ day ++ ": " ++ res 
                        return True
          Just s  -> case res==s of 
                       True  -> do putStrLn $ day ++ " passed."
                                   return True
                       False -> do putStrLn $ day ++ " failed."
                                   return False
