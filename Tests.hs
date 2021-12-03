-- This sucks, but it's easier than parsing which function to call from JSON
module Tests where

import Control.Monad(when)
import Data.Maybe(fromJust)
import TestT

import Day1.Day1 as Day1
import Day2.Day2 as Day2
import Day3.Day3 as Day3

type TestAssert = (String, String, AOCFunc, ExpectedValue)
type AOCFunc = (String -> String)
type ExpectedValue = Maybe String

testall :: [Test] -> IO ()
testall tm = do putStrLn "Running all tests..."
                results <- sequence $ map (runTest) tm
                let count  = length results
                let passes = length $ filter (==True) results
                let fails  = length $ filter (==False) results
                putStrLn . concat $ 
                        ["\n", show count, " tests completed with ", show passes, " pass"
                        , if passes /= 1 then "es" else "", " and ", show fails, " failure"
                        , if fails /= 1 then "s." else "."]

runTest :: Test -> IO Bool
runTest Test{name=day, input=file, subject=f, assert=e} = do 
        input <- readFile $ ("inputs/" ++ file)
        -- Run the function
        let res = f input
        case e of 
          Nothing -> do putStrLn $ "Untested Result of " ++ day ++ ": " ++ res 
                        return True
          Just s  -> do putStr $ day ++ if s == res then "\tpassed." else " failed."
                        when (s == res) . putStr $ " Result: " ++ res
                        putStrLn ""
                        when (s /= res) . putStrLn .concat $ ["Expected ", show $ fromJust e, " but got ", show res, "."]
                        return (s == res)
