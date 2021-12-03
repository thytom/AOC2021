-- This sucks, but it's easier than parsing which function to call from JSON
{-# LANGUAGE UnicodeSyntax #-}
module Tests where

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
                        , if fails /= 1 then "s" else "", 
                        if fails == 0 then colour (bold ++ green) " \\(^.^)/" else colour (bold++red) " (T_T)",
                        "."]

runTest :: Test -> IO Bool
runTest Test{name=day, input=file, subject=f, assert=e} = do 
        input <- readFile $ ("inputs/" ++ file)
        -- Run the function
        let res = f input
        case e of 
          Nothing -> do putStrLn $ "Untested Result of " ++ day ++ ": " ++ res 
                        return True
          Just s  -> do putStr $ day ++ "\t" ++ if s == res then colour green "passed." else colour red "failed."
                        if s == res 
                           then putStrLn $ " Result: " ++ res
                           else putStrLn . colour grey . concat $ [" Expected ", show $ fromJust e, " but got ", show res, "."]
                        return (s == res)

-- Ansi colour nonsense
reset = "\x1b[0m"
red = "\x1b[31m"
green = "\x1b[32m"
bold = "\x1b[1m"
grey = "\x1b[2m"

colour :: String -> String -> String
colour c s = c ++ s ++ reset
