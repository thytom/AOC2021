-- This sucks, but it's easier than parsing which function to call from JSON
{-# LANGUAGE UnicodeSyntax, BangPatterns #-}
module Tests where

import Text.Printf
import System.CPUTime
import Data.Maybe(fromJust)
import TestT

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
        start <- getCPUTime
        let !res = f input
        end <- getCPUTime
        let diff = fromIntegral (end - start) / (1000000000)
        case e of 
          Nothing -> do printf "%-28s (Untested)\tResult(%0.3f ms):\t%s\n" (colour bold day) (diff :: Double) res
                        return True
          Just s  -> do printf "%-28s%s" (colour bold day) $ if s == res then colour green "passed." else colour red "failed."
                        if s == res 
                           then printf " %s: %9.3f ms %s: %10s \n" (colour bold "Time") (diff :: Double) (colour bold "Result") res
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
