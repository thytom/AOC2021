-- This sucks, but it's easier than parsing which function to call from JSON
{-# LANGUAGE UnicodeSyntax, BangPatterns #-}
module Tests where

import Text.Printf
import System.CPUTime
import Data.Maybe(fromJust)

import TestT as T
import Days.Day as D

type TestAssert = (String, String, AOCFunc, ExpectedValue)
type AOCFunc = (String -> String)
type ExpectedValue = Maybe String

-- Generate tests from a given day.
testsFrom :: Day -> [Test]
testsFrom Day {D.name=name, D.part1=part1, D.part2=part2, D.testinput=testinput, D.input=input} 
  = concat [mkTests name (part1, part2) assert | assert<-[testinput, input]]

mkTests :: String -> (AOCFunc, AOCFunc) -> Assertion -> [Test]
mkTests name (p1, p2) Assertion{filename=fn, part1_assert=p1a, part2_assert=p2a}
  = [Test{T.name=name ++ " " ++ n2, T.input=fn, subject=fnc, assert=Just assert} 
  | (fnc, assert, n2) <- [(p1, p1a, "Part 1"), (p2, p2a, "Part 2")]]

testall :: [Test] -> IO ()
testall tm = do start <- getCPUTime
                results <- sequence $ map (runTest) tm
                end <- getCPUTime
                let time = printf "%.3f seconds"  (fromIntegral (end-start) / (10^12) :: Double) :: String
                let count  = length results
                let passes = length $ filter (==True) results
                let fails  = length $ filter (==False) results
                putStrLn . concat $ 
                        ["\n", show count, " tests completed in ", time, " with ", show passes, " pass"
                        , if passes /= 1 then "es" else "", " and ", show fails, " failure"
                        , if fails /= 1 then "s" else "", 
                        if fails == 0 then colour (bold ++ green) " \\(^.^)/" else colour (bold++red) " (T_T)",
                        "."]

runTest :: Test -> IO Bool
runTest Test{T.name=day, T.input=file, T.subject=f, T.assert=e} = do 
        input <- readFile $ ("inputs/" ++ file)
        -- Run the function
        start <- getCPUTime
        let !res = f input
        end <- getCPUTime
        let diff = fromIntegral (end - start) / (1000000000)
        let profile = printf "%s: %15s %s: %9.3f ms %s: %15s" (colour bold "Input") file (colour bold "Time") (diff :: Double) (colour bold "Result") res :: String
        case e of 
          Nothing -> do printf "%-20s%18s %s" (colour bold day) (colour grey "untested.") profile
                        putStr "\n"
                        return True
          Just s  -> do printf "%-20s%19s " (colour bold day) $ if s == res then colour green "passed." else colour red "failed."
                        putStr profile
                        if s == res 
                           then putStr "\n"
                           else putStr $ (colour grey $ " Expected: " ++ show s) ++ "\n"
                        return (s == res)

-- Ansi colour nonsense
reset = "\x1b[0m"
red = "\x1b[31m"
green = "\x1b[32m"
bold = "\x1b[1m"
grey = "\x1b[2m"

colour :: String -> String -> String
colour c s = c ++ s ++ reset
