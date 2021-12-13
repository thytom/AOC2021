-- This sucks, but it's easier than parsing which function to call from JSON
{-# LANGUAGE UnicodeSyntax, BangPatterns #-}
module Tests where

import Text.Printf
import System.CPUTime
import Data.Maybe(fromJust)
import Control.Monad(unless)

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
testall tm = testlist tm (runTest)

-- No fancy testing, just sees if it passes
assertall :: [Test] -> IO ()
assertall tm = testlist tm (run)

-- How many times to repeat for an average
repeats = 5

testlist :: [Test] -> (Test -> IO (Bool, Double)) -> IO ()
testlist tm f = do ress <- sequence $ map (f) tm
                   let time = printf "%.3f" (sum $ map (snd) ress)
                   let results = map (fst) ress
                   let count  = length results
                   let passes = length $ filter (==True) results
                   let fails  = length $ filter (==False) results
                   putStrLn . concat $ 
                           [show count, " tests completed in ", time, " milliseconds with ", show passes, " pass"
                           , if passes /= 1 then "es" else "", " and ", show fails, " failure"
                           , if fails /= 1 then "s" else "", 
                           if fails == 0 then colour (bold ++ green) " \\(^.^)/" else colour (bold++red) " (T_T)",
                           "."]

run :: Test -> IO (Bool, Double)
run Test{T.name=day, T.input=file, T.subject=f, T.assert=e} = do
        input <- readFile $ ("inputs/" ++ file)
        let assert = fromJust e
        (res, dur) <- time f input
        unless (assert == res) $ printf "%s %s: Expected %s but got %s\n" (colour bold day) (colour red "failed") assert res
        return $ (assert == res, dur)

runTest :: Test -> IO (Bool, Double)
runTest Test{T.name=day, T.input=file, T.subject=f, T.assert=e} = do 
        input <- readFile $ ("inputs/" ++ file)
        let assert = fromJust e
        (res, diff) <- time f input 
        if assert == res
           then do times_ <- sequence [time f input | _<-[0..repeats]]
                   let times = map snd times_
                   print_passed day (maximum times) (average times) (minimum times) res
                   return (True, average times)

           else do print_failed day res assert
                   return (False, 0)

print_passed :: String -> Double -> Double -> Double -> String -> IO ()
print_passed name worst avg best result =
        printf "%-21s%19s %s: %9.3f ms %9.3f ms %9.3f ms %s: %15s\n"
               (colour bold name) (colour green "passed.") (colour bold "Bst/Avg/Wst") best avg worst
               (colour bold "Result") result


print_failed :: String -> String -> String -> IO ()
print_failed name result expected =
        printf "%-21s%19s %s: %15s (%s)\n"
               (colour bold name) (colour red "failed.") (colour bold "Result") result (colour grey ("Expected " ++ expected))

average :: [Double] -> Double
average ns = sum ns / fromIntegral (length ns)

time :: AOCFunc -> String -> IO (String, Double)
time t input = do start <- getCPUTime
                  let !res = t input
                  end <- getCPUTime
                  return (res, fromIntegral (end-start) / 1000000000)

-- Ansi colour nonsense
reset = "\x1b[0m"
red = "\x1b[31m"
green = "\x1b[32m"
bold = "\x1b[1m"
grey = "\x1b[2m"

colour :: String -> String -> String
colour c s = c ++ s ++ reset
