module Day2.Day2 where

import Control.Monad.State.Lazy

parse :: String -> ([Int], [Int])
parse = (\x-> p x ([],[])) . lines

p :: [String] -> ([Int], [Int]) -> ([Int], [Int])
p [] ys = ys
p (x:xs) (yas, ybs) = case a of
             "forward"  -> p xs (b:yas, ybs) 
             "up"       -> p xs (yas, -b:ybs) 
             "down"     -> p xs (yas, b:ybs) 
        where (a, b) = (head $ words x, (\x-> read x :: Int) $ last $ words x)

tupletoArr :: (a, a) -> [a]
tupletoArr (x, y) = [x, y]

part1 :: String -> String
part1 =  show . foldl (*) 1 . map (foldl (+) 0) . tupletoArr . parse

part2 :: String -> String
part2 = show . (\(l, r) -> l*r) . snd . snd . 
        (flip runState) (0, (0, 0)) . sequence . map p2 . lines

type SubmarineState = (Int, (Int, Int))

p2 :: String -> State SubmarineState ()
p2 x = do (aim, (horiz, depth)) <- get 
          let (dir, val) = (\x -> (head x , (read (last x) :: Int))) $ words x
          case dir of 
                  "forward" -> put (aim, (horiz+val, depth+(aim*val)))
                  "up"      -> put (aim-val, (horiz, depth))
                  "down"    -> put (aim+val, (horiz, depth))

-- p2 :: [String] -> State SubmarineState Int
-- p2 [] = do (_, (horiz, depth)) <- get
--            return (horiz * depth)
-- p2 (x:xs) = do (aim, (horiz, depth)) <- get
--                let (dir, val) = (\x -> (head x , (read (last x) :: Int))) $ words x
--                case dir of
--                  "forward" -> do put (aim, (horiz+val, depth+(aim*val)))
--                  "up"      -> do put (aim-val, (horiz, depth))
--                  "down"    -> do put (aim+val, (horiz, depth))
--                p2 xs
