module Ansi where

-- Removes ANSI sequences from a string.
rmAnsi :: String -> String
rmAnsi [] = []
rmAnsi ab = a ++ (rmAnsi . tail' . dropWhile (/= 'm')) b
  where (a, b) = break (== '\ESC') ab

tail' :: [a] -> [a]
tail' [] = []
tail' xs = tail xs

