module Main where

import System.Console.Readline
import Control.Concurrent
import IO

main :: IO ()
main = do
  forkIO (output 0)
  input

output :: Int -> IO ()
output n = do
  let msg = ("" ++ show n ++ "> ")
  writeToTTY msg
  threadDelay 1000000
  output (n + 1)

input :: IO ()
input = do
  maybeLine <- readline ""
  putStrLn ("Read " ++ show maybeLine)
  case maybeLine of
    Nothing   -> return ()
    Just line -> do
      addHistory line
      input
