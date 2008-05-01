module Main where

import System.IO
import Network
import Control.Concurrent

main :: IO ()
main = do
  putStrLn "Connecting..."
  conn <- connectTo "darkover.isilm.com" (PortNumber (fromIntegral 5000))
  l <- hGetContents conn
  forkIO (readTTY conn)
  putStr l

readTTY :: Handle -> IO ()
readTTY h = do
  line <- getLine
  hPutStrLn h line
  hFlush h
  readTTY h
