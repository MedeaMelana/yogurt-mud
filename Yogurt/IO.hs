module Yogurt.IO
  (writeToTTY, splitAtPrompt
  , maybeInput
  , hGetImpatientLine
  ) where

import System.IO
import System.Console.Readline
import Control.Monad (when)
import Data.List (elemIndices)

writeToTTY :: String -> IO ()
writeToTTY msg = do
  -- Find out which part of the message is the prompt.
  let (prePrompt, prompt) = splitAtPrompt msg

  -- Empty buffer.
  buf <- getLineBuffer
  pt  <- getPoint
  setLineBuffer ""
  setPoint 0  -- AAARGH
  redisplay

  -- Print prePrompt.
  when (prePrompt /= "") $ do
    putStr prePrompt
    onNewLine

  -- Set prompt as new message; put buffer and point back.
  message prompt
  insertText buf
  setPoint pt
  redisplay

-- Splits a message x in two submessages y and z such that:
-- * x == y ++ z
-- * all (/= '\n') z
-- * null y || last y == '\n'  (i.e. z is maximal)
splitAtPrompt :: String -> (String, String)
splitAtPrompt cs = case elemIndices '\n' cs of
  [] -> ("", cs)
  is -> splitAt (last is + 1) cs

-- Takes an input method and catches errors, returning results in the Maybe monad.
maybeInput :: IO String -> IO (Maybe String)
maybeInput input = fmap Just input `catch` const (return Nothing)

-- Waits for input, but once the first character is read, waits
-- no longer than the specified number of ms before giving up.
hGetImpatientLine :: Handle -> Int -> IO String
hGetImpatientLine h patience = rec where
  rec = do
    c <- hGetChar h
    if c == '\n'
      then return [c]
      else do
        b <- hWaitForInput h patience
        if b
          then rec >>= return . (c:)
          else return [c]

