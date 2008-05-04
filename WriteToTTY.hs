module WriteToTTY where

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
