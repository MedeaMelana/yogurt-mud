module Main where

import Yogurt
import Yogurt.Utils
import Data.Char (isSpace, isDigit)

main :: IO ()
main = connect "eclipse.cs.pdx.edu" 7680 newmoon

newmoon :: Mud ()
newmoon = do
  mkTriggerOnce "^Enter your name:" $ do
    sendln "medea"
  mkTriggerOnce "^Medea enters the game" $ do
    sendln "wizlist"
    mkTriggerOnce ">" (echoln "")
    return ()

  mkTrigger "tells you: " (echo "\BEL")
  
  mkCommand "tp" $ do
    dest <- group 1
    if all isSpace dest
      then echoln "Teleport to where?"
      else do
        sendln "cast teleport"
        mkTriggerOnce "^You feel ready to teleport now" $ do
          sendln ("teleport to" ++ dest)
        return ()

  mkHook Remote "system (.*)" $ do
    command <- group 1
    system command
  
  mkHook Remote "^[0-9]+[neswud]$" $ do
    (n, dir) <- fmap (span isDigit) (group 0)
    times (read n) (sendln dir)
  
  startLogging "NewMoon"
  
  return ()

times :: Monad m => Int -> m a -> m [a]
times n = sequence . replicate n
