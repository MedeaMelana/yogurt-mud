{-# OPTIONS_GHC -fglasgow-exts #-}

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

  mkCommand "_go" $ mdo
    t <- mkTimerOnce 1000  (echoln "hello!" >> rmHook h)
    h <- mkCommand "_stop" (rmTimer t       >> rmHook h)
    return ()

  mkPrioHook 5 Remote "^(.*);(.*)$" $ do
    group 1 >>= sendln
    group 2 >>= sendln
  
  mkCommand "lshooks" $ do
    hs <- allHooks
    echoln $ unlines $ map show hs

  return ()

times :: Monad m => Int -> m a -> m [a]
times n = sequence . replicate n
