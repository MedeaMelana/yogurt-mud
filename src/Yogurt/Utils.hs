{-# OPTIONS_GHC -fglasgow-exts #-}

module Yogurt.Utils
  ( mkTrigger, mkTriggerOnce
  , mkAlias, mkArgAlias, mkCommand
  , matchMore
  , mkTimerOnce
  , receive, sendln, echo, echoln, echorln
  , system
  , startLogging, stopLogging, Logger
  , module Yogurt.Mud
  ) where

import Yogurt.Mud
import qualified System.Cmd as Cmd
import System.IO.Unsafe
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)
import Data.Time.LocalTime (getZonedTime)


-- Hook derivatives.

mkTrigger :: Pattern -> Mud a -> Mud Hook
mkTrigger pat act = mkHook Local pat (matchedLine >>= echo >> act)

mkTriggerOnce :: Pattern -> Mud a -> Mud Hook
mkTriggerOnce pat act = mdo  -- whoo! recursive monads!
  hook <- mkTrigger pat (act >> rmHook hook)
  return hook

mkAlias :: String -> String -> Mud Hook
mkAlias pat subst = mkHook Remote ("^" ++ pat ++ "($| .*$)") $ do
  suffix <- group 1
  echorln (subst ++ suffix)

mkArgAlias :: String -> ([String] -> String) -> Mud Hook
mkArgAlias pat f = mkHook Remote ("^" ++ pat ++ "($| .*$)") $ do
  args <- fmap words (group 1)
  echorln (f args)

mkCommand :: String -> Mud a -> Mud Hook
mkCommand pat = mkHook Remote ("^" ++ pat ++ "($| .*$)")


-- Causes matching to continue on the current line. The currently active hook won't participate.
matchMore :: Mud ()
matchMore = do
  h <- triggeredHook
  m <- matchedLine
  rmHook h
  trigger (destination h) m
  chHook h


mkTimerOnce :: Int -> Mud () -> Mud Timer
mkTimerOnce interval act = mdo
  t <- mkTimer interval (act >> rmTimer t >> return ())
  return t
  

-- Trigger and io derivatives.

-- Applies appropriate hooks. If no hooks were triggered, the result is sent to the client.
receive :: String -> Mud ()
receive = trigger Local

-- Applies appropriate hooks. If no hooks were triggered, the result is sent to the server.
sendln :: String -> Mud ()
sendln m = trigger Remote (m ++ "\n")

-- Immediately sends the result to the client, without triggering hooks.
echo :: String -> Mud ()
echo = io Local

-- Immediately sends the result to the client, without triggering hooks.
echoln :: String -> Mud ()
echoln m = echo (m ++ "\n")

-- Immediately sends the result to the server, without triggering hooks.
echorln :: String -> Mud ()
echorln m = io Remote (m ++ "\n")


-- Executing system commands.

system :: String -> Mud ()
system command = runIO (Cmd.system command >> return ())


-- Logging.

type Logger = (Hook, Hook)  -- Remote, Local

startLogging :: String -> Mud Logger
startLogging name = do
  let suffix = unsafePerformIO $
        fmap (formatTime defaultTimeLocale "-%Y%m%d-%H%M.log") getZonedTime
  let filename = name ++ suffix
  let record dest = mkPrioHook 10 dest "^" $ do
        line <- matchedLine
        runIO (appendFile filename line)
        matchMore
  r <- record Remote
  l <- record Local
  return (r, l)

stopLogging :: Logger -> Mud ()
stopLogging (r, l) = do
  rmHook r
  rmHook l
