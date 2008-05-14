{-# OPTIONS_GHC -fglasgow-exts #-}

-- | Convenience functions on top of "Yogurt.Mud".
module Yogurt.Utils (
  -- * Re-exports
  module Yogurt.Mud,

  -- * Hook and timer derivatives
  mkTrigger, mkTriggerOnce,
  mkAlias, mkArgAlias, mkCommand,
  mkTimerOnce,

  -- * Sending messages
  receive, sendln, echo, echoln, echorln,

  -- * Logging
  Logger, startLogging, stopLogging,

  -- * Miscellaneous
  matchMore,
  system

  ) where

import Yogurt.Mud
import qualified System.Cmd as Cmd
import System.IO.Unsafe
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)
import Data.Time.LocalTime (getZonedTime)



-- Hook and timer derivatives.


-- | Creates a hook that watches messages headed to the terminal. When fired, the message is passed on to the terminal and the action is executed.
mkTrigger :: Pattern -> Mud a -> Mud Hook
mkTrigger pat act = mkHook Local pat (matchedLine >>= echo >> act)

-- | Like 'mkTrigger', but fires at most once.
mkTriggerOnce :: Pattern -> Mud a -> Mud Hook
mkTriggerOnce pat act = mdo  -- whoo! recursive monads!
  hook <- mkTrigger pat (act >> rmHook hook)
  return hook

-- | @mkAlias command subst@ creates a hook that watches messages headed to the remote MUD. If the message is or starts with the word @command@, the command is replaced by @subst@ before being sent to the MUD.
mkAlias :: String -> String -> Mud Hook
mkAlias pat subst = mkHook Remote ("^" ++ pat ++ "($| .*$)") $ do
  suffix <- group 1
  echorln (subst ++ suffix)

-- | Like 'mkAlias', @mkArgAlias command subst@ creates a hook that watches messages headed to the remote MUD. But here the whole message is substituted instead of just the first command word, and the substitution depends on the command's arguments.
mkArgAlias :: String -> ([String] -> String) -> Mud Hook
mkArgAlias pat f = mkHook Remote ("^" ++ pat ++ "($| .*$)") $ do
  args <- fmap words (group 1)
  echorln (f args)

-- | Like 'mkAlias', but instead of substituting the command, a program is executed.
mkCommand :: String -> Mud a -> Mud Hook
mkCommand pat = mkHook Remote ("^" ++ pat ++ "($| .*$)")

-- | Creates a timer that fires only once.
mkTimerOnce :: Interval -> Mud a -> Mud Timer
mkTimerOnce interval act = mdo
  t <- mkTimer interval (act >> rmTimer t)
  return t



-- Sending messages.


-- | Sends a message to the terminal, triggering hooks.
receive :: String -> Mud ()
receive = trigger Local

-- | Sends a message appended with a newline character to the MUD, triggering hooks.
sendln :: String -> Mud ()
sendln m = trigger Remote (m ++ "\n")

-- | Sends a message to the terminal, without triggering hooks.
echo :: String -> Mud ()
echo = io Local

-- | Sends a message appended with a newline character to the terminal, without triggering hooks.
echoln :: String -> Mud ()
echoln m = echo (m ++ "\n")

-- | Sends a message appended with a newline character to the MUD, without triggering hooks.
echorln :: String -> Mud ()
echorln m = io Remote (m ++ "\n")



-- Logging.

type Logger = (Hook, Hook)  -- Remote, Local

-- | @startLogging name@ causes all messages to be logged in a file called @name-yyyymmdd-hhmm.log@. The used hooks have priority 100.
startLogging :: String -> Mud Logger
startLogging name = do
  let suffix = unsafePerformIO $
        fmap (formatTime defaultTimeLocale "-%Y%m%d-%H%M.log") getZonedTime
  let filename = name ++ suffix
  let record dest = mkPrioHook 100 dest "^" $ do
        line <- matchedLine
        runIO (appendFile filename line)
        matchMore
  r <- record Remote
  l <- record Local
  return (r, l)

-- | Stops the logger.
stopLogging :: Logger -> Mud ()
stopLogging (r, l) = do
  rmHook r
  rmHook l



-- Miscellaneous.


-- | When called from a hook body, causes matching to continue (using 'trigger') on the currently triggering message. The firing hook won't fire again on this specific message.
matchMore :: Mud ()
matchMore = do
  h <- triggeredHook
  m <- matchedLine
  rmHook h
  trigger (destination h) m
  setHook h

-- | Executes a shell command.
system :: String -> Mud ()
system = runIO . Cmd.system
