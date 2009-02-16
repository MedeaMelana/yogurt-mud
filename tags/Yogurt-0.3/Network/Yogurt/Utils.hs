{-# OPTIONS_GHC -fglasgow-exts #-}

-- | Convenience functions on top of "Yogurt.Mud".
module Network.Yogurt.Utils (
  -- * Hook and timer derivatives
  mkTrigger, mkTriggerOnce,
  mkAlias, mkArgAlias, mkCommand,

  -- * Timers
  Timer, Interval, mkTimer, mkTimerOnce, rmTimer, isTimerActive,

  -- * Sending messages
  receive, sendln, echo, echoln, echorln, bell,

  -- * Logging
  Logger, startLogging, stopLogging,

  -- * Miscellaneous
  matchMore, matchMoreOn, matchMoreOn'

  ) where

import Network.Yogurt.Mud
import Control.Concurrent
import Control.Monad

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



-- Section: Timers.


-- | The abstract Time type.
newtype Timer = Timer (Var Bool)

-- | Interval in milliseconds.
type Interval = Int

-- | @mkTimer interval prog@ creates a timer that executes @prog@ every @interval@ milliseconds.
mkTimer :: Interval -> Mud a -> Mud Timer
mkTimer interval prog = do
    vActive <- mkVar True
    
    let timerCycle :: RunMud -> IO ()
        timerCycle runMud = do
          threadDelay (1000 * interval)  -- interval in ms, threadDelay expects micros

          -- Execute timer action only if timer hasn't been deactivated in the meantime.
          runMud $ do
            active <- readVar vActive
            when active (prog >> return ())

          -- Maybe the timer's action removed the timer. If not, run again.
          again <- runMud (readVar vActive)
          when again (timerCycle runMud)

    forkWithCallback timerCycle
    return (Timer vActive)

-- | Creates a timer that fires only once.
mkTimerOnce :: Interval -> Mud a -> Mud Timer
mkTimerOnce interval act = mdo
  t <- mkTimer interval (act >> rmTimer t)
  return t

-- | Disables the timer.
rmTimer :: Timer -> Mud ()
rmTimer (Timer vActive) = setVar vActive False

-- | Checks whether a timer is active. A timer is deactivated by 'rmTimer'.
isTimerActive :: Timer -> Mud Bool
isTimerActive (Timer vActive) = readVar vActive



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

-- | Sends a bell character to the terminal.
bell :: Mud ()
bell = echo "\BEL"



-- Logging.

type Logger = (Hook, Hook)  -- Remote, Local

-- | @startLogging name@ causes all messages to be logged in a file called @name-yyyymmdd-hhmm.log@. The used hooks have priority 100.
startLogging :: String -> Mud Logger
startLogging name = do
  suffix <- liftIO $ fmap (formatTime defaultTimeLocale "-%Y%m%d-%H%M.log") getZonedTime
  let filename = name ++ suffix
  let record dest = mkPrioHook 100 dest "^" $ do
        line <- matchedLine
        liftIO $ appendFile filename line
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


-- | When called from a hook body, gives hooks that haven't been considered yet
-- a chance to match on the currently triggering message. Useful if you want to
-- build a hook that only has a side-effect and doesn't want to directly affect
-- the other active hooks.
matchMore :: Mud ()
matchMore = matchedLine >>= matchMoreOn

-- | Like 'matchMore', but allows specification of the message that is passed on.
matchMoreOn :: String -> Mud ()
matchMoreOn message = do
  h <- triggeredHook
  triggerJust (> h) (hDestination h) message

-- | Like 'matchMoreOn', but also makes the currently firing hook eligible for firing again.
matchMoreOn' :: String -> Mud ()
matchMoreOn' message = do
  h <- triggeredHook
  triggerJust (>= h) (hDestination h) message
