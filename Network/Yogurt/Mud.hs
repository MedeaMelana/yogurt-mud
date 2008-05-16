{-# OPTIONS_GHC -fglasgow-exts #-}

-- | The core of Yogurt, consisting of the Mud monad and all functions manipulating this monad.
module Network.Yogurt.Mud (

  -- * Types
  Mud, MudState, emptyMud,
  Hook,
  Destination(..),
  Pattern,
  Timer, Interval,
  Result(..),

  -- * Hooks
  -- | A hook watches a channel for messages matching a specific regular expression. When a hook fires, the triggering message is consumed and the hook's action is executed. When a message doesn't trigger any hooks, it is sent on to its destination. A hook's action may query for match-specific data; see section Match Information. At most one hook fires for each message, unless the hook's action explicitly sends the message through 'trigger' again. If several hooks match, only the hook with the highest priority fires. If there is still a tie, the hook that was defined last (using 'mkHook') fires.
  mkHook, mkPrioHook, setHook, rmHook, allHooks,
  
  -- ** Hook record fields
  -- | Use these in combination with 'setHook' to update hooks.
  hPriority, hDestination, hPattern, hAction,

  -- * Match information
  -- | #MatchInformation# Functions for querying the currently firing hook. These functions should only be called from within a hook's body.
  triggeredHook, matchedLine, group,

  -- * Variables
  mkVar, setVar, readVar, modifyVar,

  -- * Timers
  mkTimer, rmTimer, existsTimer, allTimers,
  -- ** Timer record fields
  tAction, tInterval,

  -- * Triggering hooks
  trigger, io, flushResults,

  -- * IO
  withIO, runIO

  ) where

import Prelude hiding (lookup)
import Data.IntMap (IntMap, empty, insert, delete, lookup, elems, member)
import Unsafe.Coerce
import Text.Regex.Posix
import Network.Yogurt.Ansi
import Control.Monad.State
import Data.List (sortBy)
import Data.Function (on)
import Data.Ord (comparing)



-- Section: Types.


-- | The Mud monad is a simple state monad.
type Mud = State MudState

-- | State internal to the Mud monad.
data MudState = MudState
  { hooks     :: IntMap Hook
  , vars      :: IntMap Opaque
  , timers    :: IntMap Timer
  , supply    :: [Int]
  , matchInfo :: Maybe MatchInfo
  , results   :: [Result]
  }

-- | The initial state of the Mud monad.
emptyMud :: MudState
emptyMud = MudState empty empty empty [0..] Nothing []

-- | The abstract Hook type.
data Hook = Hook
  { hId         :: Int
  , hPriority    :: Int          -- ^ Yields the hook's priority. 
  , hDestination :: Destination  -- ^ Yields the destination this hook watches.
  , hPattern     :: Pattern      -- ^ Yields the pattern messages must have for this hook to fire.
  , hAction      :: Mud ()       -- ^ Yields the Mud program to execute when the hook fires.
  }

instance Show Hook where
  show (Hook hid prio dest pat _) = "Hook #" ++ show hid ++ " @" ++ show prio ++ " " ++ show dest ++ " [" ++ pat ++ "]"

-- | Used to distinguish between messages going in different directions.
data Destination
  = Local   -- ^ The message is headed towards the user's terminal.
  | Remote  -- ^ The message is headed towards the remote MUD server.
  deriving (Eq, Show, Read, Enum, Ord)

-- | A Pattern is a regular expression.
type Pattern = String

type MatchInfo = (Hook, String, [String]) -- Triggered hook; matched input line; regex groups.

-- | Variables hold temporary, updatable, typed data.
data Var a = Var Int

data Opaque = forall a. Opaque a

-- Timers.
-- | The abstract Timer type.
data Timer = Timer
  { tId     :: Int
  , tAction :: Mud ()      -- ^ Yields the timer's action.
  , tInterval :: Interval  -- ^ Yields the timer's interval.
  }

-- | Interval in milliseconds.
type Interval = Int

-- | A @Result@ is a consequence of executing a @Mud@ program.
data Result
  = Send Destination String  -- no implicit newlines!
  | forall a. RunIO (IO a) (a -> Mud ())
  | NewTimer Timer



-- Section: Helper functions for querying and manipulating state.


mkId :: Mud Int
mkId = do
  i <- gets (head . supply)
  modify $ \s -> s { supply = tail (supply s) }
  return i

updateHooks :: (IntMap Hook -> IntMap Hook) -> Mud ()
updateHooks f = modify $ \s -> s { hooks = f (hooks s) }

updateVars :: (IntMap Opaque -> IntMap Opaque) -> Mud ()
updateVars f = modify $ \s -> s { vars = f (vars s) }

updateTimers :: (IntMap Timer -> IntMap Timer) -> Mud ()
updateTimers f = modify $ \s -> s { timers = f (timers s) }

addResult :: Result -> Mud ()
addResult r = modify $ \s -> s { results = results s ++ [r] }

-- | Yields all accumulated results and removes them from the state. Used by "Yogurt.Engine" in the main loop.
flushResults :: Mud [Result]
flushResults = do
  rs <- gets results
  modify $ \s -> s { results = [] }
  return rs



-- Section: Hooks.


-- | Calls 'mkPrioHook' with priority 0.
mkHook :: Destination -> Pattern -> Mud a -> Mud Hook
mkHook = mkPrioHook 0

-- | Creates and installs a hook that watches messages headed to the specified destination and match the specified pattern.
mkPrioHook :: Int -> Destination -> Pattern -> Mud a -> Mud Hook
mkPrioHook prio dest pat act = do
  hid <- mkId
  let hook = Hook hid prio dest pat (act >> return ())
  setHook hook
  return hook

-- | Saves a changed hook, or reactivates it.
setHook :: Hook -> Mud ()
setHook hook = updateHooks $ insert (hId hook) hook

-- | Disables a hook.
rmHook :: Hook -> Mud ()
rmHook = updateHooks . delete . hId

-- | Yields all current hooks in preferred firing order.
allHooks :: Mud [Hook]
allHooks = gets (reverse . sortBy (comparing hPriority) . elems . hooks)



-- Section: MatchInfo.


setMatchInfo :: Maybe MatchInfo -> Mud ()
setMatchInfo mi = modify $ \s -> s { matchInfo = mi }

getMatchInfo :: Mud MatchInfo
getMatchInfo = do
  mi <- gets matchInfo
  case mi of
    Nothing  -> fail "No match is available."
    Just mi' -> return mi'

-- | Yields the hook that is currently firing.
triggeredHook :: Mud Hook
triggeredHook = getMatchInfo >>= return . (\(x,_,_) -> x)

-- | Yields the message that triggered the currently firing hook.
matchedLine :: Mud String
matchedLine = getMatchInfo >>= return . (\(_,x,_) -> x)

-- | Yields the regex group from the matched line.
group :: Int -> Mud String
group n = getMatchInfo >>= (return . (!! n) . (\(_,_,x) -> x))



-- Section: Variables.

-- | Creates a variable with an initial value.
mkVar :: a -> Mud (Var a)
mkVar val = do
  i <- mkId
  setVar (Var i) val
  return (Var i)

-- | Updates a variable to a new value.
setVar :: Var a -> a -> Mud ()
setVar (Var i) val = updateVars $ insert i (Opaque val)

-- | Yields the variable's current value.
readVar :: Var a -> Mud a
readVar (Var i) = do
  varmap <- gets vars
  Opaque val <- lookup i varmap
  return (unsafeCoerce val)

-- | Updates the variable using the update function.
modifyVar :: Var a -> (a -> a) -> Mud ()
modifyVar var f = readVar var >>= setVar var . f



-- Section: Timers.


-- | @mkTimer interval prog@ creates a timer that executes @prog@ every @interval@ milliseconds.
mkTimer :: Interval -> Mud a -> Mud Timer
mkTimer interval prog = do
  i <- mkId
  let timer = Timer i (prog >> return ()) interval
  updateTimers $ insert i timer
  addResult (NewTimer timer)
  return timer

-- | Disables the timer.
rmTimer :: Timer -> Mud ()
rmTimer = updateTimers . delete . tId

-- | Checks whether a timer is active.
existsTimer :: Timer -> Mud Bool
existsTimer (Timer ti _ _) = gets (member ti . timers)

-- | Yields all currently active timers.
allTimers :: Mud [Timer]
allTimers = gets (elems . timers)



-- Section: Triggering hooks


-- | If the message triggers a hook, it is fired. Otherwise, the message is passed on to the destination using 'io'.
trigger :: Destination -> String -> Mud ()
trigger dest message = do
    hs <- allHooks
    case filter ok hs of
      []       -> io dest message
      (hook:_) -> fire message hook
  where
    ok hook = hDestination hook == dest && rmAnsi message =~ hPattern hook

-- | Executes the hook's action based on the matching message.
fire :: String -> Hook -> Mud ()
fire message hook = do
    oldMatchInfo <- gets matchInfo
    setMatchInfo $ Just (hook, message, match : groups)
    hAction hook
    setMatchInfo oldMatchInfo
  where
    (_, match, _, groups) = rmAnsi message =~ hPattern hook :: (String, String, String, [String])

-- | Immediately write a message to a destination, without triggering hooks.
io :: Destination -> String -> Mud ()
io ch message = addResult (Send ch message)



-- Section: IO.


-- | Invokes withIO, discarding the IO's result.
runIO :: IO a -> Mud ()
runIO io = withIO io (const $ return ())

-- | Executes the IO action soon. The computation's result is passed to the function, and the resulting Mud computation is executed.
withIO :: IO a -> (a -> Mud ()) -> Mud ()
withIO io act = addResult (RunIO io act)
