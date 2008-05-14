{-# OPTIONS_GHC -fglasgow-exts #-}

module Yogurt.Mud (

  -- * Types
  Mud, MudState, emptyMud,
  Hook,
  Destination(..),
  Pattern,
  Timer,
  Result(..),

  -- * Hooks
-- | A hook watches a channel for messages matching a specific regular expression. When a hook fires, the triggering message is intercept and the hook's action is executed. When a message doesn't trigger any hooks, it is sent to its destination immediately. A hook's action may query for match-specific data; see "Yogurt.Mud#MatchInformation". At most one hook fires for each message. If several hooks match, only the hook with the highest priority fires. If there is still a tie, the hook that was defined first fires.
  mkHook, mkPrioHook, chHook, rmHook, allHooks,
  priority, destination, pattern, action,

  -- * Match information
  -- | Several functions for querying the currently firing hook. These functions should only be called from within a hook's body.
  triggeredHook, matchedLine, group,

  -- * Variables
  mkVar, setVar, readVar, modifyVar,

  -- * Timers
  mkTimer, rmTimer, existsTimer, allTimers,
  taction,

  -- * Triggering hooks
  trigger, io, flushResults,

  -- * IO
  withIO, runIO

  ) where

import Prelude hiding (lookup)
import Data.IntMap (IntMap, empty, insert, delete, lookup, elems, member)
import Unsafe.Coerce
import Text.Regex.Posix
import Yogurt.Ansi
import Control.Monad.State
import Data.List (sortBy)
import Data.Function (on)
import Data.Ord (comparing)


-- Types.

-- | The Mud monad is a simple state monad.
type Mud = State MudState

-- | State internal to the Mud monad.
data MudState = MudState
  { hooks     :: IntMap Hook
  , vars      :: IntMap Value
  , timers    :: IntMap Timer
  , supply    :: [Int]
  , matchInfo :: Maybe MatchInfo
  , results   :: [Result]
  }

-- | The initial state of the Mud monad.
emptyMud :: MudState
emptyMud = MudState empty empty empty [0..] Nothing []


-- Hooks.
-- | The abstract Hook type.
data Hook = Hook
  { hid         :: Int
  , priority    :: Int          -- ^ Yields the hook's priority. 
  , destination :: Destination  -- ^ Yields the destination this hook watches.
  , pattern     :: Pattern      -- ^ Yields the pattern messages must have for this hook to fire.
  , action      :: Mud ()       -- ^ Yields the Mud program to execute when the hook fires.
  }

instance Show Hook where
  show (Hook hid prio dest pat _) = "Hook #" ++ show hid ++ " @" ++ show dest ++ " [" ++ pat ++ "]"

-- | Used to distinguish between messages going in different directions.
data Destination
  = Local   -- ^ The message is headed towards the user's terminal.
  | Remote  -- ^ The message is headed towards the remote MUD server.
  deriving (Eq, Show, Read, Enum)

-- | A Pattern is a regular expression.
type Pattern = String

type MatchInfo = (Hook, String, [String]) -- Triggered hook; matched input line; regex groups.


-- Variables.

-- | Variables hold temporary, updatable, typed data.
data Var a = Var Int

data Value = forall a. Value a

-- Timers.
-- | The abstract Timer type.
data Timer = Timer
  { tid     :: Int
  , taction :: Mud ()  -- ^ Yields the timer's action.
  }

-- Results.

-- | A @Result@ is a consequence of executing a @Mud@ program.
data Result
  = Send Destination String  -- no implicit newlines!
  | forall a. RunIO (IO a) (a -> Mud ())
  | NewTimer Timer Int  -- interval in ms


-- Helper functions for querying and manipulating state.

mkId :: Mud Int
mkId = do
  i <- gets (head . supply)
  modify $ \s -> s { supply = tail (supply s) }
  return i

updateHooks :: (IntMap Hook -> IntMap Hook) -> Mud ()
updateHooks f = modify $ \s -> s { hooks = f (hooks s) }

updateVars :: (IntMap Value -> IntMap Value) -> Mud ()
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


-- Hooks

-- | Creates and installs a hook that watches messages headed to the specified destination and match the specified pattern. The hook has priority 0.
mkHook :: Destination -> Pattern -> Mud a -> Mud Hook
mkHook = mkPrioHook 0

-- | Like 'mkHook'. Creates a prioritized hook.
mkPrioHook :: Int -> Destination -> Pattern -> Mud a -> Mud Hook
mkPrioHook prio dest pat act = do
  hid <- mkId
  let hook = Hook hid prio dest pat (act >> return ())
  chHook hook
  return hook

-- | Saves a changed hook, or reactivates it.
chHook :: Hook -> Mud ()
chHook hook = updateHooks $ insert (hid hook) hook

-- | Disables a hook.
rmHook :: Hook -> Mud ()
rmHook = updateHooks . delete . hid

-- | Yields all current hooks in preferred firing order: first ordered by priority, then for ties sorted by definition time.
allHooks :: Mud [Hook]
allHooks = gets (reverse . sortBy (comparing priority) . elems . hooks)


-- MatchInfo

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


-- Variables

-- | Creates a variable with an initial value.
mkVar :: a -> Mud (Var a)
mkVar val = do
  i <- mkId
  setVar (Var i) val
  return (Var i)

-- | Updates a variable to a new value.
setVar :: Var a -> a -> Mud ()
setVar (Var i) val = updateVars $ insert i (Value val)

-- | Yields the variable's current value.
readVar :: Var a -> Mud a
readVar (Var i) = do
  varmap <- gets vars
  Value val <- lookup i varmap
  return (unsafeCoerce val)

-- | Updates the variable using the update function.
modifyVar :: Var a -> (a -> a) -> Mud ()
modifyVar var f = readVar var >>= setVar var . f


-- Timers

-- | @mkTimer interval prog@ creates a timer that executes @prog@ every @interval@ milliseconds.
mkTimer :: Int -> Mud () -> Mud Timer
mkTimer interval prog = do
  i <- mkId
  let timer = Timer i prog
  updateTimers $ insert i timer
  addResult (NewTimer timer interval)
  return timer

-- | Disables the timer.
rmTimer :: Timer -> Mud Bool
rmTimer (Timer ti _) = do
  b <- gets (member ti . timers)
  updateTimers $ delete ti
  return b

-- | Checks whether a timer is active.
existsTimer :: Timer -> Mud Bool
existsTimer (Timer ti _) = gets (member ti . timers)

-- | Yields all currently active timers.
allTimers :: Mud [Timer]
allTimers = gets (elems . timers)


-- Matching of hooks

-- | If the message triggers a hook, it is fired. Otherwise, the message is passed on to the destination using 'io'.
trigger :: Destination -> String -> Mud ()
trigger dest message = do
    hs <- allHooks
    case filter ok hs of
      []       -> io dest message
      (hook:_) -> fire message hook
  where
    ok hook = destination hook == dest && rmAnsi message =~ pattern hook

-- | Executes the hook's action based on the matching message.
fire :: String -> Hook -> Mud ()
fire message hook = do
    oldMatchInfo <- gets matchInfo
    setMatchInfo $ Just (hook, message, match : groups)
    action hook
    setMatchInfo oldMatchInfo
  where
    (_, match, _, groups) = rmAnsi message =~ pattern hook :: (String, String, String, [String])

-- | Immediately write a message to a destination, without triggering hooks.
io :: Destination -> String -> Mud ()
io ch message = addResult (Send ch message)

-- | Invokes withIO, discarding the IO's result.
runIO :: IO a -> Mud ()
runIO io = withIO io (const $ return ())

-- | Executes the IO action soon. The computation's result is passed to the function, and the resulting Mud computation is executed.
withIO :: IO a -> (a -> Mud ()) -> Mud ()
withIO io act = addResult (RunIO io act)
