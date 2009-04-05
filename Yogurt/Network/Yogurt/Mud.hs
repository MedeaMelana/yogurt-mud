{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LiberalTypeSynonyms #-}

-- | The core of Yogurt, consisting of the Mud monad and all functions manipulating this monad.
module Network.Yogurt.Mud (

  -- * Types
  Mud, MudState, emptyMud,
  RunMud, Output,
  Hook,
  Destination(..),
  Pattern,
  Var,

  -- * Hooks
  -- | A hook watches a channel for messages matching a specific regular expression.
  -- When a hook fires, the triggering message is consumed and the hook's action is executed.
  -- When a message doesn't trigger any hooks, it is sent on to its destination.
  -- A hook's action may query for match-specific data; see section Match information.
  -- At most one hook fires for each message, unless the hook's action explicitly sends the message through 'trigger' again. If several hooks match, only the hook with the highest priority fires. If there is still a tie, the hook that was defined last (using 'mkHook') fires.
  mkHook, mkPrioHook, setHook, rmHook, allHooks,
  
  -- ** Hook record fields
  -- | Use these in combination with 'setHook' to update hooks.
  hPriority, hDestination, hPattern, hAction,

  -- ** Match information
  -- | #MatchInformation# Functions for querying the currently firing hook. These functions can only be called from within a hook's body.
  triggeredHook, matchedLine, before, group, after,

  -- * Variables
  mkVar, setVar, readVar, modifyVar,

  -- * Triggering hooks
  trigger, triggerJust, io,
  
  -- * Multi-threading
  liftIO, forkWithCallback

  ) where

import Prelude hiding (lookup)
import Data.IntMap (IntMap, empty, insert, delete, elems)
import Text.Regex.Posix ((=~))
import Network.Yogurt.Ansi
import Control.Monad.State
import Data.List (sort)
import Data.Function (on)
import Data.Ord (comparing)
import Data.Monoid (mconcat)
import Data.IORef
import Control.Concurrent (forkIO, ThreadId)



-- Section: Types.


-- | The Mud monad is a state monad over IO.
type Mud = StateT MudState IO

-- | Run a Mud computation in IO.
type RunMud = forall a. Mud a -> IO a

-- | Provides a way to output messages.
type Output = Destination -> String -> IO ()

-- | State internal to the Mud monad.
data MudState = MudState
  { hooks     :: IntMap Hook
  , supply    :: [Int]
  , matchInfo :: Maybe MatchInfo
  , mRunMud   :: RunMud
  , mOutput   :: Output
  }

-- | The initial state of the Mud monad.
emptyMud :: RunMud -> Output -> MudState
emptyMud = MudState empty [0..] Nothing

-- | The abstract @Hook@ type. For every pair of hooks @(h1, h2)@:
--
-- * @h1 == h2@ iff they were created by the same call to 'mkHook'.
--
-- * @h1 < h2@ iff @h1@ will match earlier than @h2@.
data Hook = Hook
  { hId          :: Int
  , hPriority    :: Int          -- ^ Yields the hook's priority. 
  , hDestination :: Destination  -- ^ Yields the destination this hook watches.
  , hPattern     :: Pattern      -- ^ Yields the pattern messages must have for this hook to fire.
  , hAction      :: Mud ()       -- ^ Yields the Mud program to execute when the hook fires.
  }

instance Eq Hook where
  (==) = (==) `on` hId

instance Ord Hook where
  compare = flip $ mconcat [comparing hDestination, comparing hPriority, comparing hId]

instance Show Hook where
  show (Hook hid prio dest pat _) = show dest ++ " @" ++ show prio ++ " [" ++ pat ++ "]"

-- | The direction in which a message is going.
data Destination
  = Local   -- ^ The message is headed towards the user's terminal.
  | Remote  -- ^ The message is headed towards the remote MUD server.
  deriving (Eq, Show, Read, Enum, Ord)

-- | A @Pattern@ is a regular expression.
type Pattern = String

data MatchInfo = MatchInfo
  { mTriggeredHook :: Hook
  , mMatchedLine   :: String
  , mBefore        :: String
  , mGroups        :: [String]
  , mAfter         :: String
  }

-- | Variables hold updatable, typed data.
newtype Var a = Var (IORef a)

type Id = Int



-- Section: Helper functions for querying and manipulating state.


mkId :: Mud Id
mkId = do
  i <- gets (head . supply)
  modify $ \s -> s { supply = tail (supply s) }
  return i

updateHooks :: (IntMap Hook -> IntMap Hook) -> Mud ()
updateHooks f = modify $ \s -> s { hooks = f (hooks s) }



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
allHooks = gets (sort . elems . hooks)



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
triggeredHook = fmap mTriggeredHook getMatchInfo

-- | Yields the message that triggered the currently firing hook.
matchedLine :: Mud String
matchedLine = fmap mMatchedLine getMatchInfo

-- | Yields the part of the triggering message that comes before the matched pattern.
before :: Mud String
before = fmap mBefore getMatchInfo

-- | Yields the regex group from the matched pattern. @group 0@ yields the complete match; higher indices correspond to the parenthesized groups.
group :: Int -> Mud String
group n = fmap ((!! n) . mGroups) getMatchInfo

-- | Yields the part of the triggering message that comes after the matched pattern.
after :: Mud String
after = fmap mAfter getMatchInfo



-- Section: Variables.

-- | Creates a variable with an initial value.
mkVar :: a -> Mud (Var a)
mkVar = liftM Var . liftIO . newIORef

-- | Updates a variable to a new value.
setVar :: Var a -> a -> Mud ()
setVar (Var var) = liftIO . writeIORef var

-- | Yields the variable's current value.
readVar :: Var a -> Mud a
readVar (Var var) = liftIO $ readIORef var

-- | Updates the variable using the update function.
modifyVar :: Var a -> (a -> a) -> Mud ()
modifyVar (Var var) = liftIO . modifyIORef var



-- Section: Triggering hooks

-- | Short for @'triggerJust' (const True)@.
trigger :: Destination -> String -> Mud ()
trigger = triggerJust (const True)

-- | If the message triggers a hook that passes the specified test, it is fired. Otherwise, the message is passed on to the destination using 'io'.
triggerJust :: (Hook -> Bool) -> Destination -> String -> Mud ()
triggerJust test dest message = do
    hs <- allHooks
    case filter ok hs of
      []       -> io dest message
      (hook:_) -> fire message hook
  where
    ok hook = test hook && hDestination hook == dest && rmAnsi message =~ hPattern hook

-- | Executes the hook's action based on the matching message.
fire :: String -> Hook -> Mud ()
fire message hook = do
    oldMatchInfo <- gets matchInfo
    setMatchInfo $ Just $ MatchInfo hook message before (match : groups) after
    hAction hook
    setMatchInfo oldMatchInfo
  where
    (before, match, after, groups) = rmAnsi message =~ hPattern hook :: (String, String, String, [String])

-- | Immediately write a message to a destination, without triggering hooks.
io :: Destination -> String -> Mud ()
-- io ch = addResult . Send ch
io to msg = do
  out <- gets mOutput
  liftIO $ out to msg

-- | Used when you want a forked thread to be able to call back into the @Mud@ monad.
-- Note that when using the @RunMud@ argument, the forked thread will have to contend
-- with the threads for the user input and MUD input, because only one @Mud@ computation
-- can run at any given time. The id of the forked thread is returned.
forkWithCallback :: (RunMud -> IO ()) -> Mud ThreadId
forkWithCallback action = do
  -- Note: compiler complains if mRunMud is written point-free.
  s <- get
  liftIO . forkIO . action $ mRunMud s
