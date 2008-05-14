{-# OPTIONS_GHC -fglasgow-exts #-}

module Yogurt.Mud
  ( Mud, MudState, Hook(..), Destination(..), Pattern, Timer(..), Result(..)  -- types
  , emptyMud
  , mkHook, mkPrioHook, chHook, rmHook, allHooks   -- hooks
  , triggeredHook, matchedLine, group              -- triggered hooks
  , mkVar, setVar, readVar, modifyVar              -- variables
  , mkTimer, rmTimer, existsTimer                  -- timers
  , trigger, io, flushResults
  , runIO
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

type Mud = State MudState

data MudState = MudState
  { hooks     :: IntMap Hook
  , vars      :: IntMap Value
  , timers    :: IntMap Timer
  , supply    :: [Int]
  , matchInfo :: Maybe MatchInfo
  , results   :: [Result]
  }


-- Hooks.
data Hook = Hook
  { hid         :: Int
  , priority    :: Int
  , destination :: Destination
  , pattern     :: Pattern
  , action      :: Mud ()
  }
data Destination = Local | Remote deriving (Eq, Show)
type Pattern = String
type MatchInfo = (Hook, String, [String]) -- Triggered hook; matched input line; regex groups.

instance Show Hook where
  show (Hook hid prio dest pat _) = "Hook #" ++ show hid ++ " @" ++ show dest ++ " [" ++ pat ++ "]"


-- Variables.
data Var a = Var Int
data Value = forall a. Value a

-- Timers.
data Timer = Timer
  { tid     :: Int
  , taction :: Mud ()
  }

-- Results.
data Result
  = Send Destination String  -- no implicit newlines!
  | forall a. RunIO (IO a) (a -> Mud ())
  | NewTimer Timer Int  -- interval in ms


-- The initial state.

emptyMud :: MudState
emptyMud = MudState empty empty empty [0..] Nothing []


-- Helper functions for querying and manipulating state.

updateHooks :: (IntMap Hook -> IntMap Hook) -> Mud ()
updateHooks f = modify $ \s -> s { hooks = f (hooks s) }

updateVars :: (IntMap Value -> IntMap Value) -> Mud ()
updateVars f = modify $ \s -> s { vars = f (vars s) }

updateTimers :: (IntMap Timer -> IntMap Timer) -> Mud ()
updateTimers f = modify $ \s -> s { timers = f (timers s) }

addResult :: Result -> Mud ()
addResult r = modify $ \s -> s { results = results s ++ [r] }

flushResults :: Mud [Result]
flushResults = do
  rs <- gets results
  modify $ \s -> s { results = [] }
  return rs

runIO :: IO () -> Mud ()
runIO io = withIO io (const $ return ())

withIO :: IO a -> (a -> Mud ()) -> Mud ()
withIO io act = addResult (RunIO io act)


-- Hooks

mkId :: Mud Int
mkId = do
  i <- gets (head . supply)
  modify $ \s -> s { supply = tail (supply s) }
  return i

mkHook :: Destination -> Pattern -> Mud a -> Mud Hook
mkHook = mkPrioHook 0

mkPrioHook :: Int -> Destination -> Pattern -> Mud a -> Mud Hook
mkPrioHook prio dest pat act = do
  hid <- mkId
  let hook = Hook hid prio dest pat (act >> return ())
  chHook hook
  return hook

-- Saves a changed hook, or reactivates it.
chHook :: Hook -> Mud ()
chHook hook = updateHooks $ insert (hid hook) hook

rmHook :: Hook -> Mud ()
rmHook = updateHooks . delete . hid

-- Yields all current hooks, high priority first.
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

triggeredHook :: Mud Hook
triggeredHook = getMatchInfo >>= return . (\(x,_,_) -> x)

matchedLine :: Mud String
matchedLine = getMatchInfo >>= return . (\(_,x,_) -> x)

group :: Int -> Mud String
group n = getMatchInfo >>= (return . (!! n) . (\(_,_,x) -> x))


-- Variables

mkVar :: a -> Mud (Var a)
mkVar val = do
  i <- mkId
  setVar (Var i) val
  return (Var i)

setVar :: Var a -> a -> Mud ()
setVar (Var i) val = updateVars $ insert i (Value val)

readVar :: Var a -> Mud a
readVar (Var i) = do
  varmap <- gets vars
  Value val <- lookup i varmap
  return (unsafeCoerce val)

modifyVar :: Var a -> (a -> a) -> Mud ()
modifyVar var f = readVar var >>= setVar var . f


-- Timers

mkTimer :: Int -> Mud () -> Mud Timer
mkTimer time act = do
  i <- mkId
  let timer = Timer i act
  updateTimers $ insert i timer
  addResult (NewTimer timer time)
  return timer

rmTimer :: Timer -> Mud Bool
rmTimer (Timer ti _) = do
  b <- gets (member ti . timers)
  updateTimers $ delete ti
  return b

existsTimer :: Timer -> Mud Bool
existsTimer (Timer ti _) = gets (member ti . timers)  


-- Matching of hooks

-- Looks for a hook to match the message. If one is found, fire is
-- called; otherwise, the message is passed on to the destination.
trigger :: Destination -> String -> Mud ()
trigger dest message = do
    hs <- allHooks
    case filter ok hs of
      []       -> io dest message
      (hook:_) -> fire message hook
  where
    ok hook = destination hook == dest && rmAnsi message =~ pattern hook

-- Executes the hook's action based on the matching message.
fire :: String -> Hook -> Mud ()
fire message hook = do
    oldMatchInfo <- gets matchInfo
    setMatchInfo $ Just (hook, message, match : groups)
    action hook
    setMatchInfo oldMatchInfo
  where
    (_, match, _, groups) = rmAnsi message =~ pattern hook :: (String, String, String, [String])

-- Immediately write a message to a destination, without triggering hooks.
io :: Destination -> String -> Mud ()
io ch message = addResult (Send ch message)
