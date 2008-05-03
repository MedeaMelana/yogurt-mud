{-# OPTIONS_GHC -fglasgow-exts #-}

module Mud where

import Prelude hiding (lookup)
import Data.IntMap (IntMap, empty, insert, delete, lookup, elems)
import Control.Monad.Fix
import Unsafe.Coerce
import Text.Regex.Posix
import Debug.Trace (trace)


-- The Mud monad.

--type Mud = State State
data Mud a = Mud (MudState -> (MudState, a))

data MudState = MudState
  { hooks     :: IntMap Hook
  , vars      :: IntMap Value
  , supply    :: [Int]
  , matchInfo :: Maybe MatchInfo
  , results   :: [Result]
  }

type MatchInfo = (String, [String]) -- Matched input line; regex groups.

data Result
  = Send Destination String  -- no implicit newlines!
  deriving (Eq, Show)

data Destination = Local | Remote deriving (Eq, Show)

runMud :: Mud a -> MudState -> (MudState, a)
runMud (Mud f) init = f init

-- TODO: Use MonadState?

instance Monad Mud where
  (Mud f) >>= g = Mud $ \s ->
    let (t, x) = f s
        Mud h  = g x
     in h t
  return x = Mud $ \s -> (s, x)

instance MonadFix Mud where
  -- What on earth have I written here? But it works!
  -- mfix :: (a -> Mud a) -> Mud a
  mfix f = Mud $ \s ->
    let (Mud g) = (f . snd . g) s in g s

updateState :: (MudState -> MudState) -> Mud ()
updateState f = Mud $ \s -> (f s, ())

queryState :: (MudState -> a) -> Mud a
queryState q = Mud $ \s -> (s, q s)

initState :: MudState
initState = MudState empty empty [0..] Nothing []

-- Sadly, we cannot pass fields as function arguments.
-- updateField :: (MudState -> a) -> (a -> a) -> Mud ()
-- updateField field f = updateState $ \s -> s { field = f (field s) }

updateHooks :: (IntMap Hook -> IntMap Hook) -> Mud ()
updateHooks f = updateState $ \s -> s { hooks = f (hooks s) }

updateVars :: (IntMap Value -> IntMap Value) -> Mud ()
updateVars f = updateState $ \s -> s { vars = f (vars s) }

addResult :: Result -> Mud ()
addResult r = updateState $ \s -> s { results = results s ++ [r] }


-- Hooks

type Pattern = String
data Hook = Hook
  { hid         :: Int
  , destination :: Destination
  , pattern     :: Pattern
  , action      :: Mud ()
  }

instance Show Hook where
  show (Hook hid dest pat act) = "Hook " ++ show hid ++ " " ++ show dest ++ " [" ++ pat ++ "]"

mkId :: Mud Int
mkId = do
  i <- queryState (head . supply)
  updateState $ \s -> s { supply = tail (supply s) }
  return i

mkHook :: Destination -> Pattern -> Mud () -> Mud Hook
mkHook dest pat act = do
  hid <- mkId
  let hook = Hook hid dest pat act
  chHook hook
  return hook

chHook :: Hook -> Mud ()
chHook hook = updateHooks $ insert (hid hook) hook

rmHook :: Hook -> Mud ()
rmHook = updateHooks . delete . hid

allHooks :: Mud [Hook]
allHooks = queryState (elems . hooks)


-- MatchInfo

setMatchInfo :: Maybe MatchInfo -> Mud ()
setMatchInfo mi = updateState $ \s -> s { matchInfo = mi }

getMatchInfo :: Mud MatchInfo
getMatchInfo = do
  mi <- queryState matchInfo
  case mi of
    Nothing  -> fail "No match is available."
    Just mi' -> return mi'

group :: Int -> Mud String
group n = getMatchInfo >>= (return . (!! n) . snd)

match :: Mud String
match = getMatchInfo >>= return . fst


-- Hook derivatives

mkTrigger :: Pattern -> Mud () -> Mud Hook
mkTrigger pat act = mkHook Local pat (act >> match >>= echo)

mkTriggerOnce :: Pattern -> Mud () -> Mud Hook
mkTriggerOnce pat act = mdo  -- whoo! recursive monads!
  hook <- mkTrigger pat (act >> rmHook hook)
  return hook

mkAlias :: Pattern -> String -> Mud Hook
mkAlias pat subst = mkHook Remote ("^" ++ pat ++ "($| .*$)") $ do
  suffix <- group 1
  echor (subst ++ suffix)


-- Variables

data Var a = Var Int
data Value = forall a. Value a

mkVar :: a -> Mud (Var a)
mkVar val = do
  i <- mkId
  setVar (Var i) val
  return (Var i)

setVar :: Var a -> a -> Mud ()
setVar (Var i) val = updateVars $ insert i (Value val)

readVar :: Var a -> Mud a
readVar (Var i) = do
  varmap <- queryState vars
  Value val <- lookup i varmap
  return (unsafeCoerce val)

updateVar :: Var a -> (a -> a) -> Mud ()
updateVar var f = readVar var >>= setVar var . f


-- Matching of hooks

-- Removes ANSI sequences from a string.
rmAnsi :: String -> String
rmAnsi [] = []
rmAnsi ab = a ++ (rmAnsi . tail' . dropWhile (/= 'm')) b
  where (a, b) = break (== '\ESC') ab

tail' :: [a] -> [a]
tail' [] = []
tail' xs = tail xs

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
    oldMatchInfo <- queryState matchInfo
    setMatchInfo $ Just (message, match : groups)
    action hook
    setMatchInfo oldMatchInfo
  where
    (before, match, after, groups) = rmAnsi message =~ pattern hook :: (String, String, String, [String])

io :: Destination -> String -> Mud ()
io ch message = addResult (Send ch message)


-- Some convenience methods.

-- Applies appropriate hooks. If no hooks were triggered, the result is sent to the client.
receive :: String -> Mud ()
receive = trigger Local

-- Applies appropriate hooks. If no hooks were triggered, the result is sent to the server.
send :: String -> Mud ()
send m = trigger Remote (m ++ "\n")

-- Immediately sends the result to the client, without triggering hooks.
echo :: String -> Mud ()
echo m = io Local (m ++ "\n")

-- Immediately sends the result to the server, without triggering hooks.
echor :: String -> Mud ()
echor m = io Remote (m ++ "\n")
