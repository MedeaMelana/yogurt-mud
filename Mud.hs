{-# OPTIONS_GHC -fglasgow-exts #-}

module Mud where

import Prelude hiding (lookup)
import Data.IntMap (IntMap, empty, insert, delete, lookup, elems)
import Control.Monad.Fix
import Unsafe.Coerce
import Text.Regex.Posix
import Debug.Trace (trace)


-- The MUD monad.

--type MUD = State State
data MUD a = MUD (MudState -> (MudState, a))

data MudState = MudState
  { hooks    :: IntMap Hook
  , vars     :: IntMap Value
  , supply   :: [Int]
  , groups   :: [String]
  , results  :: [Result]
  }

data Result
  = Send Destination String
  | Error String
  deriving (Eq, Show)

data Destination = Local | Remote deriving (Eq, Show)

runMUD :: MUD a -> MudState -> (MudState, a)
runMUD (MUD f) init = f init

-- TODO: Use MonadState?

instance Monad MUD where
  (MUD f) >>= g = MUD $ \s ->
    let (t, x) = f s
        MUD h  = g x
     in h t
  return x = MUD $ \s -> (s, x)

instance MonadFix MUD where
  -- What on earth have I written here? But it works!
  -- mfix :: (a -> MUD a) -> MUD a
  mfix f = MUD $ \s ->
    let (MUD g) = (f . snd . g) s in g s

updateState :: (MudState -> MudState) -> MUD ()
updateState f = MUD $ \s -> (f s, ())

queryState :: (MudState -> a) -> MUD a
queryState q = MUD $ \s -> (s, q s)

initState :: MudState
initState = MudState empty empty [0..] [] []

-- Sadly, we cannot pass fields as function arguments.
-- updateField :: (MudState -> a) -> (a -> a) -> MUD ()
-- updateField field f = updateState $ \s -> s { field = f (field s) }

updateHooks :: (IntMap Hook -> IntMap Hook) -> MUD ()
updateHooks f = updateState $ \s -> s { hooks = f (hooks s) }

updateVars :: (IntMap Value -> IntMap Value) -> MUD ()
updateVars f = updateState $ \s -> s { vars = f (vars s) }

addResult :: Result -> MUD ()
addResult r = updateState $ \s -> s { results = results s ++ [r] }




-- Hooks

type Pattern = String
data Hook = Hook
  { hid     :: Int
  , channel :: Destination
  , pattern :: Pattern
  , action  :: MUD String
  }

mkId :: MUD Int
mkId = do
  i <- queryState (head . supply)
  updateState $ \s -> s { supply = tail (supply s) }
  return i

mkHook :: Destination -> Pattern -> MUD String -> MUD Hook
mkHook ch pat act = do
  hid <- mkId
  let hook = Hook hid ch pat act
  chHook hook
  return hook

chHook :: Hook -> MUD ()
chHook hook = updateHooks $ insert (hid hook) hook

rmHook :: Hook -> MUD ()
rmHook hook = updateHooks $ delete (hid hook)

allHooks :: MUD [Hook]
allHooks = queryState (elems . hooks)


-- Groups

group :: Int -> MUD String
group n = queryState ((!! n) . groups)

setGroups :: [String] -> MUD ()
setGroups gs = updateState $ \s -> s { groups = gs }


-- Hook derivatives

mkTrigger :: Pattern -> MUD () -> MUD Hook
mkTrigger pat act = mkHook Local pat (act >> group 0)

mkTriggerOnce :: Pattern -> MUD () -> MUD Hook
mkTriggerOnce pat act = mdo  -- whoo! recursive monads!
  hook <- mkTrigger pat (act >> rmHook hook)
  return hook

mkAlias :: Pattern -> String -> MUD Hook
mkAlias pat subst = mkHook Remote ("^" ++ pat ++ "($| )") $ do
  suffix <- group 1
  return (subst ++ suffix)


-- Variables

data Var a = Var Int
data Value = forall a. Value a

mkVar :: a -> MUD (Var a)
mkVar val = do
  i <- mkId
  setVar (Var i) val
  return (Var i)

setVar :: Var a -> a -> MUD ()
setVar (Var i) val = updateVars $ insert i (Value val)

readVar :: Var a -> MUD a
readVar (Var i) = do
  varmap <- queryState vars
  Value val <- lookup i varmap
  return (unsafeCoerce val)

updateVar :: Var a -> (a -> a) -> MUD ()
updateVar var f = readVar var >>= setVar var . f


-- I/O

-- Applies hooks, then sends the result to the client.
receive :: String -> MUD String
receive = trigger Local

-- Applies hooks, then sends the result to the server.
send :: String -> MUD String
send = trigger Remote

-- Like receive, but does not trigger hooks.
echo :: String -> MUD ()
echo = io Local

-- Like send, but does not trigger hooks.
echor :: String -> MUD ()
echor = io Remote

trigger :: Destination -> String -> MUD String
trigger ch message = do
    hs <- allHooks
    case filter ok hs of
      []       -> io ch message >> return message
      (hook:_) -> do
        subst <- fire message hook
        io ch subst
        return subst
  where
    ok hook = channel hook == ch && message =~ pattern hook

fire :: String -> Hook -> MUD String
fire message hook = do
    if null match
      then return before
      else do
        setGroups (match : groups)
        subst <- action hook
        rest  <- fire after hook
        return (before ++ subst ++ rest)
  where
    z@(before, match, after, groups) = message =~ pattern hook :: (String, String, String, [String])

io :: Destination -> String -> MUD ()
io ch message = addResult (Send ch message)
