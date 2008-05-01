{-# OPTIONS_GHC -XRecursiveDo #-}

module MUD where

import Data.IntMap (IntMap, empty, insert, delete)
import Control.Monad.Fix


-- The MUD monad.

data MUD a = MUD (State -> (State, a))

data State = State
  { hooks    :: IntMap Hook
  , nextHid  :: Int
  , groups   :: [String]
  , commands :: [Command]
  }

data Command = Send String | Echo String

instance Monad MUD where
  f >>= g = undefined
  return = undefined

instance MonadFix MUD where
  -- mfix :: (a -> MUD a) -> MUD a
  -- What on earth have I written here? Does it work?
  mfix f = MUD $ \s ->
    let (MUD g) = (f . snd . g) s in g s

updateState :: (State -> State) -> MUD ()
updateState f = MUD $ \s -> (f s, ())

queryState :: (State -> a) -> MUD a
queryState q = MUD $ \s -> (s, q s)


-- Hooks

data Channel = User | Remote
type Pattern = String
data Hook = Hook
  { hid     :: Int
  , channel :: Channel
  , pattern :: Pattern
  , action  :: MUD String
  }

incHid :: MUD Int
incHid = do
  h <- queryState nextHid
  updateState $ \s -> s { nextHid = h + 1 }
  return h

-- Sadly, we cannot pass fields as function arguments.
-- updateField :: (State -> a) -> (a -> a) -> MUD ()
-- updateField field f = updateState $ \s -> s { field = f (field s) }

updateHooks :: (IntMap Hook -> IntMap Hook) -> MUD ()
updateHooks f = updateState $ \s -> s { hooks = f (hooks s) }

mkHook :: Channel -> Pattern -> MUD String -> MUD Hook
mkHook ch pat act = do
  hid <- incHid
  let hook = Hook hid ch pat act
  chHook hook
  return hook

chHook :: Hook -> MUD ()
chHook hook = updateHooks $ insert (hid hook) hook

rmHook :: Hook -> MUD ()
rmHook hook = updateHooks $ delete (hid hook)


-- Groups

group :: Int -> MUD String
group n = queryState ((!! n) . groups)

setGroups :: [String] -> MUD ()
setGroups gs = updateState $ \s -> s { groups = gs }



-- Hook derivatives


mkTrigger :: Pattern -> MUD () -> MUD Hook
mkTrigger pat act = mkHook Remote pat (act >> group 0)

mkTriggerOnce :: Pattern -> MUD () -> MUD Hook
mkTriggerOnce pat act = mdo
  hook <- mkTrigger pat (act >> rmHook hook)
  return hook

mkAlias :: Pattern -> String -> MUD Hook
mkAlias pat subst = mkHook User ("^" ++ pat ++ "($| )") $ do
  suffix <- group 1
  return (subst ++ suffix)



-- Variables


data Var a = Var

mkVar :: a -> MUD (Var a)
mkVar = undefined

setVar :: Var a -> a -> MUD ()
setVar = undefined

readVar :: Var a -> MUD a
readVar = undefined



-- Matching hooks


-- Applies hooks, then sends the result to the client.
receive :: String -> MUD String
receive = undefined

-- Applies hooks, then sends the result to the server.
send :: String -> MUD String
send = undefined

-- Like receive, but does not trigger hooks.
echo :: String -> MUD ()
echo = undefined

-- Like send, but does not trigger hooks.
recho :: String -> MUD ()
recho = undefined

fire :: String -> Hook -> MUD String
fire = undefined
