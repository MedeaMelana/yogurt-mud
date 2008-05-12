{-# OPTIONS_GHC -fglasgow-exts #-}

module Yogurt.Utils
  ( mkTrigger, mkTriggerOnce
  , mkAlias, mkArgAlias, mkCommand
  , matchMore
  , receive, sendln, echo, echoln, echorln
  , system
  , module Yogurt.Core
  ) where

import Yogurt.Core
import qualified System.Cmd as Cmd

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

-- Todo: this is only really useful with priorities.
matchMore :: Mud ()
matchMore = do
  h <- triggeredHook
  m <- matchedLine
  rmHook h
  trigger (destination h) m
  chHook h

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

system :: String -> Mud ()
system command = runIO (Cmd.system command >> return ())
