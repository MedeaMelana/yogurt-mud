{-# LANGUAGE RecursiveDo #-}

module Network.Yogurt.Engine (connect, runMud) where

import Network.Yogurt.Mud
import Network.Yogurt.Session
import System.IO
import Network
import Control.Concurrent
import Control.Monad.State
import System.Console.Readline
import Network.Yogurt.IO
import Data.Char (isSpace)
import System.Process


-- | @connect hostname port program@ connects to a MUD and executes the specified program. Input is read from @stdin@, and output is written to @stdout@.
connect :: String -> Int -> Mud () -> IO ()
connect host port mud = mdo
  -- Connect.
  putStrLn $ "Connecting to " ++ host ++ " port " ++ show port ++ "..."
  h <- connectTo host (PortNumber (fromIntegral port))

  -- Create shared mud state, executing initial commands.
  let out ch msg = case ch of
        Local  -> writeToTTY msg
        Remote -> do hPutStr h msg; hFlush h
  let state0 = emptyMud (runMud vState) out
  state1 <- execStateT mud state0
  vState <- newMVar state1

  -- Start child threads.
  forkIO (handleSource vState localInput Remote)
  handleSource vState (remoteInput h) Local
  
  -- Clean.
  runCommand "stty echo" >>= waitForProcess
  return ()


-- Watches an input source and updates the mud state whenever a new message arrives.
handleSource :: MVar MudState ->      -- to run mud computations
                IO (Maybe String) ->  -- input source
                Destination ->        -- target destination
                IO ()
handleSource env input dest = loop where
  loop = do
    mmessage <- input
    case mmessage of
      Nothing -> return ()
      Just message -> do
        runMud env (trigger dest message)
        loop


-- Local input using readline.
localInput :: IO (Maybe String)
localInput = do
  maybeLine <- readline ""
  setLineBuffer ""
  case maybeLine of
    Nothing   -> return Nothing
    Just line -> do
      when (not $ all isSpace line) (addHistory line)
      return (Just $ line ++ "\n")


-- Remote input using a connection handle.
remoteInput :: Handle -> IO (Maybe String)
remoteInput h = do
  input <- maybeInput (hGetImpatientLine h 10)
  return input


-- | Runs a Mud computation, executes the results (such as sending messages to the screen or the MUD) and returns the computation's result. The MVar is updated.
runMud :: MVar MudState -> Mud a -> IO a
runMud vState prog = do
  s <- takeMVar vState
  (rv, s') <- runStateT prog s
  putMVar vState s'
  return rv
