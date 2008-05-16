module Yogurt.Engine (connect) where

import Yogurt.Mud
import System.IO
import Network
import Control.Concurrent
import Control.Monad.State
import System.Console.Readline
import Yogurt.IO
import Data.Char (isSpace)


-- Data that is manipulated and passed around during execution.
type Environment = (Handle, MVar MudState)


-- | Connects to a MUD and executes the specified program.
connect :: String -> Int -> Mud () -> IO ()
connect host port mud = do
  -- Connect.
  putStrLn $ "Connecting to " ++ host ++ " port " ++ show port ++ "..."
  h <- connectTo host (PortNumber (fromIntegral port))

  -- Create shared mud state, executing initial commands.
  vState <- newMVar (execState mud emptyMud)

  -- Start child threads.
  let env = (h, vState)
  forkIO (handleSource env localInput Remote)
  handleSource env (remoteInput h) Local


-- Watches an input source and updates the mud state whenever a new message arrives.
handleSource :: Environment ->        -- to run mud computations
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
  case maybeLine of
    Nothing   -> return Nothing
    Just line -> do
      when (not $ all isSpace line) (addHistory line)
      return (Just $ line ++ "\n")


-- Remote input using a connection handle.
remoteInput :: Handle -> IO (Maybe String)
remoteInput h = do
  input <- maybeInput (hGetImpatientLine h 50)
  return input


-- Runs a Mud computation, executes the results and returns the computation's result.
runMud :: Environment -> Mud a -> IO a
runMud env@(_, vState) prog = do
  s1 <- takeMVar vState
  let (rv, s2) = runState prog s1
  let (rs, s3) = runState flushResults s2
  putMVar vState s3
  executeResults env rs
  return rv


-- Executes results in sequence.
executeResults :: Environment -> [Result] -> IO ()
executeResults env = sequence_ . map (executeResult env)


-- Executes one result.
executeResult :: Environment -> Result -> IO ()
executeResult env@(h, _) res = case res of

    Send ch msg ->
      case ch of
        Local  -> writeToTTY msg
        Remote -> do hPutStr h msg; hFlush h

    RunIO io actf -> do
      x <- io
      runMud env (actf x)

    NewTimer timer interval -> do
      forkIO (runTimer env timer interval)
      return ()


-- Called whenever a new timer is created.
runTimer :: Environment -> Timer -> Interval -> IO ()
runTimer env@(h, vState) timer interval = loop where
  loop = do
    -- Sleep.
    threadDelay (1000 * interval)  -- interval in ms, threadDelay expects micros

    -- Execute timer action only if timer hasn't been removed in the meantime.
    ok <- runMud env (existsTimer timer)
    when ok (runMud env $ tAction timer)

    -- Maybe the timer's action removed the timer. If not, run again.
    again <- runMud env (existsTimer timer)
    when again loop
