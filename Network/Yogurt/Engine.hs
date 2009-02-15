module Network.Yogurt.Engine (connect, Environment, Output, runMud) where

import Network.Yogurt.Mud
import System.IO
import Network
import Control.Concurrent
import Control.Monad.State
import System.Console.Readline
import Network.Yogurt.IO
import Data.Char (isSpace)
import System.Process


-- | Used by 'runMud' to output messages and update the state during execution.
type Environment = (Output, MVar MudState)

-- | Provides a way to output messages.
type Output = Destination -> String -> IO ()

-- | @connect hostname port program@ connects to a MUD and executes the specified program. Input is read from @stdin@, and output is written to @stdout@.
connect :: String -> Int -> Mud () -> IO ()
connect host port mud = do
  -- Connect.
  putStrLn $ "Connecting to " ++ host ++ " port " ++ show port ++ "..."
  h <- connectTo host (PortNumber (fromIntegral port))

  -- Create shared mud state, executing initial commands.
  state0 <- execStateT mud emptyMud
  vState <- newMVar state0

  -- Start child threads.
  let out ch msg = case ch of
        Local  -> writeToTTY msg
        Remote -> do hPutStr h msg; hFlush h
  let env = (out, vState)
  forkIO (handleSource env localInput Remote)
  handleSource env (remoteInput h) Local
  runCommand "stty echo" >>= waitForProcess
  return ()

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
runMud :: Environment -> Mud a -> IO a
runMud env@(_, vState) prog = do
  s1 <- takeMVar vState
  (rv, s2) <- runStateT prog s1
  (rs, s3) <- runStateT flushResults s2
  putMVar vState s3
  executeResults env rs
  return rv


-- Executes results in sequence.
executeResults :: Environment -> [Result] -> IO ()
executeResults env = sequence_ . map (executeResult env)


-- Executes one result.
executeResult :: Environment -> Result -> IO ()
executeResult env@(out, _) res = case res of

    Send ch msg -> do
      -- debug $ "Send " ++ show ch ++ " " ++ show msg
      out ch msg

    -- RunIO io actf -> do
    --   -- debug "RunIO"
    --   x <- io
    --   runMud env (actf x)

    NewTimer timer -> do
      -- debug "NewTimer"
      forkIO (runTimer env timer)
      return ()


-- Called whenever a new timer is created.
runTimer :: Environment -> Timer -> IO ()
runTimer env timer = loop where
  loop = do
    -- Sleep.
    threadDelay (1000 * tInterval timer)  -- interval in ms, threadDelay expects micros

    -- Execute timer action only if timer hasn't been removed in the meantime.
    ok <- runMud env (existsTimer timer)
    when ok (runMud env $ tAction timer)

    -- Maybe the timer's action removed the timer. If not, run again.
    again <- runMud env (existsTimer timer)
    when again loop

debug :: String -> IO ()
debug = appendFile "debug.log" . (++ "\n")
