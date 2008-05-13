module Yogurt.Engine (connect) where

import Yogurt.Mud
import System.IO
import Network
import Control.Concurrent
import Control.Monad.State
import System.Console.Readline
import Yogurt.IO
import Data.Char (isSpace)

connect :: String -> Int -> Mud () -> IO ()
connect host port mud = do
  -- Connect.
  putStrLn $ "Connecting to " ++ host ++ " port " ++ show port ++ "..."
  h <- connectTo host (PortNumber (fromIntegral port))

  -- Create shared mud state, executing initial commands.
  vState <- newMVar (execState mud emptyMud)

  -- Start child threads.
  let handleS = handleSource vState (executeResults h vState)
  forkIO $ handleS localInput Remote
  handleS (remoteInput h) Local


-- Watches an input source and updates the mud state whenever a new message arrives.
handleSource :: MVar MudState ->        -- shared mud state
                ([Result] -> IO ()) ->  -- execution of results
                IO (Maybe String) ->    -- input source
                Destination ->          -- target destination
                IO ()
handleSource vState exec input dest = loop where
  loop = do
    mmessage <- input
    case mmessage of
      Nothing -> return ()
      Just message -> do
        oldState <- takeMVar vState
        let (results, newState) = runState (trigger dest message >> flushResults) oldState
        exec results
        putMVar vState newState
        loop

executeResults :: Handle -> MVar MudState -> [Result] -> IO ()
executeResults h vState rs = do
    sequence_ (map (executeResult h vState) rs)

executeResult :: Handle -> MVar MudState -> Result -> IO ()
executeResult h vState res = case res of
    Send ch msg ->
      case ch of
        Local  -> writeToTTY msg
        Remote -> do hPutStr h msg; hFlush h
    RunIO io actf -> do
      io
      return ()      
    NewTimer timer time ->
      spawnTimer vState timer time

spawnTimer :: MVar MudState -> Timer -> Int -> IO ()
spawnTimer vState t@(Timer ti act) time = forkIO loop >> return () where
  loop = do
    threadDelay time
    st1 <- takeMVar vState
    let ok = evalState (existsTimer t) st1
    let st2 = if ok then execState act st1 else st1
    putMVar vState st2
    let again = evalState (existsTimer t) st2
    when again loop


-- Input from tty and remote

localInput :: IO (Maybe String)
localInput = do
  maybeLine <- readline ""
  case maybeLine of
    Nothing   -> return Nothing
    Just line -> do
      when (not $ all isSpace line) (addHistory line)
      return (Just $ line ++ "\n")

remoteInput :: Handle -> IO (Maybe String)
remoteInput h = do
  input <- maybeInput (hGetImpatientLine h 50)
  return input
