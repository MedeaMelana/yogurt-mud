module Engine where

import Core
import System.IO
import Network
import Control.Concurrent
import Control.Monad.State
import System.Console.Readline
import Data.Maybe (isJust)
import Data.List (elemIndices)
import WriteToTTY

connect :: String -> Int -> Mud () -> IO ()
connect host port mud = do
  -- Connect.
  putStrLn $ "Connecting to " ++ host ++ " port " ++ show port ++ "..."
  h <- connectTo host (PortNumber (fromIntegral port))

  -- Create shared mud state, executing initial commands.
  vState <- newMVar (execState mud emptyMud)

  -- Start child threads.
  let handleS = handleSource vState (executeResults h)
  forkIO $ handleS localInput Remote
  handleS (remoteInput h) Local

localInput :: IO (Maybe String)
localInput = do
  maybeLine <- readline ""
  case maybeLine of
    Nothing   -> return Nothing
    Just line -> do
      addHistory line
      return (Just $ line ++ "\n")

remoteInput :: Handle -> IO (Maybe String)
remoteInput h = do
  input <- maybeInput (hGetImpatientLine h 50)
  return input

-- Takes an input method and catches errors, returning results in the Maybe monad.
maybeInput :: IO String -> IO (Maybe String)
maybeInput input = fmap Just input `catch` const (return Nothing)

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

executeResults :: Handle -> [Result] -> IO ()
executeResults h rs = do
    sequence_ (map (executeResult h) rs)

executeResult :: Handle -> Result -> IO ()
executeResult h res = do
  debug (show res)
  case res of
    Send ch msg ->
      case ch of
        Local  -> writeToTTY msg
        Remote -> do hPutStr h msg; hFlush h
    RunIO io ->
      io

hGetImpatientLine :: Handle -> Int -> IO String
hGetImpatientLine h patience = rec where
  rec = do
    c <- hGetChar h
    if c == '\n'
      then return [c]
      else do
        b <- hWaitForInput h patience
        if b
          then rec >>= return . (c:)
          else return [c]

debug :: String -> IO ()
debug = appendFile "debug.log" . (++ "\n")
