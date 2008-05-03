module Engine where

import Core
import System.IO
import Network
import Control.Concurrent

connect :: String -> Int -> Mud () -> IO ()
connect host port mud = do
  -- Connect.
  putStrLn $ "Connecting to " ++ host ++ " port " ++ show port ++ "..."
  h <- connectTo host (PortNumber (fromIntegral port))

  -- Create shared mud state.
  vState <- newMVar (fst $ runMud mud emptyMud)

  -- Start child threads.
  let localInput  = maybeInput $ fmap (++ "\n") getLine
  let remoteInput = maybeInput $ hGetImpatientLine h 50
  let handleS = handleSource vState (executeResults h)
  forkIO $ handleS localInput Remote
  handleS remoteInput Local

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
        let (newState, results) = runMud (trigger dest message >> flushResults) oldState
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
        Local  -> putStr msg
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
