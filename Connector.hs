module Connector where

import MUD
import System.IO
import Network
import Control.Concurrent

connect :: String -> Int -> MUD () -> IO ()
connect host port mud = do
  putStrLn $ "Connecting to " ++ host ++ " port " ++ show port ++ "..."
  conn <- connectTo host (PortNumber (fromIntegral port))  
  vState <- newMVar (fst $ runMUD mud initState)
  forkIO $ handleSide (hGetImpatientLine conn 50) vState Local conn
  handleSide getLine vState Remote conn

handleSide :: IO String -> MVar MudState -> Destination -> Handle -> IO ()
handleSide readLine vState ch conn = loop where
  loop = do
    line <- readLine
    oldState <- takeMVar vState
    let newState = fst $ runMUD (trigger ch line) oldState
    newerState <- sendMessages conn newState
    putMVar vState newerState
    loop

sendMessages :: Handle -> MudState -> IO MudState
sendMessages h state = do
    sequence_ (map (sendMessage h) cs)
    return (state { results = [] })
  where cs = results state

sendMessage :: Handle -> Result -> IO ()
sendMessage h (Send ch msg) = do
  case ch of
    Local  -> putStr msg
    Remote -> do hPutStrLn h msg; hFlush h

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
