module Connector where

import MUD
import System.IO
import Network
import Control.Concurrent

hInteract :: Handle -> (String -> String) -> IO ()
hInteract h f = hGetContents h >>= hPutStr h . f

connect :: String -> Int -> MUD () -> IO ()
connect host port mud = do
  putStrLn $ "Connecting to " ++ host ++ " port " ++ show port ++ "..."
  conn <- connectTo host (PortNumber (fromIntegral port))  
  vState <- newMVar (fst $ runMUD mud initState)
  forkIO $ handleSide (hGetLine conn) vState Local conn
  handleSide getLine vState Remote conn

handleSide :: IO String -> MVar State -> Channel -> Handle -> IO ()
handleSide readLine vState ch conn = loop where
  loop = do
    line <- readLine
    oldState <- takeMVar vState
    let newState = fst $ runMUD (trigger ch line) oldState
    newerState <- executeCommands conn newState
    putMVar vState newerState
    loop

executeCommands :: Handle -> State -> IO State
executeCommands h state = do
    sequence_ (map (executeCommand h) cs)
    return (state { commands = [] })
  where cs = commands state

executeCommand :: Handle -> Command -> IO ()
executeCommand h (Send ch msg) = do
  case ch of
    Local  -> putStrLn msg
    Remote -> do hPutStrLn h msg; hFlush h
