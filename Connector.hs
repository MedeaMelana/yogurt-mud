module Connector where

import MUD
import System.IO
import Network
import Control.Concurrent

hInteract :: Handle -> (String -> String) -> IO ()
hInteract h f = hGetContents h >>= hPutStr h . f

connect :: String -> Int -> MUD () -> IO ()
connect host port mud = do
  conn <- connectTo host (PortNumber (fromIntegral port))
  
  vState <- newMVar (fst $ runMUD mud initState)

  forkIO $ do
    -- read from server
    line <- hGetLine conn
    oldState <- takeMVar vState
    let newState = fst $ runMUD (receive line) oldState
    newerState <- executeCommands conn newState
    putMVar vState newerState

  forkIO $ do
    -- read from tty
    line <- getLine
    oldState <- takeMVar vState
    let newState = fst $ runMUD (send line) oldState
    newerState <- executeCommands conn newState
    putMVar vState newerState

  return ()

executeCommands :: Handle -> State -> IO State
executeCommands h state = do
    sequence_ (map (executeCommand h) cs)
    return (state { commands = [] })
  where cs = commands state

executeCommand :: Handle -> Command -> IO ()
executeCommand h (Send ch msg) = case ch of
  Local  -> putStrLn msg
  Remote -> hPutStrLn h msg
