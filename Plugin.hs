module Main where

import System.Plugins
import Utils

main :: IO ()
main = do
  status <- make "NewMoon.hs" []
  obj <- case status of
    MakeSuccess _ o -> return o
    MakeFailure e   -> mapM_ putStrLn e >> error "failed"
  
  m_v <- load obj ["."] [] "newmoon"
  val <- case m_v of
    LoadSuccess _ v -> return v
    _               -> error "load failed"
  print status
