module Minimal where

import Network.Yogurt

newmoon :: Session
newmoon = session
  { hostName   = "eclipse.cs.pdx.edu"
  , portNumber = 7680
  }
