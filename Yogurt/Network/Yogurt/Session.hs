{-# LANGUAGE DeriveDataTypeable #-}

-- | 
-- Sessions are used by Yogurt's standalone executable @yogurt@; see package @Yogurt-Standalone@ on hackage.
--
-- Every Yogurt file loaded by @yogurt@ should define a value of type 'Session'. For future compatibility, such a session is best defined using 'session' as starting value:
--
-- > import Network.Yogurt
-- >
-- > newmoon :: Session
-- > newmoon = session
-- >   { hostName   = "eclipse.cs.pdx.edu"
-- >   , portNumber = 7680
-- >   , mudProgram = \reload -> do
-- >       mkCommand "reload" reload
-- >   }
-- 
-- A module is free to define multiple sessions, in which case you will have to tell @yogurt@ which session to load.
module Network.Yogurt.Session (
    Session(..), Reload, session
  ) where

import Network.Yogurt.Mud
import Data.Generics (Typeable)
import Control.Monad
import Data.List (elemIndices)

-- | Describes a MUD session.
data Session = Session
  { -- | The hostname to connect to.
    hostName    :: String
    -- | The port to connect to.
  , portNumber  :: Int
    -- | The initial program to run. The Reload argument provides a way to
    --   reload the plugin without interrupting the MUD connection.
  , mudProgram  :: Reload -> Mud ()
  }
  deriving Typeable

-- | When executed, reloads the session from disk without interrupting the MUD connection. If the reloaded session contains no errors, all hooks are uninstalled before the reloaded program is executed. Timers are /not/ stopped and previous variables will still be reachable if you still have their handles.
type Reload = Mud ()

-- | Starting value for sessions. The default 'mudProgram' is @return ()@. There are no default values for 'hostName' and 'portNumber'.
session :: Session
session = Session
  { hostName    = error "session hostName not set"
  , portNumber  = error "session portNumber not set"
  , mudProgram  = const (return ())
  }
