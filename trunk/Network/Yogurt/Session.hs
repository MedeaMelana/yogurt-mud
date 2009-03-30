{-# LANGUAGE DeriveDataTypeable #-}

-- | Every Yogurt file should define a function @main@ of type 'Session'. For future compatibility, such a session is best defined using 'session' as starting value:
--
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
    Session(..), Reload, session, loadPlugin
  ) where

import Network.Yogurt.Mud
import Data.Generics (Typeable)
import Language.Haskell.Interpreter
import Control.Monad

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

-- | When executed, reloads the session from disk without interrupting the MUD connection.
type Reload = Mud ()

-- | Starting value for sessions. The default 'mudProgram' is @return ()@. There are no default values for 'hostName' and 'portNumber'.
session :: Session
session = Session
  { hostName    = error "session hostName not set"
  , portNumber  = error "session portNumber not set"
  , mudProgram  = const (return ())
  }

-- | Given a module name, yields all sessions and their names defined in that module.
loadPlugin :: String -> IO (Either InterpreterError [(String, Session)])
loadPlugin moduleName = runInterpreter $ do
  loadModules [moduleName]
  loadedModuleNames <- getLoadedModules
  if not (moduleName `elem` loadedModuleNames)
    then do
      fail "The module's name must match the filename."
    else do
      setImports ["Prelude", "Network.Yogurt.Session", moduleName]
      symbols <- map name `liftM` getModuleExports moduleName
      typedSymbols <- mapM (\s -> (,) `liftM` return s `ap` typeOf s) symbols
      let sessionNames = [ n | (n, t) <- typedSymbols, t == "Session" ]
      forM sessionNames $ \sn -> do
        session <- interpret sn (as :: Session)
        return (sn, session)
