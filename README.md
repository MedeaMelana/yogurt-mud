# Yogurt

Yogurt is a MUD client for Haskell. Its features include:

* hooks;
* timers;
* logging;
* type-safe variables of arbitrary types;
* dynamic reloading of Yogurt scripts without reconnecting/restarting;
* built on top of Haskell, leveraging the language's full expressiveness.

Because Yogurt scripts are pure Haskell, all Haskell libraries and constructs are available, making Yogurt the ideal tool for both simple aliases and advanced bots. Think of plugging in one of the powerful parser libraries, or building a GUI on top of yogurt.

## Download and installation

Yogurt can be downloaded and installed through [http://www.haskell.org/cabal/ cabal]:

```
$ cabal update
$ cabal install Yogurt -freadline
$ cabal install Yogurt-Standalone
```

Or, if you want just the libraries and don't need the executable:

```
$ cabal update
$ cabal install Yogurt
```

## Documentation

  * [API](http://hackage.haskell.org/cgi-bin/hackage-scripts/package/Yogurt) (on hackage)

## A few examples to get you started

All examples are also available in the download from hackage.

### Minimal example

After installing Yogurt, create a file called Minimal.hs and put the following text in it:

```
module Minimal where

import Network.Yogurt

newmoon :: Session
newmoon = session
  { hostName   = "eclipse.cs.pdx.edu"
  , portNumber = 7680
  }
```

This is the smallest example: a single session specifying the host and port to connect to, but no scripting.

Then start Yogurt by issuing:

```
$ yogurt Minimal
```

### A more involved example

The following example demonstrates some of the features Yogurt boasts.

```
module NewMoon where

import Network.Yogurt
import System.Cmd
import Data.Char

newmoon :: Session
newmoon = session
  { hostName = "eclipse.cs.pdx.edu"
  , portNumber = 7680
  , mudProgram = \reload -> do
      -- The reload argument allows reloading this Haskell file during a session.
      -- Let's make a command for it so we can call it while playing.
      mkCommand "reload" reload
      installHooks      
  }

installHooks :: Mud ()
installHooks = do
  -- Logs are stored in a timestamped file in the local directory.
  startLogging "NewMoon"

  -- Log in automatically.
  mkTriggerOnce "^Enter your name:" $ do
    sendln "username"
    sendln "password"

  -- Sound system bell whenever you receive a tell.
  mkTrigger "^[^ ]+ tells you: " bell

  -- Send a carriage return every 5 minutes to keep the connection alive.
  -- Most MUDs don't like this...
  mkTimer (5 * 60 * 1000) (sendln "")

  -- Enable calling system commands during a session.
  mkCommand "system" $ do
    group 1 >>= liftIO . system
    return ()

  -- | Split commands in two on semicolons.
  mkPrioHook 100 Remote ";" $ do
    before >>= matchMoreOn  . (++ "\n")
    after  >>= matchMoreOn'

  -- Enable speedwalks, e.g. "5w" expands to "w;w;w;w;w"
  mkHook Remote "^[0-9]+[neswud]$" $ do
    (n, dir) <- fmap (span isDigit) (group 0)
    sequence $ replicate (read n) (sendln dir)
  
  -- A very simple alias: make "fb" expand to "cast fireball"
  mkAlias "fb" "cast fireball"
  
  -- Create a command to teleport to a specific place.
  mkCommand "tp" $ do
    -- Find out where to go.
    dest <- group 1
    if all isSpace dest
      then do
        echoln "Teleport to where?"
      else do
        sendln "cast teleport"
  
        -- Wait for the spell to get ready...
        mkTriggerOnce "^You feel ready to teleport now" $ do
          sendln ("teleport to" ++ dest)
        return ()
  
  -- Show all currently install hooks whenever "hooks" is entered.
  mkCommand "hooks" (allHooks >>= echo . unlines . map show)

  return ()
```

## Version history

### 0.4.1 — Saturday 18 June 2011 ==

  * Fixed a compiler error introduced in GHC 7.

### 0.4 — Friday 10 April 2009 ==

  * Yogurt has become a standalone executable and is able to dynamically load and reload Yogurt scripts.

### 0.3 — Monday 16 February 2009 ==

  * Fixes issues with GHC 6.10. 
  * Mud now has IO as underlying monad.
  * Forking is supported, with callback into the Mud monad.
  * Timers are no longer a primitive and have been moved to the Utils module.
  * Variables are expressed as IORefs.
  * Collecting results is no longer needed—output is part of the state now.

### 0.2 — Thursday 29 May 2008 ==

Immediate follow-up to fix build error on hackage.

### 0.1 — Wednesday 28 May 2008 ==

Initial publication!
