# Yogurt #

Yogurt is a MUD client for Haskell. Its features include:

  * hooks;
  * timers;
  * logging;
  * type-safe variables of arbitrary types;
  * dynamic reloading of Yogurt scripts without reconnecting/restarting;
  * built on top of Haskell, leveraging the language's full expressiveness.

Because Yogurt scripts are pure Haskell, all Haskell libraries and constructs are available, making Yogurt the ideal tool for both simple aliases and advanced bots. Think of plugging in one of the powerful parser libraries, or building a GUI on top of yogurt.

## Download and installation ##

Yogurt can be downloaded and installed through [cabal](http://www.haskell.org/cabal/):

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

## Documentation ##

  * [API](http://hackage.haskell.org/cgi-bin/hackage-scripts/package/Yogurt) (on hackage)
  * [QuickStart](QuickStart.md) (including examples)
  * [VersionHistory](VersionHistory.md)