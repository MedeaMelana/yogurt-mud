import Network.Yogurt
import Network.Yogurt.Readline

import System.Exit
import System.Environment
import System.Console.GetOpt
import System.IO
import Data.List
import Data.Function
import Data.Version
import Control.Monad
import Control.Monad.Trans
import qualified Paths_Yogurt_Standalone as P

import Language.Haskell.Interpreter

options :: [OptDescr (IO ())]
options =
  [ Option "v" ["version"] (NoArg (printVersion >> exitSuccess)) "print version and exit"
  , Option "h" ["help"]    (NoArg (printHelp    >> exitSuccess)) "print help and exit"
  ]

main :: IO ()
main = do
  (flags, otherArgs, errs) <- getOpt RequireOrder options `liftM` getArgs
  let flatErrs = filter (not . null) $ concatMap (lines . indent) errs

  unless (null flatErrs) $ do
    errLn $ "Illegal arguments:"
    err   $ unlines flatErrs
    errLn $ "Try yogurt --help for help."
    exitFailure

  sequence_ flags
  case otherArgs of
    [moduleName] ->
      loadSession moduleName pickDefaultSession
    [moduleName, sessionName] ->
      loadSession moduleName (pickSession sessionName)
    _ ->
      printUsage >> exitFailure

-- moduleName -> available session names -> session name to load
type PickSession = String -> [String] -> IO String

pickDefaultSession :: PickSession
pickDefaultSession moduleName sessionNames = do
  case sessionNames of
    [] -> do
      errLn $ "Module " ++ moduleName ++ " defines no sessions."
      exitFailure
    [sessionName] -> do
      return sessionName
    _ -> do
      errLn $ "Module " ++ moduleName ++ " defines several sessions: " ++ intercalate ", " sessionNames
      errLn $ "Use \"yogurt " ++ moduleName ++ " <session>\" to pick a specific session."
      exitFailure

pickSession :: String -> PickSession
pickSession sessionName moduleName sessionNames = do
  if sessionName `elem` sessionNames
    then return sessionName
    else do
      errLn $ "Module " ++ moduleName ++ " defines no session called \"" ++ sessionName ++ "\"."
      exitFailure

indent :: String -> String
indent = unlines . map ("  " ++ ) . lines

loadSession :: String -> PickSession -> IO ()
loadSession moduleName pick = do
  errLn $ "Loading module " ++ moduleName ++ "..."
  mSessions <- loadPlugin moduleName
  case mSessions of
    Left e -> do
      errLn (pretty e)
      exitFailure
    Right sessions -> do
      sessionName <- pick moduleName (map fst sessions)
      let Just session = lookup sessionName sessions
      let doReload = reload moduleName sessionName
      connect (hostName session) (portNumber session) (mudProgram session doReload)

printUsage :: IO ()
printUsage = do
  errLn $ "Usage: yogurt <module> [<session>]"
  errLn $ "   or: yogurt --help"

printVersion :: IO ()
printVersion = do
  errLn $ "Yogurt version " ++ showVersion P.version
  errLn $ "Using version " ++ showVersion version ++ " of the Yogurt library"
  errLn $ "Some Rights Reserved (CC) 2008-2009 Martijn van Steenbergen"
  errLn $ "http://martijn.van.steenbergen.nl/projects/yogurt/"

printHelp :: IO ()
printHelp = do
  printVersion
  errLn $ ""
  errLn $ "Usage: yogurt <module> [<session>]"
  errLn $ ""
  err   $ usageInfo "Available options:" options

err :: String -> IO ()
err = hPutStr stderr

errLn :: String -> IO ()
errLn = hPutStrLn stderr

pretty :: InterpreterError -> String
pretty e = case e of
  UnknownError s -> s
  WontCompile ss -> intercalate "\n\n" (map errMsg ss)
  NotAllowed s -> s
  GhcException s -> s

reload :: String -> String -> Mud ()
reload moduleName sessionName = fix $ \loop -> do
  echoln $ "Loading module " ++ moduleName ++ "..."
  mSessions <- lift $ loadPlugin moduleName
  case mSessions of
    Left e -> echoln (pretty e)
    Right sessions -> do
      case lookup sessionName sessions of
        Nothing -> do
          echoln $ "Module " ++ moduleName ++
                      " no longer contains a session called \"" ++ sessionName ++ "\"."
        Just session -> do
          mapM_ rmHook =<< allHooks
          mudProgram session loop
          echoln "Done."

-- | Given a module name, yields all sessions and their names defined in that module.
loadPlugin :: String -> IO (Either InterpreterError [(String, Session)])
loadPlugin mn = runInterpreter $ do
  let moduleName = case elemIndices '.' mn of
        []  -> mn
        is  -> take (last is) mn
  loadModules [moduleName]
  loadedModuleNames <- getLoadedModules
  if not (moduleName `elem` loadedModuleNames)
    then do
      fail "The module's name must match the filename."
    else do
      setImports ["Network.Yogurt.Session", moduleName]
      symbols <- map name `liftM` getModuleExports moduleName
      typedSymbols <- mapM (\s -> (,) `liftM` return s `ap` typeOf s) symbols
      let sessionNames = [ n | (n, t) <- typedSymbols, t == "Session" ]
      forM sessionNames $ \sn -> do
        session <- interpret sn (as :: Session)
        return (sn, session)
