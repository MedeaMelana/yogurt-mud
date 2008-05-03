module Main where

import Mud
import Control.Monad (liftM)
import Connector

main :: IO ()
main = connect "darkover.isilm.com" 5000 load

load :: Mud ()
load = do
  mkSpellAliases
  --recordXpDelta
  trackHp
{-  mkTrigger "Welcome to Darkover!" $ do
    echo "hello!"
    send ""
    send "medea"
    send "rildrikjan11"
    return ()-}
  aap <- mkVar 0
  mkHook Remote "aap" $ do
    updateVar aap (+ 1)
    a <- readVar aap
    echo (show a)
    return "aap"
  mkTrigger "OOOOOOOOOOOOOO" $ do
    echo "match"
  return ()

mkSpellAliases = sequence $ map mkFull spells where
  mkFull (ab, full) = mkAlias ab ("cast '" ++ full ++ "'")

spells :: [(String, String)]
spells =
  [ ("ia", "ice arrow")
  , ("bl", "blindness")
  , ("sg", "shocking grasp")
  ]

recordXpDelta = do
  xpEarned <- mkVar 0
  xpDelta  <- mkVar 0
  mkTrigger "^Experience Earned: [(.*)]" $ do
    oldXpEarned <- readVar xpEarned
    newXpEarned <- liftM read (group 1)
    let newXpDelta = newXpEarned - oldXpEarned
    if oldXpEarned /= newXpEarned
      then setVar xpDelta (newXpEarned - oldXpEarned)
      else return ()
    setVar xpEarned newXpEarned
    echo ("Experience gained since last score: [" ++ show newXpDelta ++ "]")

trackHp = do
  hpMax <- mkVar 0
  hpCur <- mkVar 0
  mkTrigger "^< (.*)hp (.*)m (.*)mv >" $ do
    echo "matched prompt"
    newHpCur <- liftM read (group 1)
    setVar hpCur newHpCur
  mkHook Remote "^hp$" $ do
    hp <- readVar hpCur
    echo ("current hp: " ++ show hp)
    return ""