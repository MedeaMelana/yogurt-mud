module Main where

import MUD
import Control.Monad (liftM)
import Connector

main :: IO ()
main = connect "darkover.isilm.com" 5000 load

load :: MUD ()
load = do
  --mkSpellAliases
  --recordXpDelta
  --trackHp
  mkTrigger "Welcome to Darkover!" $ do
    echo "hello!"
    send ""
    return ()
  mkTriggerOnce "OOOOOOOOOOOOOO" $ do
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
    newHpCur <- liftM read (group 1)
    setVar hpCur newHpCur
