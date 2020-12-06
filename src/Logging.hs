{-# LANGUAGE TemplateHaskell #-}
module Logging where

import           Prelude hiding (log)

import           Utils   (deriveJSON)


data Priority
  = Debug
  | Info
  | Warning
  | Error
  deriving (Show, Eq, Ord)

$(deriveJSON ''Priority)


log :: Priority -> Priority -> String -> IO ()
log msgLvl appLvl msg =
  if msgLvl >= appLvl
     then putStrLn $ "[" <> show msgLvl <> "] " <> msg
     else return ()

logDebug, logInfo, logWarning :: Priority -> String -> IO ()
logDebug   = log Debug
logInfo    = log Info
logWarning = log Warning

logError = putStrLn . ("[Error] "<>)
