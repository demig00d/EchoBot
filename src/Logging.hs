{-# LANGUAGE TemplateHaskell #-}
module Logging where

import           Utils (deriveJSON)


data Priority
  = Debug
  | Info
  | Warn
  | Error
  | Fatal
  deriving (Show, Eq, Ord)


$(deriveJSON ''Priority)
