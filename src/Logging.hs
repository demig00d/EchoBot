{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Logging where

import qualified Data.ByteString.Char8      as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Text                  (Text)
import           Data.Text.Encoding         (encodeUtf8)
import           Prelude                    hiding (log)

import           Utils                      (deriveJSON)


data Priority
  = Debug
  | Info
  | Warning
  | Error
  deriving (Show, Eq, Ord)

$(deriveJSON ''Priority)


class ToLogStr msg where
  toLogStr :: msg -> S8.ByteString

instance ToLogStr S8.ByteString where
    {-# INLINE toLogStr #-}
    toLogStr = id
instance ToLogStr L8.ByteString where
    {-# INLINE toLogStr #-}
    toLogStr = L8.toStrict
instance ToLogStr String where
    {-# INLINE toLogStr #-}
    toLogStr = S8.pack
instance ToLogStr Text where
    {-# INLINE toLogStr #-}
    toLogStr = encodeUtf8


log :: ToLogStr msg => Priority -> Priority -> msg -> IO ()
log msgLvl appLvl msg =
  if msgLvl >= appLvl
     then S8.putStrLn $ "[" <> (S8.pack $ show msgLvl) <> "] " <> toLogStr msg
     else return ()

logDebug, logInfo, logWarning :: ToLogStr msg => Priority -> msg -> IO ()
logDebug   = log Debug
logInfo    = log Info
logWarning = log Warning

logError msg = S8.putStrLn $ "[Error] " <> toLogStr msg

-- | Logging functions that don't require type annotation
-- for message, when OverloadedStrings pragma is enabled.
logDebug', logInfo', logWarning' :: Priority -> S8.ByteString -> IO ()
logDebug'   = log Debug
logInfo'    = log Info
logWarning' = log Warning

logError' msg = S8.putStrLn $ "[Error] " <> msg
