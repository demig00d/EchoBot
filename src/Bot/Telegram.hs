{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
module Bot.Telegram where

import qualified Data.ByteString.Lazy.Char8 as L8

import           Bot.Types
import           Logging
import           Telegram.API
import           Telegram.Types


instance Bot TelegramEnv where
  type BotUpdate TelegramEnv = Update
  getUpdates   = undefined
  handleUpdate = undefined

data TelegramEnv =
  TelegramEnv
    { getToken :: String
    , offset   :: Int
    } deriving Show

-- | Check request environment and try to get Model from Config.
getModel :: Config -> IO (Either L8.ByteString (Model TelegramEnv))
getModel Config{..} = do
  logInfo' cLogLevel "Send request with 'getMe' method to check token."
  response <- getMe cToken
  case response of
    Left msg  -> do
      logDebug cLogLevel ("\n" <> msg)
      pure . Left  $ "Can't check token, request was unsuccessful."
    Right msg -> do
      logDebug cLogLevel ("\n" <> msg)
      logInfo' cLogLevel "Bot has been found, token is valid."
      pure $ Right model
    where
      model = Model
                { mBotSettings   = cBotSettings
                , mPlatformEnv   = TelegramEnv cToken 0
                , mUsersSettings = []
                , mLogLevel      = cLogLevel
                }
