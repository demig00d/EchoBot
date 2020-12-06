{-# LANGUAGE RecordWildCards #-}
module Bot.Telegram where

import           Bot.Types
import           Telegram.API


newtype TelegramEnv = TelegramEnv {getToken :: String}

getModel :: Config -> IO (Either String (Model TelegramEnv))
getModel Config{..} = do
  response <- getMe cToken
  case response of
    Left msg -> pure . Left  $ "Telegram bot " <> msg
    _        -> pure . Right $ model
  where
    model = Model
              { mBotSettings   = cBotSettings
              , mPlatformEnv   = TelegramEnv cToken
              , mUsersSettings = []
              , mLogLevel      = cLogLevel
              }
