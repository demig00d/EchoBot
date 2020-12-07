{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}
module Bot.Telegram where

import           Bot.Types
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

getModel :: Config -> IO (Either String (Model TelegramEnv))
getModel Config{..} = do
  response <- getMe cToken
  case response of
    Left msg -> pure . Left  $ "Telegram bot " <> msg
    _        -> pure . Right $ model
  where
    model = Model
              { mBotSettings   = cBotSettings
              , mPlatformEnv   = TelegramEnv cToken 0
              , mUsersSettings = []
              , mLogLevel      = cLogLevel
              }
