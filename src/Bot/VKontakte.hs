{-# LANGUAGE RecordWildCards #-}
module Bot.VKontakte where

import           Bot.Types
import           VKontakte.API


data VKontakteEnv =
  VKontakteEnv
    { rToken   :: String
    , rGroupId :: String
    , rServer  :: String
    , rKey     :: String
    , rTs      :: String
    }

getModel :: Config -> IO (Either String (Model VKontakteEnv))
getModel Config{cGroupId = Just groupId,..} = do
  serverKeyTs <- getLongPollServer cToken groupId
  case serverKeyTs of
    Left msg          -> pure $ Left msg
    Right serverKeyTs -> pure . Right $ model serverKeyTs
  where
    model serverKeyTs = Model
              { mBotSettings   = cBotSettings
              , mPlatformEnv   = platformEnv serverKeyTs
              , mUsersSettings = []
              , mLogLevel      = cLogLevel
              }
    platformEnv ServerKeyTs{..} =
      VKontakteEnv
        { rToken   = cToken
        , rGroupId = groupId
        , rServer  = server
        , rKey     = key
        , rTs      = ts
        }
