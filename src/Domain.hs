{-# LANGUAGE StrictData      #-}
{-# LANGUAGE TemplateHaskell #-}
module Domain where

import           Data.Text (Text)
import           Logging   (Priority)
import           Utils     (deriveManyJSON)


data Model env =
  Model
    { mBotSettings   :: BotSettings
    , mPlatformEnv   :: env
    , mUsersSettings :: [UserSettings]
    , mLogLevel      :: Priority
    }

data BotSettings =
  BotSettings
    { bHelpMessage     :: Text
    , bRepeatMessage   :: Text
    , bNumberOfRepeats :: Int
    }

data UserSettings =
  UserSettings
    { uId              :: Int
    , uNumberOfRepeats :: Int
    }

data Config =
  Config
    { cBotSettings  :: BotSettings
    , cPlatformName :: String
    , cToken        :: String
    , cGroupId      :: (Maybe String)
    , cLogLevel     :: Priority
    }

$(deriveManyJSON
    [''Config
    ,''BotSettings
    ])
