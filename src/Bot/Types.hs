{-# LANGUAGE StrictData      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
module Bot.Types where

import           Control.Monad (foldM)
import           Data.Text     (Text)

import           Logging       (Priority)
import           Utils         (deriveManyJSON)


class Bot env where
  type BotUpdate env
  getUpdates    :: env -> IO (Either String [BotUpdate env])
  handleUpdate  :: Model env -> BotUpdate env -> IO (Model env)

  handleUpdates :: Model env -> [BotUpdate env] -> IO (Model env)
  handleUpdates model updates = foldM handleUpdate model updates

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

