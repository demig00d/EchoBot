{-# LANGUAGE StrictData      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
module Bot.Types where

import           Control.Monad (foldM)
import           Data.Text     (Text)

import           Logging       (Priority)
import           Utils         (deriveManyJSON)


class Show env => Bot env where
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
    } deriving Show

data BotSettings =
  BotSettings
    { bHelpMessage     :: Text
    , bRepeatMessage   :: Text
    , bNumberOfRepeats :: Int
    } deriving Show

data UserSettings =
  UserSettings
    { uId              :: Int
    , uNumberOfRepeats :: Int
    } deriving Show

data Config =
  Config
    { cBotSettings  :: BotSettings
    , cPlatformName :: String
    , cToken        :: String
    , cGroupId      :: (Maybe String)
    , cLogLevel     :: Priority
    } deriving Show

$(deriveManyJSON
    [''Config
    ,''BotSettings
    ])

