{-# LANGUAGE StrictData      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
module Bot.Types where

import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Map.Strict
import           Data.Text                  (Text)

import           Logging                    (Priority)
import           Utils                      (deriveManyJSON, dropPrefixOptions)


class Show env => Bot env where
  type BotIncome env

  getIncome    :: Model env -> IO (Either L8.ByteString (BotIncome env))
  handleIncome :: Model env -> BotIncome env -> IO (Model env)

  setRepeatNumber :: Int -> Int -> Model env -> Model env
  setRepeatNumber userId n model@Model{usersSettings=usersSettings} =
    model{ usersSettings = insert userId n usersSettings }


data Model env =
  Model
    { botSettings   :: BotSettings
    , platformEnv   :: env
    , usersSettings :: UserSettings
    , logLevel      :: Priority
    } deriving (Show, Eq)

data BotSettings =
  BotSettings
    { bHelpMessage     :: Text
    , bRepeatMessage   :: Text
    , bNumberOfRepeats :: Int
    } deriving (Show, Eq)

-- A Map from keys 'user_id' to 'number of repeats'.
type UserSettings = Map Int Int

data Config =
  Config
    { cBotSettings  :: BotSettings
    , cPlatformName :: String
    , cToken        :: String
    , cGroupId      :: Maybe String
    , cLogLevel     :: Priority
    } deriving (Show, Eq)

$(deriveManyJSON dropPrefixOptions
    [''Config
    ,''BotSettings
    ])

