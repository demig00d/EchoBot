{-# LANGUAGE StrictData      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
module Bot.Types where

import           Control.Monad              (foldM)
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Map.Strict
import           Data.Text                  (Text)

import           Logging                    (Priority, logInfo, logWarning)
import           Utils                      (deriveManyJSON)


class Show env => Bot env where
  type BotIncome env
  type BotUpdate env

  getIncome   :: Model env -> IO (Either L8.ByteString (BotIncome env))

  handleIncome :: Model env -> BotIncome env -> IO (Model env)
  handleIncome model income =
    case extractUpdates income model of
      Nothing      -> logWarning (logLevel model) "Can't get Updates." >> pure model
      Just []      -> logInfo (logLevel model) "Updates are empty."    >> pure model
      Just updates -> logInfo (logLevel model) "Handling updates."
              >> let model' = updateModel model income
              in foldM handleUpdate model' updates

  handleUpdate :: Model env -> BotUpdate env  -> IO (Model env)

  updateModel :: Model env -> BotIncome env -> Model env
  updateModel = const

  extractUpdates :: BotIncome env -> Model env -> Maybe [BotUpdate env]


data Model env =
  Model
    { botSettings   :: BotSettings
    , platformEnv   :: env
    , usersSettings :: UserSettings
    , logLevel      :: Priority
    } deriving Show

data BotSettings =
  BotSettings
    { bHelpMessage     :: Text
    , bRepeatMessage   :: Text
    , bNumberOfRepeats :: Int
    } deriving (Show, Eq)

-- A Map from keys 'user_id' to 'number of repeats'.
type UserSettings = Map Int Int

-- Auxiliary type for update handling
data MessageAction
  = ShowStart
  | ShowHelp
  | ShowRepeat
  | Echo

data Config =
  Config
    { cBotSettings  :: BotSettings
    , cPlatformName :: String
    , cToken        :: String
    , cGroupId      :: Maybe String
    , cLogLevel     :: Priority
    } deriving (Show, Eq)

$(deriveManyJSON
    [''Config
    ,''BotSettings
    ])

