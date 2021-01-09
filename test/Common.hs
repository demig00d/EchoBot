{-# LANGUAGE OverloadedStrings #-}
module Common
  ( helpMessage
  , repeatMessage
  , repeatsNumber
  , repeats
  --
  , telegramModel
  , vkontakteModel
  --
  , telegramConfig
  , vkontakteConfig
  --
  , Common.groupId
  )where

import           Bot.Types
import           Data.Map.Strict (empty)
import           Data.String     (IsString)
import           Data.Text       (Text)

import           Bot.Telegram    (TelegramEnv (..))
import           Bot.VKontakte   (VKontakteEnv (..))
import           Logging         (Priority (..))
import           Utils           (gshow)


helpMessage :: IsString a => a
helpMessage = "I am bot that can echo your messages."

repeatMessage :: IsString a => a
repeatMessage = "Choose number of repeats:"

repeatsNumber :: Int
repeatsNumber = 2

repeats :: IsString a => a
repeats = gshow repeatsNumber

groupId :: IsString a => a
groupId = "923456789"

model :: env -> Model env
model env =
  Model
      { botSettings =
         BotSettings
           { bHelpMessage = helpMessage
           , bRepeatMessage = repeatMessage
           , bNumberOfRepeats = repeatsNumber
           }
      , platformEnv = env
      , usersSettings = empty
      , logLevel = Debug
      }

telegramModel :: Model TelegramEnv
telegramModel = model $ TelegramEnv "<token>" 0

vkontakteModel :: Model VKontakteEnv
vkontakteModel = model
  VKontakteEnv
    { token   = "<token>"
    , groupId = Common.groupId
    , server  = "https://server.com"
    , key     = "46567asdfgh"
    , ts      = "3"
    }

telegramConfig :: Config
telegramConfig =
  Config
    { cBotSettings =
       BotSettings
         { bHelpMessage = helpMessage
         , bRepeatMessage = repeatMessage
         , bNumberOfRepeats = repeatsNumber
         }
    , cPlatformName = "telegram"
    , cToken = "<token>"
    , cGroupId  = Nothing
    , cLogLevel = Debug
    }

vkontakteConfig :: Config
vkontakteConfig =
  Config
    { cBotSettings =
       BotSettings
         { bHelpMessage = helpMessage
         , bRepeatMessage = repeatMessage
         , bNumberOfRepeats = repeatsNumber
         }
    , cPlatformName = "vkontakte"
    , cToken = "<token>"
    , cGroupId  = Just Common.groupId
    , cLogLevel = Debug
    }
