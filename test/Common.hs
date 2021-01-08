{-# LANGUAGE OverloadedStrings #-}
module Common
  ( helpMessage
  , repeatMessage
  --
  , telegramModel
  , vkontakteModel
  --
  , telegramConfig
  , vkontakteConfig
  )where

import           Bot.Types
import           Data.Map.Strict (empty)
import           Data.Text       (Text)

import           Bot.Telegram    (TelegramEnv (..))
import           Bot.VKontakte   (VKontakteEnv (..))
import           Logging         (Priority (..))


helpMessage :: Text
helpMessage = "I am bot that can echo your messages."

repeatMessage :: Text
repeatMessage = "Choose number of repeats:"

model :: env -> Model env
model env =
  Model
      { botSettings =
         BotSettings
           { bHelpMessage = helpMessage
           , bRepeatMessage = repeatMessage
           , bNumberOfRepeats = 2
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
    , groupId = "923456789"
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
         , bNumberOfRepeats = 2
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
         , bNumberOfRepeats = 2
         }
    , cPlatformName = "vkontakte"
    , cToken = "<token>"
    , cGroupId  = Just "123456"
    , cLogLevel = Debug
    }
