{-# LANGUAGE OverloadedStrings #-}
module TelegramSpec where

import qualified Data.ByteString.Char8 as S8 (putStrLn)
import           Data.Map.Strict       (empty)
import           Data.Text             (Text)
import           Test.Hspec

import           Bot.Telegram
import           Bot.Types
import           Logging
import           Requests
import           Telegram.API
import           Telegram.Types


model :: Model TelegramEnv
model =
  Model
      { botSettings =
         BotSettings
           { bHelpMessage = "I am bot that can echo your messages."
           , bRepeatMessage = "Choose number of repeats:"
           , bNumberOfRepeats = 2
           }
      , platformEnv =
         TelegramEnv
           { token = "<token>"
           , offset = 0
           }
      , usersSettings = empty
      , logLevel = Debug
      }


formUpdate :: Text -> Update
formUpdate t =
    Update
      { rUpdateId = 783724748
      , rMessage =
         Just (Message
           { mMessageId = 100
           , mFrom =
              User
                { uId = 123456789
                , uIsBot = False
                , uFirstName = "UserName"
                , uLastName = Nothing
                , uUsername = Just "Nickname"
                , uLanguageCode = Just "ru"
                }
            , mDate = 1000000000
            , mChat =
                Chat
                  { cId = 123456789
                  , cType = "private"
                  , cTitle = Nothing
                  , cUsername = Just "Nickname"
                  , cFirstName = Just "UserName"
                  , cLastName = Nothing
                  }
             , mForwardFrom = Nothing
             , mReplyToMessage = Nothing
             , mText = Just t
             })
      , rCallbackQuery = Nothing
      , rData = Nothing
      , rEditedMessage = Nothing
      }

helpUpdate :: Update
helpUpdate  = formUpdate "/help"

keyboardUpdate :: Update
keyboardUpdate = formUpdate "/repeat"

echoUpdate :: Update
echoUpdate = formUpdate "some text"


getIncomeQuery :: Requests.Handler
getIncomeQuery =
  Requests.Handler
    { url = "https://api.telegram.org/bot<token>/getUpdates"
    , body = "{\"offset\":0,\"timeout\":25}"
    , headers = [(hContentType, "application/json")]
    , logger = S8.putStrLn
    }

sendHelp :: Action
sendHelp = Send
  SendMessage
    { chatId = 123456789
    , text   = "I am bot that can echo your messages."
    , replyMarkup = Nothing
    }

sendKeyboard :: Action
sendKeyboard = Send
  SendMessage
    { chatId = 123456789
    , text = "Current number of repeats = 2.\nChoose number of repeats:"
    , replyMarkup = Just
        (InlineKeyboardMarkup
            [[ InlineKeyboardButton
                { iText         = "1"
                , iCallbackData = "1"
                }
            , InlineKeyboardButton
                { iText         = "2"
                , iCallbackData = "2"
                }
            , InlineKeyboardButton
                { iText         = "3"
                , iCallbackData = "3"
                }
            , InlineKeyboardButton
                { iText         = "4"
                , iCallbackData = "4"
                }
            , InlineKeyboardButton
                { iText         = "5"
                , iCallbackData = "5"
                }
            ]]
        )
    }

copyMessage :: Action
copyMessage = Send
  CopyMessage
    { chatId     = 123456789
    , fromChatId = 123456789
    , messageId  = 100
    }


spec :: Spec
spec = do
  describe "Telegram methods:" $ do
    it "get request with 'getUpdates' method from Model of bot" $
      encodeGetIncome model `shouldBe` getIncomeQuery

    it "handle update with '/help' command" $
      handleUpdate model helpUpdate `shouldBe` sendHelp

    it "handle update with '/repeat' command" $
      handleUpdate model keyboardUpdate `shouldBe` sendKeyboard

    it "handle update with ordinary message" $
      handleUpdate model echoUpdate `shouldBe` copyMessage
