{-# LANGUAGE OverloadedStrings #-}
module TelegramSpec where

import qualified Data.ByteString.Char8      as S8 (putStrLn)
import qualified Data.ByteString.Lazy.Char8 as L8 (ByteString)
import           Data.Functor.Identity
import           Data.Text                  (Text)
import           Test.Hspec

import           Bot.Telegram
import           Common
import           Requests
import           Telegram.API
import           Telegram.Types


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

payloadUpdate :: String -> Update
payloadUpdate n =
    Update
      { rUpdateId = 783724748
      , rMessage = Nothing
      , rCallbackQuery =
        Just CallbackQuery
          { cData = n
          , cFrom =
              User
                { uId = 123456789
                , uIsBot = False
                , uFirstName = "UserName"
                , uLastName = Nothing
                , uUsername = Just "Nickname"
                , uLanguageCode = Just "ru"
                }
          }
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
    , text   = helpMessage
    , replyMarkup = Nothing
    }

sendKeyboard :: Action
sendKeyboard = Send
  SendMessage
    { chatId = 123456789
    , text = "Current number of repeats = 2.\n" <> repeatMessage
    , replyMarkup = Just
        (InlineKeyboardMarkup
            [[ InlineKeyboardButton
                { text         = "1"
                , callbackData = "1"
                }
            , InlineKeyboardButton
                { text         = "2"
                , callbackData = "2"
                }
            , InlineKeyboardButton
                { text         = "3"
                , callbackData = "3"
                }
            , InlineKeyboardButton
                { text         = "4"
                , callbackData = "4"
                }
            , InlineKeyboardButton
                { text         = "5"
                , callbackData = "5"
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

mockMethod :: String -> Identity (Either L8.ByteString L8.ByteString)
mockMethod _ = Identity $ Right ""

spec :: Spec
spec = do
  describe "Telegram methods:" $ do
    it "obtain model from config" $
      runIdentity (getModel telegramConfig mockMethod) `shouldBe` Right telegramModel

    it "form request with 'getUpdates' method from Model of bot" $
      encodeGetIncome telegramModel `shouldBe` getIncomeQuery

    it "handle update with '/help' command" $
      getAction telegramModel helpUpdate `shouldBe` sendHelp

    it "handle update with '/repeat' command" $
      getAction telegramModel keyboardUpdate `shouldBe` sendKeyboard

    it "handle update with ordinary message" $
      getAction telegramModel echoUpdate `shouldBe` copyMessage

    it "handle update with payload" $
      let n = 3
      in getAction telegramModel (payloadUpdate $ show n) `shouldBe` SetRepeatNumber 123456789 n
