{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Telegram.API where

import           Data.Aeson                 (encode)
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Text                  (Text)

import           Requests                   (sendGet, sendPost)
import           Telegram.Types
import           Utils                      (deriveManyJSON)



data Method
  = GetUpdates
      { gOffset  :: Int
      , gTimeout :: Int
      }
  | SendMessage
      { sChatId      :: Int
      , sText        :: Text
      , sReplyMarkup :: Maybe InlineKeyboardMarkup
      }
  | CopyMessage
      { cChatId     :: Int
      , cFromChatId :: Int
      , cMessageId  :: Int
      }

newtype InlineKeyboardMarkup =
  InlineKeyboardMarkup
    { rInlineKeyboard :: [[InlineKeyboardButton]]
    }

data InlineKeyboardButton =
  InlineKeyboardButton
    { iText         :: String
    , iCallbackData :: String
    }

$(deriveManyJSON
    [ ''Method
    , ''InlineKeyboardMarkup
    , ''InlineKeyboardButton
    ])


apiUrl = "https://api.telegram.org/bot"

mkKeyboard a = InlineKeyboardMarkup [fmap (uncurry InlineKeyboardButton) a]


getMe :: String -> IO (Either L8.ByteString L8.ByteString)
getMe token = do
  resp <- sendGet $ apiUrl <> token <> "/getMe"
  pure $ resp >>= verify
    where
      verify bs = case L8.take 10 . L8.drop 6 $ bs of
           "\"ok\": true" -> Right bs
           _              -> Left bs


getUpdates :: String -> Int -> IO (Either L8.ByteString L8.ByteString)
getUpdates token offset = sendPost url body
  where
    url = apiUrl <> token <> "/getUpdates"
    body = encode $ GetUpdates offset 25

sendMessage :: String -> Int -> Text -> Maybe InlineKeyboardMarkup -> IO (Either L8.ByteString L8.ByteString)
sendMessage token chatId text replyMarkup = sendPost url body
  where
    url  = apiUrl <> token <> "/sendMessage"
    body = encode $ SendMessage chatId text replyMarkup

copyMessage :: String -> Int -> Int -> Int -> IO (Either L8.ByteString L8.ByteString)
copyMessage token chatId fromChatId messageId = sendPost url body
  where
    url  = apiUrl <> token <> "/copyMessage"
    body = encode $ CopyMessage chatId fromChatId messageId
