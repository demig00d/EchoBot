{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Telegram.API where

import           Data.Aeson                 (encode)
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Text                  (Text)

import           Requests                   (sendGet, sendPost)
import           Utils                      (deriveManyJSON)


apiUrl = "https://api.telegram.org/bot"

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


mkKeyboard a = InlineKeyboardMarkup [fmap (uncurry InlineKeyboardButton) a]


getMe :: String -> IO (Either L8.ByteString L8.ByteString)
getMe token = do
  resp <- sendGet $ apiUrl <> token <> "/getMe"
  pure $ resp >>= verify
    where
      verify bs = case L8.take 10 . L8.drop 6 $ bs of
           "\"ok\": true" -> Right bs
           _              -> Left bs


sendMethod :: String -> Method -> IO (Either L8.ByteString L8.ByteString)
sendMethod token = \case
  m@GetUpdates{}  ->
    send "/getUpdates" m
  m@SendMessage{} ->
    send "/sendMessage" m
  m@CopyMessage{} ->
    send "/copyMessage" m
  where
    send methodName method =
      sendPost
        (apiUrl <> token <> methodName)
        (encode method)
