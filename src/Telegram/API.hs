{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Telegram.API where

import           Data.Aeson                 (encode)
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Text                  (Text)
import           Prelude                    hiding (log)

import           Requests                   (sendGet, sendPost)
import           Utils                      (deriveManyJSON)


apiUrl :: String
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


mkKeyboard :: [(String, String)] -> InlineKeyboardMarkup
mkKeyboard a = InlineKeyboardMarkup [fmap (uncurry InlineKeyboardButton) a]


getMe :: String -> IO (Either L8.ByteString L8.ByteString)
getMe token = do
  resp <- sendGet $ apiUrl <> token <> "/getMe"
  pure $ resp >>= verify
    where
      verify bs = case L8.take 10 . L8.drop 6 $ bs of
           "\"ok\": true" -> Right bs
           _              -> Left bs


sendMethod :: (L8.ByteString -> IO ()) -> String -> Method -> IO (Either L8.ByteString L8.ByteString)
sendMethod logger token = \case
  m@GetUpdates{}  ->
    send "/getUpdates" m
  m@SendMessage{} ->
    send "/sendMessage" m
  m@CopyMessage{} ->
    send "/copyMessage" m
  where
    log a = logger $ "Request body: " <> a

    send methodName method =
      let body = encode method
      in log body
      >> sendPost
         (apiUrl <> token <> methodName)
          body
