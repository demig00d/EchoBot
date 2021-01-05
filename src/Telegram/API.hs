{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
module Telegram.API where

import qualified Data.Aeson                 as Aeson (encode)
import qualified Data.ByteString.Char8      as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Char                  (toLower)
import           Data.Data                  (Data (toConstr))
import           Data.Text                  (Text)
import           Prelude                    hiding (log)

import           Requests                   (Handler (..), hContentType,
                                             sendGet)
import           Utils                      (deriveManyJSON, standartOptions)


apiUrl :: String
apiUrl = "https://api.telegram.org/bot"

data Method
  = GetUpdates
      { offset  :: Int
      , timeout :: Int
      }
  | SendMessage
      { chatId      :: Int
      , text        :: Text
      , replyMarkup :: Maybe InlineKeyboardMarkup
      }
  | CopyMessage
      { chatId     :: Int
      , fromChatId :: Int
      , messageId  :: Int
      }
  deriving (Data, Show, Eq)

newtype InlineKeyboardMarkup =
  InlineKeyboardMarkup
    { inlineKeyboard :: [[InlineKeyboardButton]]
    } deriving (Data, Show, Eq)

data InlineKeyboardButton =
  InlineKeyboardButton
    { text         :: String
    , callbackData :: String
    } deriving (Data, Show, Eq)


$(deriveManyJSON standartOptions
    [ ''Method
    , ''InlineKeyboardMarkup
    , ''InlineKeyboardButton
    ])

getName :: Method -> String
getName = helper . show . toConstr where
  helper []     = []
  helper (x:xs) = toLower x : xs


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


encodeRequest
  :: (S8.ByteString -> IO ()) -> String -> Method -> Requests.Handler
encodeRequest logger token method = encode (getName method) method
  where
    encode methodName body =
      let url  = apiUrl <> token <> ('/' : methodName)
      in Requests.Handler
          { url = url
          , body = L8.toStrict $ Aeson.encode body
          , headers = [(hContentType, "application/json")]
          , logger = logger
          }
