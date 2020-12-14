{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
module VKontakte.API where

import qualified Data.ByteString.Char8      as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Data
import           Data.Text                  (Text)

import           Requests                   (sendPostUrlEncoded)
import           Utils                      (deriveManyJSON)
import           VKontakte.Utils


apiUrl = "https://api.vk.com/method/"


data ServerKeyTs =
  ServerKeyTs
    { server :: String
    , key    :: String
    , ts     :: String
    }


data Method
  = GetLongPollServer
      { gAccessToken :: String
      , gGroupId     :: String
      , gV           :: String
      }
  | UpdatesGet -- Not method actually
      { uServer :: Maybe String
      , uKey    :: Text
      , uTs     :: Text
      }
  | MessagesSend
      { mAccessToken :: String
      , mGroupId     :: String
      , mUserId      :: Int
      , mRandomId    :: Int
      , mMessage     :: Maybe Text
      , mAttachment  :: Maybe Text
      , mStickerId   :: Maybe Int
      , mKeyboard    :: Maybe L8.ByteString
      , mV           :: String
      } deriving (Data, FormUrlEncoded)


sendMethod :: Method -> IO (Either L8.ByteString L8.ByteString)
sendMethod = \case
  m@GetLongPollServer{} ->
      sendPostUrlEncoded
        (apiUrl <> "groups.getLongPollServer")
        (toUrlEncoded m)
  m@UpdatesGet{uServer=Just server} ->
      sendPostUrlEncoded
        server
        (toUrlEncoded m{uServer=Nothing})
  m@MessagesSend{} ->
      sendPostUrlEncoded
        (apiUrl <> "messages.Send")
        (toUrlEncoded m)


data Keyboard =
  Keyboard
    { kOneTime :: Bool
    , kButtons :: [[Button]]
    }

newtype Button =
  Button
    { bAction :: Action
    }

data Action =
  Action
    { aType    :: Text
    , aPayload :: Payload
    , aLabel   :: Text
    }

newtype Payload =
  Payload
    { pButton :: Text
    }


$(deriveManyJSON
    [ ''Keyboard
    , ''Button
    , ''Action
    , ''Payload
    ])


mkButton b =
  Button $ Action
    { aType = "text"
    , aLabel = "/"<>b
    , aPayload = Payload $ "/"<>b
    }
