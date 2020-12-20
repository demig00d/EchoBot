{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
module VKontakte.API where

import qualified Data.ByteString.Char8      as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Data
import           Data.Text                  (Text)
import           Prelude                    hiding (log)

import           Data.UrlEncoded            (ToUrlEncoded)
import           Requests                   (sendPostUrlEncoded)
import           Utils                      (deriveManyJSON)


apiUrl :: String
apiUrl = "https://api.vk.com/method/"


data Method
  = GetLongPollServer
      { gAccessToken :: String
      , gGroupId     :: String
      , gV           :: String
      }
  | UpdatesGet -- Not a VKontakte method actually
      { uServer :: String
      , uKey    :: String
      , uAct    :: String
      , uWait   :: Int
      , uTs     :: String
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
      } deriving (Data, ToUrlEncoded)


sendMethod :: (S8.ByteString -> IO ()) -> Method -> IO (Either L8.ByteString L8.ByteString)
sendMethod logger = \case
  m@GetLongPollServer{} ->
      send (apiUrl <> "groups.getLongPollServer") m

  m@UpdatesGet{uServer=uServer} ->
      send uServer m{uServer=""} -- Omit 'server' field in request body
                                               -- because its value is already contained inside URL
  m@MessagesSend{} ->
      send (apiUrl <> "messages.send") m
  where
    send url body = sendPostUrlEncoded logger url body


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


mkButton :: Text -> Button
mkButton b =
  Button $ Action
    { aType = "text"
    , aLabel = "/"<>b
    , aPayload = Payload $ "/"<>b
    }
