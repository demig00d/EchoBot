{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
module VKontakte.API where

import qualified Data.ByteString.Char8      as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Char                  (isUpper, toLower)
import           Data.Data                  (Data (toConstr))
import           Data.Text                  (Text, cons)
import           Prelude                    hiding (log)

import           Data.UrlEncoded            (ToUrlEncoded, toUrlEncoded)
import           Requests                   (Handler (..), hContentType,
                                             sendPost, urlEncode)
import           Utils                      (deriveManyJSON, dropPrefixOptions)


apiUrl :: String
apiUrl = "https://api.vk.com/method/"


data Method
  = GroupsGetLongPollServer
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
      } deriving (Data, ToUrlEncoded, Show ,Eq)


getName :: Method -> String
getName = helper . show . toConstr where
  helper []     = []
  helper (x:xs) = toLower x : helper2 xs
  helper2 a | null a = []
            | isUpper (head a) = '.' : toLower (head a) : tail a
            | otherwise = head a : helper2 (tail a)


groupsGetLongPollServer ::
  String -> String -> String
  -> (S8.ByteString -> IO ())
  -> IO (Either L8.ByteString L8.ByteString)
groupsGetLongPollServer groupId cToken apiVersion logger =
  sendPost .
    encodeRequest logger $
      GroupsGetLongPollServer
        { gAccessToken = cToken
        , gGroupId = groupId
        , gV = apiVersion
        }


encodeRequest :: (S8.ByteString -> IO ()) -> Method -> Requests.Handler
encodeRequest logger = \case
  m@UpdatesGet{uServer=server} ->
       encode server m{uServer=""} -- Omit 'server' field in request body
                                   -- because its value is already contained inside URL
  m -> encode (apiUrl <> getName m) m
  where
    encode url body =
      Requests.Handler
        { url = url
        , body = urlEncode False $ toUrlEncoded body
        , headers = [(hContentType, "application/x-www-form-urlencoded")]
        , logger = logger
        }


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


$(deriveManyJSON dropPrefixOptions
    [ ''Keyboard
    , ''Button
    , ''Action
    , ''Payload
    ])


mkButton :: Text -> Button
mkButton b =
  Button $ Action
    { aType = "text"
    , aLabel = '/' `cons` b
    , aPayload = Payload $ '/' `cons` b
    }
