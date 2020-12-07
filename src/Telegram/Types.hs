{-# LANGUAGE TemplateHaskell #-}
module Telegram.Types where

import           Data.Text (Text)

import           Utils     (deriveManyJSON)

data Chat =
  Chat
    { cId        :: Int
    , cType      :: Text
    , cTitle     :: Maybe Text
    , cUsername  :: Maybe Text
    , cFirstName :: Maybe Text
    , cLastName  :: Maybe Text
    }

data Message =
  Message
    { mMessageId      :: Int
    , mFrom           :: User
    , mDate           :: Int
    , mChat           :: Chat
    , mForwardFrom    :: Maybe User
    , mReplyToMessage :: Maybe Message
    , mText           :: Maybe Text
    }

data CallbackQuery =
  CallbackQuery
    { cData :: String
    , cFrom :: User
    }

data Update =
  Update
    { rUpdateId      :: Int
    , rMessage       :: Maybe Message
    , rCallbackQuery :: Maybe CallbackQuery
    , rData          :: Maybe Text
    , rEditedMessage :: Maybe Message
    }

data Response =
  Response
    { rOk          :: Bool
    , rDescription :: Maybe String
    , rResult      :: Maybe [Update]
    , rErrorCode   :: Maybe Int
    }

data User =
  User
    { uId           :: Int
    , uisBot        :: Bool
    , uFirstName    :: Text
    , uLastName     :: Maybe Text
    , uUsername     :: Maybe Text
    , uLanguageCode :: Maybe Text
    }

$(deriveManyJSON
    [ ''Chat
    , ''Message
    , ''CallbackQuery
    , ''Update
    , ''Response
    , ''User
    ])
