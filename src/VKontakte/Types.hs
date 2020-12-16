{-# LANGUAGE TemplateHaskell #-}
module VKontakte.Types where

import           Data.Text (Text)

import           Utils     (deriveManyJSON)


newtype Sticker =
  Sticker
    { sStickerId :: Int
    }

data Media =
  Media
    { mAccessKey :: Maybe Text
    , mId        :: Int
    , mOwnerId   :: Int
    }

data Attachment =
  Attachment
    { aType    :: Text
    , aSticker :: Maybe Sticker
    , aPhoto   :: Maybe Media
    , aVideo   :: Maybe Media
    , aAudio   :: Maybe Media
    , aDoc     :: Maybe Media
    }

data Message =
  Message
    { mFromId      :: Int
    , mText        :: Text
    , mRandomId    :: Int
    , mAttachments :: [Attachment]
    }

newtype Object =
  Object
    { oMessage :: Maybe Message
    }

data Update =
  Update
    { uObject :: Object
    , uType   :: Text
    }

data ServerKeyTs =
  ServerKeyTs
    { sServer :: String
    , sKey    :: String
    , sTs     :: String
    }

data Response =
  Response
    { rTs       :: Maybe String
    , rResponse :: Maybe ServerKeyTs
    , rUpdates  :: Maybe [Update]
    }


$(deriveManyJSON
    [ ''Sticker
    , ''Media
    , ''Attachment
    , ''Message
    , ''Object
    , ''Update
    , ''ServerKeyTs
    , ''Response
    ])
