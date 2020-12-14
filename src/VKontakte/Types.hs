{-# LANGUAGE TemplateHaskell #-}
module VKontakte.Types where

import           Data.Aeson hiding (Object, object)
import           Data.Text  (Text)

import           Utils      (deriveManyJSON)


newtype Sticker =
  Sticker
    { sStickerId :: Int
    }

data Media =
  Media
    { mAccessKey :: Text
    , mId        :: Int
    , mOwnerId   :: Int
    }

data Attachment =
  Attachment
    { aType    :: String
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

newtype Update =
  Update
    { uObject :: Object
    }

data Response =
  Response
    { rTs      :: Maybe String
    , rUpdates :: Maybe [Update]
    }


$(deriveManyJSON
    [ ''Sticker
    , ''Media
    , ''Attachment
    , ''Message
    , ''Object
    , ''Update
    , ''Response
    ])
