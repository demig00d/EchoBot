{-# LANGUAGE StrictData #-}
module Domain where

import           Data.Text (Text)


data Model a =
  Model
    { mBotSettings   :: BotSettings
    , mPlatformEnv   :: a
    , mUsersSettings :: [UserSettings]
    }

data BotSettings =
  BotSettings
    { bHelpMessage     :: Text
    , bRepeatMessage   :: Text
    , bNumberOfRepeats :: Int
    }

data UserSettings =
  UserSettings
    { uId              :: Int
    , uNumberOfRepeats :: Int
    }

