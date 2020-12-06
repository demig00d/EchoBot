{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
module Configuration
  ( getConfig
  , Config(..)
  ) where

import           Control.Exception     (tryJust)
import           Data.Aeson            (eitherDecodeStrict)
import qualified Data.ByteString.Char8 as S8 (ByteString, readFile)
import           Data.Char             (toLower)
import           Data.Text             (Text)
import           System.IO.Error

import           Domain                (BotSettings (..))
import           Logging
import           Utils                 (deriveManyJSON)


getConfig :: FilePath -> IO (Either String Config)
getConfig path = do
  rawConfig <- readConfig path
  pure $ rawConfig >>= eitherDecodeStrict >>= verifyConfig

readConfig :: FilePath -> IO (Either String S8.ByteString)
readConfig path =
    tryJust selectIOError (S8.readFile path)
  where
    selectIOError :: IOError -> Maybe String
    selectIOError e
      | isAlreadyInUseError e = Just $ "File '" <> path <> "' is already open and cannot be reopened."
      | isDoesNotExistError e = Just $ "File '" <> path <> "' does not exist."
      | isPermissionError e   = Just $ "Can't open '" <> path <> "' permission denied."
      | otherwise             = Nothing

verifyConfig :: Config -> Either String Config
verifyConfig config = case toLower <$> cPlatformName config of
  "telegram"  -> Right config{cPlatformName = "telegram"}
  "vkontakte" -> verifyVKontakte $ cGroupId config
  name        -> Left $ "Can't recognize bot platform '" <> name <> "'."
  where
    verifyVKontakte = \case
      Just groupId -> Right config{cPlatformName = "vkontakte"}
      _            -> Left "group_id is required for VKontakte."

data Config =
  Config
    { cBotSettings  :: !BotSettings
    , cPlatformName :: !String
    , cToken        :: !String
    , cGroupId      :: !(Maybe String)
    , cLogLevel     :: !Priority
    }

$(deriveManyJSON
    [''Config
    ,''BotSettings
    ])
