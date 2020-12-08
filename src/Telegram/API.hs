{-# LANGUAGE OverloadedStrings #-}
module Telegram.API where

import           Data.Aeson                 (eitherDecodeStrict)
import qualified Data.ByteString.Lazy.Char8 as L8

import           Requests
import           Telegram.Types


urlAPI = "https://api.telegram.org/bot"

getMe :: String -> IO (Either L8.ByteString L8.ByteString)
getMe token = do
  resp <- sendGet $ urlAPI <> token <> "/getMe"
  pure $ resp >>= verify
    where
      verify bs = case L8.take 10 . L8.drop 6 $ bs of
           "\"ok\": true" -> Right bs
           _              -> Left bs


