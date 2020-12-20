{-# LANGUAGE OverloadedStrings #-}
module Requests where

import           Control.Exception          (tryJust)
import           Data.Aeson                 (ToJSON, Value, decode, encode)
import           Data.Aeson.Encode.Pretty   (encodePretty)
import qualified Data.ByteString.Char8      as S8 (ByteString, pack)
import qualified Data.ByteString.Lazy.Char8 as L8 (ByteString, pack, toStrict)
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS    (tlsManagerSettings)
import           Network.HTTP.Types         (hContentType, urlEncode)
import           Prelude                    hiding (log)

import           Data.UrlEncoded


sendPostJSON :: ToJSON b => (S8.ByteString -> IO ()) -> String -> b -> IO (Either L8.ByteString L8.ByteString)
sendPostJSON logger url body = do
  log url bodyEncoded
  send request
    { method = "POST"
    , requestBody = RequestBodyBS bodyEncoded
    , requestHeaders = [(hContentType, "application/json")]
    }
  where
    log u b = logger $ "\n    URL: " <> S8.pack u <> "\n    Request body: " <> b

    request = parseRequest_ url
    bodyEncoded = L8.toStrict $ encode body

sendPostUrlEncoded :: ToUrlEncoded b => (S8.ByteString -> IO ()) -> String -> b -> IO (Either L8.ByteString L8.ByteString)
sendPostUrlEncoded logger url body = do
  log url bodyEncoded
  send request
    { method = "POST"
    , requestBody = RequestBodyBS bodyEncoded
    , requestHeaders = [(hContentType, "application/x-www-form-urlencoded")]
    }
  where
    log u b = logger $ "\n    URL: " <> S8.pack u <> "\n    Request body: " <> b

    request = parseRequest_ url
    bodyEncoded = urlEncode False $ toUrlEncoded body


sendGet :: String -> IO (Either L8.ByteString L8.ByteString)
sendGet url = send $ parseRequest_ url


-- | Send request and check if response is JSON.
send :: Request -> IO (Either L8.ByteString L8.ByteString)
send request = do
  response <- sendRequest request
  pure $ response >>= decodeJSON
  where
    decodeJSON resp = case decode resp :: Maybe Value of
           Just v -> Right $ encodePretty v
           _      -> Left $ "Response is not JSON: " <> resp


sendRequest :: Request -> IO (Either L8.ByteString L8.ByteString)
sendRequest request = do
  -- A Manager is present to keep track of open connections, so that multiple
  -- requests to the same server use the same connection.
  manager  <- newManager tlsManagerSettings

  tryJust selectHttpException (responseBody <$> httpLbs request manager)
  where
    selectHttpException :: HttpException -> Maybe L8.ByteString
    selectHttpException = \case
      HttpExceptionRequest failedRequest content ->
          Just . L8.pack $ show failedRequest <> show content

      InvalidUrlException url reason ->
          Just . L8.pack $ "A URL '"<> url <> "': " <> reason <> "."

