{-# LANGUAGE OverloadedStrings #-}
module Requests where

import           Control.Exception          (tryJust)
import           Data.Aeson                 (Value, decode)
import           Data.Aeson.Encode.Pretty   (encodePretty)
import qualified Data.ByteString.Char8      as S8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L8 (ByteString, pack, toStrict)
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS    (tlsManagerSettings)
import           Network.HTTP.Types         (hContentType)


sendPost :: String -> L8.ByteString -> IO (Either L8.ByteString L8.ByteString)
sendPost url body =
  send request
    { method = "POST"
    , requestBody = RequestBodyBS . L8.toStrict $ body
    , requestHeaders = [(hContentType, "application/json")]
    }
  where
    request = parseRequest_ url

sendPostUrlEncoded :: String -> S8.ByteString -> IO (Either L8.ByteString L8.ByteString)
sendPostUrlEncoded url body =
  send request
    { method = "POST"
    , requestBody = RequestBodyBS body
    , requestHeaders = [(hContentType, "application/x-www-form-urlencoded")]
    }
  where
    request = parseRequest_ url

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

  response <- tryJust selectHttpException (responseBody <$> httpLbs request manager)
  return $ response
  where
    selectHttpException :: HttpException -> Maybe L8.ByteString
    selectHttpException = \case
      HttpExceptionRequest request content ->
          Just . L8.pack $ show request <> show content

      InvalidUrlException url reason ->
          Just . L8.pack $ "A URL '"<> url <> "': " <> reason <> "."

