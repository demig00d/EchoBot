{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Requests
  ( Handler(..)
  , sendGet
  , hContentType
  , sendPost
  , urlEncode
  ) where


import           Control.Exception          (tryJust)
import           Data.Aeson                 (Value, decode)
import           Data.Aeson.Encode.Pretty   (encodePretty)
import qualified Data.ByteString.Char8      as S8 (ByteString, pack)
import qualified Data.ByteString.Lazy.Char8 as L8 (ByteString, pack)
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS    (tlsManagerSettings)
import           Network.HTTP.Types         (RequestHeaders, hContentType,
                                             urlEncode)
import           Prelude                    hiding (log)


data Handler =
  Handler
    { url     :: String
    , body    :: S8.ByteString
    , headers :: RequestHeaders
    , logger  :: S8.ByteString -> IO ()
    }

instance Eq Handler where
  Handler url1 body1 headers1 _ == Handler url2 body2 headers2 _
    =  url1     == url2
    && body1    == body2
    && headers1 == headers2

instance Show Handler where
  show Handler{url, body, headers} =
    "Handler{ url="     <> show url
        <> ", body="    <> show body
        <> ", headers=" <> show headers
        <> "}"


sendPost :: Handler -> IO (Either L8.ByteString L8.ByteString)
sendPost Handler{..} = do
  log url body
  send request
    { method = "POST"
    , requestBody = RequestBodyBS body
    , requestHeaders = headers
    }
  where
    log u b = logger $ "\n    URL: " <> S8.pack u <> "\n    Request body: " <> b

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
  manager <- newManager tlsManagerSettings

  tryJust selectHttpException (responseBody <$> httpLbs request manager)
  where
    selectHttpException :: HttpException -> Maybe L8.ByteString
    selectHttpException = \case
      HttpExceptionRequest failedRequest content ->
          Just . L8.pack $ show failedRequest <> show content

      InvalidUrlException url reason ->
          Just . L8.pack $ "A URL '" <> url <> "': " <> reason <> "."

