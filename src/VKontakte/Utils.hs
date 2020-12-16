{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module VKontakte.Utils
  ( FormUrlEncoded
  , toUrlEncoded
  , toUrlEncodedWithOptions
  , defaultOptions
  , fieldLabelModifier
  ) where

import           Data.Aeson                 (camelTo2)
import qualified Data.ByteString.Char8      as S8 (ByteString, pack)
import qualified Data.ByteString.Lazy.Char8 as L8 (ByteString, toStrict)
import           Data.Data
import           Data.Text                  (Text)
import           Data.Text.Encoding         (encodeUtf8)

import           Utils                      (dropPrefix)


data Options =
  Options { fieldLabelModifier :: String -> String }

defaultOptions :: Options
defaultOptions = Options id

type FieldName  = String
type FieldValue = S8.ByteString


class FormUrlEncoded a where
  toUrlEncoded :: a -> S8.ByteString
  default toUrlEncoded :: Data a => a -> S8.ByteString
  toUrlEncoded a = toUrlEncodedWithOptions
    defaultOptions{fieldLabelModifier = camelTo2 '_' . dropPrefix} a


toUrlEncodedWithOptions :: Data a => Options -> a -> S8.ByteString
toUrlEncodedWithOptions o d = helper . getPairs $ d
  where
    helper :: [(FieldName, FieldValue)] -> S8.ByteString
    helper []  = ""
    helper arr = foldl ampersand (firstParam $ head arr) (tail arr)

    ampersand :: S8.ByteString -> (FieldName, FieldValue) -> S8.ByteString
    ampersand x (a, b)  = x <> "&" <> S8.pack (f a) <> "=" <> b

    firstParam :: (FieldName, FieldValue) -> S8.ByteString
    firstParam (a, b)  = S8.pack (f a) <> "=" <> b

    f = fieldLabelModifier o


getNames :: Data object => object -> [FieldName]
getNames = constrFields . toConstr

getFields :: Data object => object -> [FieldValue]
getFields = gmapQ toByteString

getPairs :: Data object => object -> [(FieldName, FieldValue)]
getPairs = zipOmit <$> getNames <*> getFields


zipOmit :: [a] -> [S8.ByteString] -> [(a, S8.ByteString)]
zipOmit []     _bs      = []
zipOmit _as    []       = []
zipOmit (_a:as) ("":bs) = zipOmit as bs
zipOmit (a:as) (b:bs)   = (a, b) : zipOmit as bs


toByteString :: (Data a) => a -> S8.ByteString
toByteString =
     mkQ "" id
    `extQ` S8.pack
    `extQ` L8.toStrict
    `extQ` encodeUtf8
    `extQ` bshow @Int
    `extQ` bshow @Double
    `extQ` (\(a :: Maybe S8.ByteString) -> omitBS a)
    `extQ` (\(a :: Maybe L8.ByteString) -> omitLS a)
    `extQ` (\(a :: Maybe String)        -> omitString a)
    `extQ` (\(a :: Maybe Text)          -> omitText a)
    `extQ` (\(a :: Maybe Int)           -> omitInt a)
    `extQ` (\(a :: Maybe Double)        -> omitDouble a)
  where
    bshow :: (Show a) => a -> S8.ByteString
    bshow = S8.pack . show

    omitBS     = maybe "" id
    omitLS     = maybe "" L8.toStrict
    omitText   = maybe "" encodeUtf8
    omitString = maybe "" S8.pack
    omitInt    = maybe "" bshow
    omitDouble = maybe "" bshow


-- | Make a generic query;
-- start from a type-specific case;
-- return a constant otherwise.
mkQ :: (Typeable a, Typeable b) => r -> (b -> r) -> a -> r
(r `mkQ` br) a = maybe r br (cast a)


-- | Extend a generic query by a type-specific case.
extQ :: (Typeable a, Typeable b) => (a -> q) -> (b -> q) -> a -> q
extQ f g a = maybe (f a) g (cast a)
