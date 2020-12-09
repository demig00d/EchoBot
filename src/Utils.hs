module Utils
  ( deriveJSON
  , deriveManyJSON
  --
  , prettyShow
  , prettyShowMap
  , gshow
  --
  , eitherDecode
  --
  , lookupInsert
  ) where

import           Data.Aeson                 (FromJSON, camelTo2)
import qualified Data.Aeson                 as Aeson (eitherDecode)
import qualified Data.Aeson.TH              as TH (defaultOptions, deriveJSON,
                                                   fieldLabelModifier,
                                                   omitNothingFields)
import           Data.ByteString.Char8      as S8 (ByteString)
import           Data.ByteString.Lazy.Char8 as L8 (ByteString)
import           Data.Char                  (isUpper, toLower)
import           Data.Map.Strict            as Map (Map, insert, lookup, toList)
import           Data.String                (IsString, fromString)
import           Data.Text                  (Text)


deriveJSON = TH.deriveJSON TH.defaultOptions
    { TH.fieldLabelModifier =  camelTo2 '_' . dropPrefix
    , TH.omitNothingFields  = True}
  where
    dropPrefix []                 = []
    dropPrefix (x:xs) | isUpper x = toLower x : xs
                      | otherwise = dropPrefix xs

deriveManyJSON names = concat <$> mapM deriveJSON names


-- | Generalized version of show. Unlike show this function
-- is polymorphic in its result type.
gshow :: (Show a, IsString b) => a -> b
gshow x = fromString $ show x
{-# INLINE gshow #-}
{-# SPECIALIZE show :: Show a => a -> Text  #-}
{-# SPECIALIZE show :: Show a => a -> S8.ByteString  #-}
{-# SPECIALIZE show :: Show a => a -> L8.ByteString  #-}
{-# SPECIALIZE show :: Show a => a -> String  #-}

-- | Pretty show for data types with fields that have a Show
-- instance, breaks with more then one parenthesis at the end.
prettyShow :: Show a => a -> String
prettyShow str = unwords $ (pretty "" False) . words $ show str

-- | Helper for prettyShow.
pretty :: String -> Bool -> [String] -> [String]
pretty indent addComma (s:ss)
  | lastP        = (init s <> "\n" <> indent <> " }")             : []
  | equals       = s                                              : pretty indent False ss
  | penultimateP = ((init $ init s) <> "\n" <> indent <> " }\n" ) : pretty (drop 5 indent) True ss
  | lastC        = (init s <> "\n" )                              : pretty indent True ss
  | constructor  = ("\n" <> indent <> "    " <> s <> "\n")        : pretty ("     " <> indent) False ss
  | firstP       = (indent <> "{ " <> tail s)                     : pretty indent False ss
  | addComma     = (indent <> ", "<> s)                           : pretty indent False ss
  | otherwise    = s                                              : pretty indent False ss
  where
    lastP        = last s == '}'
    equals       = s == "="
    penultimateP = (last $ init s) == '}'
    constructor  = isUpper $ head s
    firstP       = head s == '{'
    lastC        = last s == ','


-- | Helper function for Map pretty-printing.
-- Used only in Bot module to prettify particular
-- log message like this:
-- [Debug] Map of user_id and repeat_number:
--                (100000000,       4      )
--                (100000001,       2      )
prettyShowMap m =
  mconcat $ (\(k, v) -> "               " <> "(" <> show k <> ","
                     <> "       " <> show v
                     <> "      " <> ")\n") <$> (Map.toList m)


-- | Generalized version of eitherDecode from Data.Aeson.
eitherDecode :: (IsString a, FromJSON b) => L8.ByteString -> Either a b
eitherDecode bs = either (Left . fromString) Right (Aeson.eitherDecode bs)
{-# INLINE eitherDecode #-}
{-# SPECIALIZE eitherDecode :: FromJSON b => L8.ByteString -> Either L8.ByteString b #-}


-- | Lookup value in Map or insert defailt value if it does not exist yet.
lookupInsert :: Ord k => k -> a -> Map k a -> (a, Map k a)
lookupInsert key defaultValue dict =
  maybe (defaultValue, Map.insert key defaultValue dict) (\v -> (v, dict)) (Map.lookup key dict)
