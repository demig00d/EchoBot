module Utils
  ( deriveManyJSON
  , standartOptions
  , dropPrefixOptions
  --
  , dropPrefix
  --
  , prettyShow
  , prettyShowMap
  , gshow
  --
  , eitherDecode
  --
  , nTimes
  ) where

import           Data.Aeson                 hiding (eitherDecode)
import qualified Data.Aeson                 as Aeson (eitherDecode)
import qualified Data.Aeson.TH              as TH (deriveJSON)
import           Data.ByteString.Char8      as S8 (ByteString)
import           Data.ByteString.Lazy.Char8 as L8 (ByteString)
import           Data.Char                  (isUpper, toLower)
import           Data.Map.Strict            as Map (Map, toList)
import           Data.String                (IsString, fromString)
import           Data.Text                  (Text)
import           Language.Haskell.TH


dropPrefixOptions :: Options
dropPrefixOptions =
  defaultOptions
    { fieldLabelModifier =  camelTo2 '_' . dropPrefix
    , omitNothingFields  = True
    , sumEncoding = UntaggedValue
    }

standartOptions :: Options
standartOptions =
  defaultOptions
    { fieldLabelModifier =  camelTo2 '_'
    , omitNothingFields  = True
    , sumEncoding = UntaggedValue
    }

deriveManyJSON :: Traversable t => Options -> t Name -> Q [Dec]
deriveManyJSON options names = concat <$> mapM (TH.deriveJSON options) names


dropPrefix :: String -> String
dropPrefix []                 = []
dropPrefix (x:xs) | isUpper x = toLower x : xs
                  | otherwise = dropPrefix xs


-- | Generalized version of show. Unlike show this function
-- is polymorphic in its result type.
gshow :: (Show a, IsString b) => a -> b
gshow x = fromString $ show x
{-# INLINE gshow #-}
{-# SPECIALIZE gshow :: Show a => a -> Text  #-}
{-# SPECIALIZE gshow :: Show a => a -> S8.ByteString  #-}
{-# SPECIALIZE gshow :: Show a => a -> L8.ByteString  #-}
{-# SPECIALIZE gshow :: Show a => a -> String  #-}

-- | Pretty show for data types with fields that have a Show
-- instance, breaks with more then one parenthesis at the end.
prettyShow :: Show a => a -> String
prettyShow str = unwords $ pretty "" False . words $ show str

-- | Helper for prettyShow.
pretty :: String -> Bool -> [String] -> [String]
pretty _ _ [] = []
pretty indent addComma (s:ss)
  | lastP        = [init s <> "\n" <> indent <> " }"]
  | equals       = s                                         : pretty indent False ss
  | penultimateP = init (init s) <> "\n" <> indent <> " }\n" : pretty (drop 5 indent) True ss
  | lastC        = (init s <> "\n" )                         : pretty indent True ss
  | constructor  = ("\n" <> indent <> "    " <> s <> "\n")   : pretty ("     " <> indent) False ss
  | firstP       = (indent <> "{ " <> tail s)                : pretty indent False ss
  | addComma     = (indent <> ", "<> s)                      : pretty indent False ss
  | otherwise    = s                                         : pretty indent False ss
  where
    lastP        = last s == '}'
    equals       = s == "="
    penultimateP = last (init s) == '}'
    constructor  = isUpper $ head s
    firstP       = head s == '{'
    lastC        = last s == ','


-- | Helper function for Map pretty-printing.
-- Used only in Bot module to prettify particular
-- log message like this:
-- [Debug] Map of user_id and repeat_number:
--                (100000000,       4      )
--                (100000001,       2      )
prettyShowMap :: (Show k, Show v) => Map k v -> String
prettyShowMap m =
  mconcat $ (\(k, v) -> "               " <> "(" <> show k <> ","
                     <> "       " <> show v
                     <> "      " <> ")\n") <$> Map.toList m


-- | Generalized version of eitherDecode from Data.Aeson.
eitherDecode :: (IsString a, FromJSON b) => L8.ByteString -> Either a b
eitherDecode bs = either (Left . fromString) Right (Aeson.eitherDecode bs)
{-# INLINE eitherDecode #-}
{-# SPECIALIZE eitherDecode :: FromJSON b => L8.ByteString -> Either L8.ByteString b #-}


nTimes :: Int -> String
nTimes = \case
     1   -> gshow (1 :: Int) <> " time"
     num -> gshow num <> " times"
