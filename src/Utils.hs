module Utils
  ( deriveJSON
  , deriveManyJSON
  ) where

import           Data.Aeson    (camelTo2)
import qualified Data.Aeson.TH as TH (defaultOptions, deriveJSON,
                                      fieldLabelModifier, omitNothingFields)
import           Data.Char     (isUpper, toLower)


deriveJSON = TH.deriveJSON TH.defaultOptions
    { TH.fieldLabelModifier =  camelTo2 '_' . dropPrefix
    , TH.omitNothingFields  = True}
  where
    dropPrefix []                 = []
    dropPrefix (x:xs) | isUpper x = toLower x : xs
                      | otherwise = dropPrefix xs


deriveManyJSON names = concat <$> mapM deriveJSON names
