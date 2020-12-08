module Utils
  ( deriveJSON
  , deriveManyJSON
  --
  , prettyShow
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


-- | Pretty show for data types with fields that have a Show
-- instance, breaks with more then one parenthesis at the end.
prettyShow :: Show a => a -> String
prettyShow str = unwords $ (pretty "" False) . words $ show str

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
