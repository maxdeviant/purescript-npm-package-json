module Npm.PackageJson
  ( PackageJson(..)
  ) where

import Prelude
import Data.Argonaut (class DecodeJson, decodeJson, (.:), (.:?), (.!=))
import Data.Maybe (Maybe)

newtype PackageJson
  = PackageJson
  { name :: String
  , version :: String
  , description :: Maybe String
  , keywords :: Array String
  }

instance decodeJsonPackageJson :: DecodeJson PackageJson where
  decodeJson json = do
    json' <- decodeJson json
    name <- json' .: "name"
    version <- json' .: "version"
    description <- json' .:? "description"
    keywords <- json' .:? "keywords" .!= mempty
    pure
      $ PackageJson
          { name
          , version
          , description
          , keywords
          }
