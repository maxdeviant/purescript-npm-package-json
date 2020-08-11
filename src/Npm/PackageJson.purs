module Npm.PackageJson
  ( PackageJson(..)
  ) where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, jsonEmptyObject, stringify, (.!=), (.:), (.:?), (:=), (:=?), (~>), (~>?))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Maybe (Maybe)

newtype PackageJson
  = PackageJson
  { name :: String
  , version :: String
  , description :: Maybe String
  , keywords :: Array String
  }

derive instance genericPackageJson :: Generic PackageJson _

instance eqPackageJson :: Eq PackageJson where
  eq = genericEq

instance showPackageJson :: Show PackageJson where
  show = stringify <<< encodeJson

instance encodeJsonPackageJson :: EncodeJson PackageJson where
  encodeJson (PackageJson packageJson) = do
    "name" := packageJson.name
      ~> "version"
      := packageJson.version
      ~> "description"
      :=? packageJson.description
      ~>? "keywords"
      := packageJson.keywords
      ~> jsonEmptyObject

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
