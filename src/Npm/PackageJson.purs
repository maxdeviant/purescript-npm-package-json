module Npm.PackageJson
  ( PackageJson(..)
  , Person(..)
  , Bin(..)
  ) where

import Prelude
import Control.Alt ((<|>))
import Data.Argonaut
  ( class DecodeJson
  , class EncodeJson
  , Json
  , JsonDecodeError(..)
  , caseJsonObject
  , caseJsonString
  , decodeJson
  , encodeJson
  , jsonEmptyObject
  , stringify
  , (.:)
  , (.:?)
  , (:=)
  , (:=?)
  , (~>)
  , (~>?)
  )
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Foreign.Object (Object)
import Foreign.Object as Object

-- | An npm [package.json](https://docs.npmjs.com/files/package.json) file.
newtype PackageJson
  = PackageJson
  { name :: String
  , version :: String
  , description :: Maybe String
  , keywords :: Maybe (Array String)
  , homepage :: Maybe String
  , author :: Maybe Person
  , contributors :: Maybe (Array Person)
  , bin :: Maybe Bin
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
      :=? packageJson.keywords
      ~>? "homepage"
      :=? packageJson.homepage
      ~>? "author"
      :=? packageJson.author
      ~>? "contributors"
      :=? packageJson.contributors
      ~>? "bin"
      :=? packageJson.bin
      ~>? jsonEmptyObject

instance decodeJsonPackageJson :: DecodeJson PackageJson where
  decodeJson json = do
    json' <- decodeJson json
    name <- json' .: "name"
    version <- json' .: "version"
    description <- json' .:? "description"
    keywords <- json' .:? "keywords"
    homepage <- json' .:? "homepage"
    author <- json' .:? "author"
    contributors <- json' .:? "contributors"
    bin <- json' .:? "bin"
    pure
      $ PackageJson
          { name
          , version
          , description
          , keywords
          , homepage
          , author
          , contributors
          , bin
          }

data Person =
  Person
    { name :: String
    , email :: Maybe String
    , url :: Maybe String
    }

derive instance genericPerson :: Generic Person _

instance eqPerson :: Eq Person where
  eq = genericEq

instance encodeJsonPerson :: EncodeJson Person where
  encodeJson (Person person) = do
    "name" := person.name
      ~> "email" :=? person.email
      ~>? "url" :=? person.url
      ~>? jsonEmptyObject

instance decodeJsonPerson :: DecodeJson Person where
  decodeJson json = do
    json' <- decodeJson json
    name <- json' .: "name"
    email <- json' .:? "email"
    url <- json' .:? "url"
    pure $ Person { name, email, url}

data Bin
  = BinPath String
  | BinPaths (Map String String)

derive instance genericBin :: Generic Bin _

instance eqBin :: Eq Bin where
  eq = genericEq

mapToObject :: Map String String -> Object Json
mapToObject =
  fromFoldable
    <<< Map.toUnfoldable
    <<< Map.mapMaybe (Just <<< encodeJson)
  where
  fromFoldable = Object.fromFoldable :: Array _ -> Object Json

mapFromObject :: Object Json -> Map String String
mapFromObject =
  Map.mapMaybe (caseJsonString Nothing Just)
    <<< Map.fromFoldable
    <<< toUnfoldable
  where
  toUnfoldable = Object.toUnfoldable :: Object Json -> Array _

instance encodeJsonBin :: EncodeJson Bin where
  encodeJson bin = case bin of
    BinPath path -> encodeJson path
    BinPaths paths -> encodeJson $ mapToObject $ paths

instance decodeJsonBin :: DecodeJson Bin where
  decodeJson json = do
    caseJsonString (Left MissingValue) (Right <<< BinPath) json
      <|> caseJsonObject (Left MissingValue) (Right <<< BinPaths <<< mapFromObject) json
