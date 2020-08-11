module Test.Main where

import Prelude
import Data.Argonaut (decodeJson, parseJson)
import Data.Either (fromRight)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Npm.PackageJson (PackageJson(..))
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Partial => Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        describe "parse package.json" do
          describe "simple" do
            it "works" do
              let
                json =
                  """
                  {
                    "name": "my-awesome-package",
                    "version": "1.0.0",
                    "description": "This package does awesome stuff!"
                  }
                  """
              let
                json' = fromRight $ parseJson json
              let
                actual = fromRight $ decodeJson json'
              let
                expected =
                  PackageJson
                    { name: "my-awesome-package"
                    , version: "1.0.0"
                    , description: Just $ "This package does awesome stuff!"
                    , keywords: mempty
                    }
              actual `shouldEqual` expected
