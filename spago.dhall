{ name = "npm-package-json"
, license = "MIT"
, repository = "https://github.com/maxdeviant/purescript-npm-package-json"
, dependencies = [ "argonaut", "console", "effect", "psci-support" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
