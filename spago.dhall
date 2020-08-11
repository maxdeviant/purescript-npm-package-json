{ name = "npm-package-json"
, dependencies = [ "argonaut", "console", "effect", "psci-support" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
