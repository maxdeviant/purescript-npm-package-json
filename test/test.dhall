let conf = ../spago.dhall
in conf //
  { dependencies =
      conf.dependencies #
        [ "aff", "effect", "exceptions", "spec" ]
  , sources =
      conf.sources #
        [ "test/**/*.purs"
        ]
  }
