{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "purescript-avro"
, dependencies =
    [ "argonaut-codecs"
    , "bytestrings"
    , "console"
    , "effect"
    , "foreign"
    , "foreign-generic"
    , "gen"
    , "psci-support"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
