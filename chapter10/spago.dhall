{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "console"
    , "contravariant"
    , "effect"
    , "either"
    , "foreign"
    , "foreign-generic"
    , "maybe"
    , "newtype"
    , "psci-support"
    , "transformers"
    , "validation"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
