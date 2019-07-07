{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "canvas-graphics"
, dependencies =
    [ "arrays"
    , "canvas"
    , "console"
    , "effect"
    , "lists"
    , "math"
    , "psci-support"
    , "random"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
