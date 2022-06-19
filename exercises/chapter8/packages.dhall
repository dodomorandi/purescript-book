let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.2-20220615/packages.dhall
        sha256:6b62a899c22125a2735a7c354bbb66a2fe24ff45cec0a8b8b890769a01a99210

let overrides =
      { test-unit =
        { dependencies =
          [ "aff"
          , "either"
          , "prelude"
          , "effect"
          , "quickcheck"
          , "free"
          , "strings"
          , "lists"
          , "js-timers"
          , "avar"
          ]
        , repo = "https://github.com/2jt/purescript-test-unit.git"
        , version = "es-modules"
        }
      }

let additions =
      { react-basic =
        { dependencies = [ "prelude", "effect", "record" ]
        , repo = "https://github.com/lumihq/purescript-react-basic.git"
        , version = "main"
        }
      , indexed-monad =
        { dependencies = [ "control", "newtype" ]
        , repo = "https://github.com/garyb/purescript-indexed-monad.git"
        , version = "master"
        }
      }

in  upstream // overrides // additions
