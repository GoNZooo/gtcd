{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "gtcd"
, dependencies =
  [ "arrays"
  , "assert"
  , "bifunctors"
  , "console"
  , "control"
  , "datetime"
  , "debug"
  , "effect"
  , "either"
  , "erl-atom"
  , "erl-binary"
  , "erl-kernel"
  , "erl-lists"
  , "erl-logger"
  , "erl-maps"
  , "erl-pinto"
  , "erl-process"
  , "erl-quickcheck-helpers"
  , "erl-simplebus"
  , "erl-tuples"
  , "exceptions"
  , "foldable-traversable"
  , "foreign"
  , "free"
  , "heterogeneous"
  , "integers"
  , "lists"
  , "maybe"
  , "newtype"
  , "ordered-collections"
  , "parsing"
  , "partial"
  , "prelude"
  , "quickcheck"
  , "record"
  , "refs"
  , "simple-json"
  , "simple-json-generics"
  , "strings"
  , "transformers"
  , "tuples"
  , "typelevel-prelude"
  , "unicode"
  , "unsafe-coerce"
  , "purerl-test"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, backend = "purerl"
}
