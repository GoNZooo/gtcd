{-
Welcome to your new Dhall package-set!

Below are instructions for how to edit this file for most use
cases, so that you don't need to know Dhall to use it.

## Use Cases

Most will want to do one or both of these options:
1. Override/Patch a package's dependency
2. Add a package not already in the default package set

This file will continue to work whether you use one or both options.
Instructions for each option are explained below.

### Overriding/Patching a package

Purpose:
- Change a package's dependency to a newer/older release than the
    default package set's release
- Use your own modified version of some dependency that may
    include new API, changed API, removed API by
    using your custom git repo of the library rather than
    the package set's repo

Syntax:
where `entityName` is one of the following:
- dependencies
- repo
- version
-------------------------------
let upstream = --
in  upstream
  with packageName.entityName = "new value"
-------------------------------

Example:
-------------------------------
let upstream = --
in  upstream
  with halogen.version = "master"
  with halogen.repo = "https://example.com/path/to/git/repo.git"

  with halogen-vdom.version = "v4.0.0"
  with halogen-vdom.dependencies = [ "extra-dependency" ] # halogen-vdom.dependencies
-------------------------------

### Additions

Purpose:
- Add packages that aren't already included in the default package set

Syntax:
where `<version>` is:
- a tag (i.e. "v4.0.0")
- a branch (i.e. "master")
- commit hash (i.e. "701f3e44aafb1a6459281714858fadf2c4c2a977")
-------------------------------
let upstream = --
in  upstream
  with new-package-name =
    { dependencies =
       [ "dependency1"
       , "dependency2"
       ]
    , repo =
       "https://example.com/path/to/git/repo.git"
    , version =
        "<version>"
    }
-------------------------------

Example:
-------------------------------
let upstream = --
in  upstream
  with benchotron =
      { dependencies =
          [ "arrays"
          , "exists"
          , "profunctor"
          , "strings"
          , "quickcheck"
          , "lcg"
          , "transformers"
          , "foldable-traversable"
          , "exceptions"
          , "node-fs"
          , "node-buffer"
          , "node-readline"
          , "datetime"
          , "now"
          ]
      , repo =
          "https://github.com/hdgarrood/purescript-benchotron.git"
      , version =
          "v7.0.0"
      }
-------------------------------
-}
let upstream =
      https://github.com/purerl/package-sets/releases/download/erl-0.15.3-20220629/packages.dhall
        sha256:48ee9f3558c00e234eae6b8f23b4b8b66eb9715c7f2154864e1e425042a0723b

let erl-quickcheck-helpers =
      https://raw.githubusercontent.com/quanterall/purescript-erl-quickcheck-helpers/v0.0.4/spago.dhall
        sha256:df6abef567d04b64cef1dd714d1e872d7e7800cf89a9a2184fbb35ccb3c65468

let purerl-test =
      https://raw.githubusercontent.com/GoNZooo/purerl-test/v0.1.0/spago.dhall
        sha256:dc58efbdc04ed56d57eb405ee1ec5b592d3415ffeec0d416e65f8c220d8566b2

let overrides =
      { erl-quickcheck-helpers =
        { repo =
            "https://github.com/quanterall/purescript-erl-quickcheck-helpers.git"
        , version = "v0.0.4"
        , dependencies = erl-quickcheck-helpers.dependencies
        }
      , erl-simplebus =
        { dependencies = upstream.erl-simplebus.dependencies
        , repo = "https://github.com/id3as/purescript-erl-simplebus.git"
        , version = "551cb2e110f70bf470825d0912c06c48e71867bc"
        }
      , purerl-test =
        { dependencies = purerl-test.dependencies
        , repo = "https://github.com/GoNZooo/purerl-test.git"
        , version = "v0.1.0"
        }
      }

in  upstream // overrides
