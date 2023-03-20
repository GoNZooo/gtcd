module Test.Gtcd.BEncoding
  ( testSuite
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Tuple.Nested ((/\))
import Erl.Data.List (nil, (:))
import Erl.Data.Map as Map
import Erl.Test.EUnit (TestSuite, suite, test)
import Gtcd.BEncoding as BEncoding
import Test.Assert (assertEqual)

testSuite :: TestSuite
testSuite = do
  suite "BEncoding" do
    test "Encodes `Int`s properly" do
      assertEqual { actual: BEncoding.encode 42, expected: "i42e" }
      assertEqual { actual: BEncoding.encode (-42), expected: "i-42e" }

    test "Encodes `String`s properly" do
      assertEqual { actual: BEncoding.encode "foo", expected: "3:foo" }
      assertEqual { actual: BEncoding.encode "bar", expected: "3:bar" }
      assertEqual { actual: BEncoding.encode "baz", expected: "3:baz" }
      assertEqual { actual: BEncoding.encode "spam", expected: "4:spam" }

    test "Encodes `Array`s properly" do
      assertEqual { actual: BEncoding.encode [ 1, 2, 3 ], expected: "li1ei2ei3ee" }
      assertEqual { actual: BEncoding.encode [ "foo", "bar", "baz" ], expected: "l3:foo3:bar3:baze" }

    test "Encodes `List`s properly" do
      assertEqual { actual: BEncoding.encode (1 : 2 : 3 : nil), expected: "li1ei2ei3ee" }
      assertEqual
        { actual: BEncoding.encode ("foo" : "bar" : "baz" : nil), expected: "l3:foo3:bar3:baze" }

    test "Encodes `Map`s properly" do
      assertEqual
        { actual: BEncoding.encode (Map.fromFoldable [ "foo" /\ 1, "bar" /\ 2, "baz" /\ 3 ])
        , expected: "d3:bari2e3:bazi3e3:fooi1ee"
        }
      assertEqual
        { actual: BEncoding.encode (Map.fromFoldable [ "foo" /\ "bar", "baz" /\ "spam" ])
        , expected: "d3:baz4:spam3:foo3:bare"
        }
      assertEqual
        { actual: BEncoding.encode (Map.fromFoldable [ "cow" /\ "moo", "spam" /\ "eggs" ])
        , expected: "d3:cow3:moo4:spam4:eggse"
        }
      assertEqual
        { actual: BEncoding.encode (Map.fromFoldable [ "spam" /\ "eggs", "cow" /\ "moo" ])
        , expected: "d3:cow3:moo4:spam4:eggse"
        }

    test "Parses `Int`s properly" do
      result <- BEncoding.runParserM BEncoding.parser "i42e"
      assertEqual { actual: result, expected: Right { result: 42, source: "i42e" } }

      result2 <- BEncoding.runParserM BEncoding.parser "i-42e"
      assertEqual { actual: result2, expected: Right { result: -42, source: "i-42e" } }

    test "Parses `String`s properly" do
      result <- BEncoding.runParserM BEncoding.parser "3:foo"
      assertEqual { actual: result, expected: Right { result: "foo", source: "3:foo" } }

      result2 <- BEncoding.runParserM BEncoding.parser "3:bar"
      assertEqual { actual: result2, expected: Right { result: "bar", source: "3:bar" } }

      result3 <- BEncoding.runParserM BEncoding.parser "3:baz"
      assertEqual { actual: result3, expected: Right { result: "baz", source: "3:baz" } }

      result4 <- BEncoding.runParserM BEncoding.parser "4:spam"
      assertEqual { actual: result4, expected: Right { result: "spam", source: "4:spam" } }

    test "Parses `Array`s properly" do
      result <- BEncoding.runParserM BEncoding.parser "li1ei2ei3ee"
      assertEqual { actual: result, expected: Right { result: [ 1, 2, 3 ], source: "li1ei2ei3ee" } }

      result2 <- BEncoding.runParserM BEncoding.parser "l3:foo3:bar3:baze"
      assertEqual
        { actual: result2
        , expected: Right { result: [ "foo", "bar", "baz" ], source: "l3:foo3:bar3:baze" }
        }

    test "Parses `List`s properly" do
      result <- BEncoding.runParserM BEncoding.parser "li1ei2ei3ee"
      assertEqual
        { actual: result, expected: Right { result: 1 : 2 : 3 : nil, source: "li1ei2ei3ee" } }

      result2 <- BEncoding.runParserM BEncoding.parser "l3:foo3:bar3:baze"
      assertEqual
        { actual: result2
        , expected: Right { result: "foo" : "bar" : "baz" : nil, source: "l3:foo3:bar3:baze" }
        }

    test "Parses `Map`s properly" do
      result <- BEncoding.runParserM BEncoding.parser "d3:bari2e3:bazi3e3:fooi1ee"
      assertEqual
        { actual: result
        , expected: Right
            { result: Map.fromFoldable [ "foo" /\ 1, "bar" /\ 2, "baz" /\ 3 ]
            , source: "d3:bari2e3:bazi3e3:fooi1ee"
            }
        }

      result2 <- BEncoding.runParserM BEncoding.parser "d3:baz4:spam3:foo3:bare"
      assertEqual
        { actual: result2
        , expected: Right
            { result: Map.fromFoldable [ "foo" /\ "bar", "baz" /\ "spam" ]
            , source: "d3:baz4:spam3:foo3:bare"
            }
        }

      result3 <- BEncoding.runParserM BEncoding.parser "d3:cow3:moo4:spam4:eggse"
      assertEqual
        { actual: result3
        , expected: Right
            { result: Map.fromFoldable [ "cow" /\ "moo", "spam" /\ "eggs" ]
            , source: "d3:cow3:moo4:spam4:eggse"
            }
        }

      result4 <- BEncoding.runParserM BEncoding.parser "d4:spam4:eggs3:cow3:mooe"
      assertEqual
        { actual: result4
        , expected: Right
            { result: Map.fromFoldable [ "cow" /\ "moo", "spam" /\ "eggs" ]
            , source: "d4:spam4:eggs3:cow3:mooe"
            }
        }

