module Test.Gtcd.BEncoding
  ( testSuite
  ) where

import Prelude

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

