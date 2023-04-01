module Test.Gtcd.BEncoding
  ( testSuite
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import Erl.Data.List (nil, (:))
import Erl.Data.Map as Map
import Gtcd.BEncoding as BEncoding
import PurerlTest (assertEqual, suite, test)
import PurerlTest.Types (Suites)

testSuite :: Suites
testSuite = do
  suite "BEncoding encoding and decoding" do
    test "Encodes `Int`s properly" do
      assertEqual (BEncoding.encode 42) "i42e"
      assertEqual (BEncoding.encode (-42)) "i-42e"

    test "Encodes `String`s properly" do
      assertEqual (BEncoding.encode "foo") "3:foo"
      assertEqual (BEncoding.encode "bar") "3:bar"
      assertEqual (BEncoding.encode "baz") "3:baz"
      assertEqual (BEncoding.encode "spam") "4:spam"

    test "Encodes `Array`s properly" do
      assertEqual (BEncoding.encode [ 1, 2, 3 ]) "li1ei2ei3ee"
      assertEqual (BEncoding.encode [ "foo", "bar", "baz" ]) "l3:foo3:bar3:baze"

    test "Encodes `List`s properly" do
      assertEqual (BEncoding.encode (1 : 2 : 3 : nil)) "li1ei2ei3ee"
      assertEqual
        (BEncoding.encode ("foo" : "bar" : "baz" : nil))
        "l3:foo3:bar3:baze"

    test "Encodes `Map`s properly" do
      assertEqual
        (BEncoding.encode (Map.fromFoldable [ "foo" /\ 1, "bar" /\ 2, "baz" /\ 3 ]))
        "d3:bari2e3:bazi3e3:fooi1ee"

      assertEqual
        (BEncoding.encode (Map.fromFoldable [ "foo" /\ "bar", "baz" /\ "spam" ]))
        "d3:baz4:spam3:foo3:bare"

      assertEqual
        (BEncoding.encode (Map.fromFoldable [ "cow" /\ "moo", "spam" /\ "eggs" ]))
        "d3:cow3:moo4:spam4:eggse"

      assertEqual
        (BEncoding.encode (Map.fromFoldable [ "spam" /\ "eggs", "cow" /\ "moo" ]))
        "d3:cow3:moo4:spam4:eggse"

    test "Parses `Int`s properly" do
      result <- liftEffect $ BEncoding.runParserM BEncoding.parser "i42e"
      assertEqual result (Right { result: 42, source: "i42e" })

      result2 <- liftEffect $ BEncoding.runParserM BEncoding.parser "i-42e"
      assertEqual result2 (Right { result: -42, source: "i-42e" })

    test "Parses `String`s properly" do
      result <- liftEffect $ BEncoding.runParserM BEncoding.parser "3:foo"
      assertEqual result (Right { result: "foo", source: "3:foo" })

      result2 <- liftEffect $ BEncoding.runParserM BEncoding.parser "3:bar"
      assertEqual result2 (Right { result: "bar", source: "3:bar" })

      result3 <- liftEffect $ BEncoding.runParserM BEncoding.parser "3:baz"
      assertEqual result3 (Right { result: "baz", source: "3:baz" })

      result4 <- liftEffect $ BEncoding.runParserM BEncoding.parser "4:spam"
      assertEqual result4 (Right { result: "spam", source: "4:spam" })

    test "Parses `Array`s properly" do
      result <- liftEffect $ BEncoding.runParserM BEncoding.parser "li1ei2ei3ee"
      assertEqual result (Right { result: [ 1, 2, 3 ], source: "li1ei2ei3ee" })

      result2 <- liftEffect $ BEncoding.runParserM BEncoding.parser "l3:foo3:bar3:baze"
      assertEqual
        result2
        (Right { result: [ "foo", "bar", "baz" ], source: "l3:foo3:bar3:baze" })

    test "Parses `List`s properly" do
      result <- liftEffect $ BEncoding.runParserM BEncoding.parser "li1ei2ei3ee"
      assertEqual
        result
        (Right { result: 1 : 2 : 3 : nil, source: "li1ei2ei3ee" })

      result2 <- liftEffect $ BEncoding.runParserM BEncoding.parser "l3:foo3:bar3:baze"
      assertEqual
        result2
        (Right { result: "foo" : "bar" : "baz" : nil, source: "l3:foo3:bar3:baze" })

    test "Parses `Map`s properly" do
      result <- liftEffect $ BEncoding.runParserM BEncoding.parser "d3:bari2e3:bazi3e3:fooi1ee"
      assertEqual
        result
        ( Right
            { result: Map.fromFoldable [ "foo" /\ 1, "bar" /\ 2, "baz" /\ 3 ]
            , source: "d3:bari2e3:bazi3e3:fooi1ee"
            }
        )

      result2 <- liftEffect $ BEncoding.runParserM BEncoding.parser "d3:baz4:spam3:foo3:bare"
      assertEqual
        result2
        ( Right
            { result: Map.fromFoldable [ "foo" /\ "bar", "baz" /\ "spam" ]
            , source: "d3:baz4:spam3:foo3:bare"
            }
        )

      result3 <- liftEffect $ BEncoding.runParserM BEncoding.parser "d3:cow3:moo4:spam4:eggse"
      assertEqual
        result3
        ( Right
            { result: Map.fromFoldable [ "cow" /\ "moo", "spam" /\ "eggs" ]
            , source: "d3:cow3:moo4:spam4:eggse"
            }
        )

      result4 <- liftEffect $ BEncoding.runParserM BEncoding.parser "d4:spam4:eggs3:cow3:mooe"
      assertEqual
        result4
        ( Right
            { result: Map.fromFoldable [ "cow" /\ "moo", "spam" /\ "eggs" ]
            , source: "d4:spam4:eggs3:cow3:mooe"
            }
        )

