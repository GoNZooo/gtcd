module Test.Main
  ( main
  ) where

import Prelude

import Effect (Effect)
import Erl.Test.EUnit as EUnit
import Test.Gtcd.BEncoding as BEncoding

main :: Effect Unit
main = do
  void $ EUnit.runTests do
    BEncoding.testSuite
