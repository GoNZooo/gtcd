module Test.GtcdSpec
  ( main
  ) where

import Prelude

import Effect (Effect)
import PurerlTest as PurerlTest
import Test.Gtcd.BEncoding as BEncoding

main :: Effect Unit
main = do
  PurerlTest.runSuites do
    BEncoding.testSuite
