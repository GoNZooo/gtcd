module Test.GtcdSpec
  ( main
  ) where

import Prelude

import Effect (Effect)
import PurerlTest as PurerlTest
import Test.Gtcd.BEncoding as BEncoding
import Test.Gtcd.MetaInfo as MetaInfo

main :: Effect Unit
main = do
  PurerlTest.runSuites do
    BEncoding.testSuite
    MetaInfo.testSuite
