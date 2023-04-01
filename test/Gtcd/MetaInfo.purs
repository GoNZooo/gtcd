module Test.Gtcd.MetaInfo where

import Prelude

import Effect (Effect)

type TestCase a = { left :: a, right :: a }

data Test a
  = EqualTest (TestCase a)
  | LessThanTest (TestCase a)
  | GreaterThanTest (TestCase a)
  | LessThanOrEqualTest (TestCase a)
  | GreaterThanOrEqualTest (TestCase a)
  | NotEqualTest (TestCase a)
  | AssertTest { value :: a }
  | RefuteTest { value :: a }

assertEqual :: forall a. Eq a => a -> a -> Effect (Test a)
assertEqual left right = pure $ EqualTest { left, right }

assertLessThan :: forall a. Ord a => a -> a -> Effect (Test a)
assertLessThan left right = pure $ LessThanTest { left, right }

assertGreaterThan :: forall a. Ord a => a -> a -> Effect (Test a)
assertGreaterThan left right = pure $ GreaterThanTest { left, right }

assertLessThanOrEqual :: forall a. Ord a => a -> a -> Effect (Test a)
assertLessThanOrEqual left right = pure $ LessThanOrEqualTest { left, right }

assertGreaterThanOrEqual :: forall a. Ord a => a -> a -> Effect (Test a)
assertGreaterThanOrEqual left right = pure $ GreaterThanOrEqualTest { left, right }

assertNotEqual :: forall a. Eq a => a -> a -> Effect (Test a)
assertNotEqual left right = pure $ NotEqualTest { left, right }

assert :: Boolean -> Effect (Test Boolean)
assert value = pure $ AssertTest { value }

refute :: Boolean -> Effect (Test Boolean)
refute value = pure $ RefuteTest { value }

test_metaInfo :: Effect (Test String)
test_metaInfo = do
  assertNotEqual "test" "test2"

test_metaInfo2 :: Effect (Test String)
test_metaInfo2 = do
  assertEqual "test" "test"

test_metaInfo3 :: Effect (Test Int)
test_metaInfo3 = do
  assertGreaterThan 2 1

test_metaInfo4 :: Effect (Test Int)
test_metaInfo4 = do
  assertLessThan 1 2

test_metaInfo5 :: Effect (Test Int)
test_metaInfo5 = do
  assertGreaterThanOrEqual 2 1

test_metaInfo5_2 :: Effect (Test Int)
test_metaInfo5_2 = do
  assertGreaterThanOrEqual 1 1

test_metaInfo6 :: Effect (Test Int)
test_metaInfo6 = do
  assertLessThanOrEqual 1 2

test_metaInfo6_2 :: Effect (Test Int)
test_metaInfo6_2 = do
  assertLessThanOrEqual 1 1

test_metaInfo7 :: Effect (Test Boolean)
test_metaInfo7 = do
  assert true

test_metaInfo8 :: Effect (Test Boolean)
test_metaInfo8 = do
  refute false

