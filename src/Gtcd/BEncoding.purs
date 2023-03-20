module Gtcd.BEncoding
  ( class BEncoding
  , encode
  , parser
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Int as Int
import Data.List.Lazy (replicateM)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\))
import Erl.Data.List (List)
import Erl.Data.List as List
import Erl.Data.List.NonEmpty as NonEmptyList
import Erl.Data.Map (Map)
import Erl.Data.Map as Map
import Text.Parsing.Parser (Parser, fail)
import Text.Parsing.Parser.Combinators (many1)
import Text.Parsing.Parser.String (anyChar, char)
import Text.Parsing.Parser.Token (digit)

class BEncoding a where
  encode :: a -> String
  parser :: Parser String a

instance BEncoding Int where
  encode x = "i" <> show x <> "e"
  parser = char 'i' *> integerP <* char 'e'

instance BEncoding String where
  encode s = show (String.length s) <> ":" <> s
  parser = do
    length <- integerP <* char ':'
    (Array.fromFoldable >>> map String.codePointFromChar >>> String.fromCodePointArray) <$>
      replicateM length anyChar

instance BEncoding a => BEncoding (Array a) where
  encode as = "l" <> Array.foldMap encode as <> "e"
  parser = char 'l' *> Array.many parser <* char 'e'

instance BEncoding a => BEncoding (List a) where
  encode as = "l" <> NonEmptyList.foldMap encode as <> "e"
  parser = char 'l' *> (List.fromFoldable <$> Array.many parser) <* char 'e'

instance BEncoding a => BEncoding (Map String a) where
  encode m = do
    let
      encoded =
        m
          # Map.toUnfoldable
          # Array.sortWith Tuple.fst
          # Array.foldMap (\(k /\ v) -> encode k <> encode v)
    "d" <> encoded <> "e"
  parser = do
    pairs <- char 'd' *> Array.many keyValueP <* char 'e'
    pairs # Map.fromFoldable # pure

integerP :: Parser String Int
integerP = do
  sign <- char '-' *> pure (-1) <|> pure 1
  digits <- many1 digit
  let
    asString = digits # map String.codePointFromChar # Array.fromFoldable # String.fromCodePointArray
  case Int.fromString asString of
    Just x -> pure (sign * x)
    Nothing -> fail $ "Invalid integer: " <> asString

keyValueP :: forall a. BEncoding a => Parser String (Tuple String a)
keyValueP = do
  key <- parser
  value <- parser
  pure $ Tuple key value
