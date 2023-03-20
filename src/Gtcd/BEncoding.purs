module Gtcd.BEncoding
  ( class BEncoding
  , encode
  , parser
  , runParserM
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Reader (runReaderT)
import Control.Monad.Reader.Class (ask)
import Control.Monad.Reader.Trans (ReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Array as Array
import Data.Either (Either)
import Data.Int as Int
import Data.List.Lazy (replicateM)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.CodeUnits as CodeUnits
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref (Ref, modify_)
import Effect.Ref as Ref
import Erl.Data.List (List)
import Erl.Data.List as List
import Erl.Data.List.NonEmpty as NonEmptyList
import Erl.Data.Map (Map)
import Erl.Data.Map as Map
import Text.Parsing.Parser (ParseError, ParserT, fail, runParserT)
import Text.Parsing.Parser.Combinators (many1)
import Text.Parsing.Parser.String as StringParsing
import Text.Parsing.Parser.Token as Token

type ParserM a = ParserT String (ReaderT (Ref String) Effect) a

runParserM
  :: forall a. ParserM a -> String -> Effect (Either ParseError { result :: a, source :: String })
runParserM ma input = do
  sourceRef <- Ref.new ""
  parseResult <- runReaderT (runParserT input ma) sourceRef
  source <- Ref.read sourceRef
  pure $ map (\result -> { result, source }) parseResult

class BEncoding a where
  encode :: a -> String
  parser :: ParserM a

instance BEncoding Int where
  encode x = "i" <> show x <> "e"
  parser = do
    result <- char 'i' *> integerP <* char 'e'
    pure result

instance BEncoding String where
  encode s = show (String.length s) <> ":" <> s
  parser = do
    length <- integerP <* char ':'
    stringOfLength length

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

integerP :: ParserM Int
integerP = do
  sign <- char '-' *> pure (-1) <|> pure 1
  digits <- many1 digit
  let
    asString = digits # map String.codePointFromChar # Array.fromFoldable # String.fromCodePointArray
  case Int.fromString asString of
    Just x -> pure (sign * x)
    Nothing -> fail $ "Invalid integer: " <> asString

keyValueP :: forall a. BEncoding a => ParserM (Tuple String a)
keyValueP = do
  key <- parser
  value <- parser
  pure $ Tuple key value

addSource :: String -> ParserM Unit
addSource s = do
  sourceRef <- lift ask
  lift $ liftEffect $ modify_ (_ <> s) sourceRef

stringOfLength :: Int -> ParserM String
stringOfLength length = do
  (Array.fromFoldable >>> map String.codePointFromChar >>> String.fromCodePointArray) <$>
    replicateM length anyChar

anyChar :: ParserM Char
anyChar = do
  c <- StringParsing.anyChar
  addSource (CodeUnits.singleton c)
  pure c

char :: Char -> ParserM Char
char c = do
  StringParsing.char c <* addSource (CodeUnits.singleton c)

digit :: ParserM Char
digit = do
  c <- Token.digit
  addSource (CodeUnits.singleton c)
  pure c
