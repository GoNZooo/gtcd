module Gtcd.BEncoding
  ( class BEncoding
  , encode
  , parser
  , runParserM
  , parseTorrentFile
  , ParseTorrentError(..)
  , InfoMap(..)
  , FileInfo(..)
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (ExceptT(..))
import Control.Monad.Except as Except
import Control.Monad.Reader (runReaderT)
import Control.Monad.Reader.Class (ask)
import Control.Monad.Reader.Trans (ReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Either as Either
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.List.Lazy (replicateM)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.String.CodeUnits as CodeUnits
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Ref (Ref, modify_)
import Effect.Ref as Ref
import Erl.Data.Binary (Binary)
import Erl.Data.List (List)
import Erl.Data.List as List
import Erl.Data.List.NonEmpty as NonEmptyList
import Erl.Data.Map (Map)
import Erl.Data.Map as Map
import Erl.Kernel.File as File
import Text.Parsing.Parser (ParseError, ParserT, fail, runParserT)
import Text.Parsing.Parser.Combinators (choice, many1)
import Text.Parsing.Parser.String as StringParsing
import Text.Parsing.Parser.Token as Token

type ParserM a = ParserT String (ReaderT (Ref String) Effect) a

data ParseTorrentError
  = TorrentParsingError ParseError
  | TorrentFileError File.FileError
  | TorrentInfoMapError

derive instance Generic ParseTorrentError _

instance Show ParseTorrentError where
  show = genericShow

-- = Eof
-- | BadArg
-- | SystemLimit
-- | Terminated
-- | NoTranslation
-- | Posix PosixError
-- | Other Foreign

instance Eq ParseTorrentError where
  eq (TorrentParsingError e1) (TorrentParsingError e2) = e1 == e2
  eq (TorrentFileError File.Eof) (TorrentFileError File.Eof) = true
  eq (TorrentFileError File.BadArg) (TorrentFileError File.BadArg) = true
  eq (TorrentFileError File.SystemLimit) (TorrentFileError File.SystemLimit) = true
  eq (TorrentFileError File.Terminated) (TorrentFileError File.Terminated) = true
  eq (TorrentFileError File.NoTranslation) (TorrentFileError File.NoTranslation) = true
  eq (TorrentFileError (File.Posix e1)) (TorrentFileError (File.Posix e2)) = e1 == e2
  eq (TorrentFileError (File.Other _e1)) (TorrentFileError (File.Other _e2)) = true
  eq TorrentInfoMapError TorrentInfoMapError = true
  eq _a _b = false

data InfoMap
  = FileInfoMap { pieces :: List String, pieceLength :: Int, length :: Int }
  | MultiFileInfoMap { pieces :: List String, pieceLength :: Int, files :: List FileInfo }

derive instance Eq InfoMap
derive instance Generic InfoMap _

instance Show InfoMap where
  show = genericShow

type FileInfo = { path :: List String, length :: Int }

type ParseResult a = { result :: a, source :: String }

data BEncodingValue
  = BEncodingInt Int
  | BEncodingString String
  | BEncodingList (List BEncodingValue)
  | BEncodingMap (Map String BEncodingValue)

derive instance Eq BEncodingValue
derive instance Generic BEncodingValue _

instance Show BEncodingValue where
  show (BEncodingInt i) = "BEncodingInt " <> show i
  show (BEncodingString s) = "BEncodingString " <> show s
  show (BEncodingList l) = "BEncodingList " <> show l
  show (BEncodingMap m) = "BEncodingMap " <> show m

-- | Looks for the `4:info` section of a binary so that we can decode the info map.
foreign import findInfoMap :: Binary -> Maybe String

parseTorrentFile :: String -> Effect (Either ParseTorrentError (ParseResult InfoMap))
parseTorrentFile path = Except.runExceptT do
  fileData <- ExceptT $ lmap TorrentFileError <$> File.readFile (wrap path)
  -- Console.log $ "fileData: " <> UnsafeCoerce.unsafeCoerce fileData
  infoMapBinary <- fileData # findInfoMap # Either.note TorrentInfoMapError # Except.except
  -- Console.log $ "infoMapBinary: " <> show infoMapBinary
  parseResult :: (ParseResult InfoMap) <-
    infoMapBinary # runParserM parser # map (lmap TorrentParsingError) # ExceptT
  pure parseResult

runParserM
  :: forall a. ParserM a -> String -> Effect (Either ParseError (ParseResult a))
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

instance BEncoding InfoMap where
  encode (FileInfoMap { pieces, pieceLength, length }) =
    [ "pieces" /\ BEncodingList (map BEncodingString pieces)
    , "piece length" /\ BEncodingInt pieceLength
    , "length" /\ BEncodingInt length
    ]
      # Map.fromFoldable
      # encode
  encode (MultiFileInfoMap { pieces, pieceLength, files }) = do
    let
      encodeFile { path, length } =
        [ "path" /\ BEncodingList (map BEncodingString path)
        , "length" /\ BEncodingInt length
        ]
          # Map.fromFoldable
          # BEncodingMap
    [ "pieces" /\ BEncodingList (map BEncodingString pieces)
    , "piece length" /\ BEncodingInt pieceLength
    , "files" /\ BEncodingList (map encodeFile files)
    ]
      # Map.fromFoldable
      # encode
  parser = do
    result <- parser
    m <- asMap result
    maybePieces <- m # Map.lookup "pieces" # traverse parsePieces
    maybePieceLength <- m # Map.lookup "piece length" # traverse asInt
    maybeFiles <- (m # Map.lookup "files" # traverse parseFiles) <|> pure Nothing
    maybeLength <- (m # Map.lookup "length" # traverse asInt) <|> pure Nothing
    case maybePieces, maybePieceLength, maybeFiles, maybeLength of
      Just pieces, Just pieceLength, Nothing, Just length -> do
        pure $ FileInfoMap { pieces, pieceLength, length }
      Just pieces, Just pieceLength, Just files, Nothing -> do
        pure $ MultiFileInfoMap { pieces, pieceLength, files }
      _, _, _, _ -> fail "Invalid info map"

parsePieces :: BEncodingValue -> ParserM (List String)
parsePieces = asList >=> traverse asString

parseFiles :: BEncodingValue -> ParserM (List FileInfo)
parseFiles = asList >=> traverse parseFile

parseFile :: BEncodingValue -> ParserM FileInfo
parseFile = asMap >=> parseFileMap

parseFileMap :: Map String BEncodingValue -> ParserM FileInfo
parseFileMap m = do
  maybePath <- m # Map.lookup "path" # traverse (asList >=> traverse asString)
  maybeLength <- m # Map.lookup "length" # traverse asInt
  case maybePath, maybeLength of
    Just path, Just length -> pure { path, length }
    _, _ -> fail "Invalid file map"

asMap :: BEncodingValue -> ParserM (Map String BEncodingValue)
asMap (BEncodingMap m) = pure m
asMap _ = fail "Expected BEncodingMap"

asList :: BEncodingValue -> ParserM (List BEncodingValue)
asList (BEncodingList xs) = pure xs
asList _ = fail "Expected BEncodingList"

asInt :: BEncodingValue -> ParserM Int
asInt (BEncodingInt x) = pure x
asInt _ = fail "Expected BEncodingInt"

asString :: BEncodingValue -> ParserM String
asString (BEncodingString x) = pure x
asString _ = fail "Expected BEncodingString"

-- expectList :: forall a. BEncodingValue -> Maybe (List BEncodingValue)
-- expectList (BEncodingList xs) = Just xs
-- expectList _ = Nothing

parseBEncodingValue :: ParserM BEncodingValue
parseBEncodingValue = do
  choice
    [ BEncodingInt <$> parser
    , BEncodingString <$> parser
    , BEncodingList <$> parseBEncodingList
    , BEncodingMap <$> parseBEncodingMap
    ]
  where
  parseBEncodingList :: ParserM (List BEncodingValue)
  parseBEncodingList = do
    _ <- char 'l'
    values <- Array.many parseBEncodingValue
    _ <- char 'e'
    pure $ List.fromFoldable values

  parseBEncodingMap :: ParserM (Map String BEncodingValue)
  parseBEncodingMap = do
    _ <- char 'd'
    pairs <- Array.many keyValueP
    _ <- char 'e'
    pure $ Map.fromFoldable pairs

instance BEncoding BEncodingValue where
  encode (BEncodingInt x) = encode x
  encode (BEncodingString s) = encode s
  encode (BEncodingList xs) = encode xs
  encode (BEncodingMap m) = encode m

  parser = parseBEncodingValue

integerP :: ParserM Int
integerP = do
  sign <- char '-' *> pure (-1) <|> pure 1
  digits <- many1 digit
  let
    asString' =
      digits
        # map String.codePointFromChar
        # Array.fromFoldable
        # String.fromCodePointArray
  case Int.fromString asString' of
    Just x -> pure (sign * x)
    Nothing -> fail $ "Invalid integer: " <> asString'

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
    replicateM length byte

byte :: ParserM Char
byte = do
  c <- StringParsing.satisfy (const true)
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
