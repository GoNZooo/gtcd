module Gtcd.MetaInfo
  ( Torrent(..)
  , InfoMap(..)
  , FileInfo(..)
  , AnnounceData(..)
  , parseTorrentFile
  ) where

import Prelude

import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Erl.Data.List (List)
import Erl.Data.Map (Map)
import Erl.Kernel.File (FileError)
import Foreign (Foreign)

-- #{announce => Announce,
--   info =>
--     #{name => Name,
--       piece_length => PieceLength,
--       pieces => Pieces},
--   info_hash => InfoHash,
--   additional_data => AdditionalData}};

type Torrent =
  { announce :: AnnounceData
  , info :: InfoMap
  , info_hash :: String
  , additional_data :: Map String Foreign
  }

data AnnounceData
  = AnnounceString String
  | AnnounceList (List String)

derive instance Eq AnnounceData
derive instance Generic AnnounceData _

instance Show AnnounceData where
  show = genericShow

data InfoMap
  = FileInfoMap { pieces :: List String, pieceLength :: Int, length :: Int }
  | MultiFileInfoMap { pieces :: List String, pieceLength :: Int, files :: List FileInfo }

derive instance Eq InfoMap
derive instance Generic InfoMap _

instance Show InfoMap where
  show = genericShow

type FileInfo = { path :: List String, length :: Int }

foreign import parseTorrentFile :: String -> Effect (Either FileError Torrent)
