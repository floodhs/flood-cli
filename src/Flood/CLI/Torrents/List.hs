module Flood.CLI.Torrents.List where

import Data.ByteString.Lazy qualified as ByteString.Lazy
import Data.Csv qualified as Csv
import Data.Foldable qualified as Foldable
import Data.Text.Encoding
import Data.Text.Encoding.Error
import Flood.API.Effect
import Flood.API.Time qualified as API
import Flood.API.Torrents qualified as API
import Flood.CLI.Interaction qualified as CLI

execute :: FloodT IO ()
execute = do
  infos <- fmap (Foldable.toList . (.torrents)) API.list
  let csv = Csv.encodeDefaultOrderedByName infos
  CLI.output $ decodeUtf8With strictDecode $ ByteString.Lazy.toStrict $ csv

deriving newtype instance Csv.ToField API.Hash
deriving newtype instance Csv.ToField API.NominalDiffFloodTime
deriving newtype instance Csv.ToField API.Tag
deriving newtype instance Csv.ToField API.TrackerURI
deriving newtype instance Csv.ToField API.UTCFloodTime
deriving via CLI.CsvShow API.Status instance Csv.ToField API.Status
deriving via CLI.CsvSemicolonList API.Status instance Csv.ToField [API.Status]
deriving via CLI.CsvSemicolonList API.Tag instance Csv.ToField [API.Tag]
deriving via CLI.CsvSemicolonList API.TrackerURI instance Csv.ToField [API.TrackerURI]
deriving via CLI.CsvSemicolonList API.UTCFloodTime instance Csv.ToField [API.UTCFloodTime]
deriving via CLI.CsvSemicolonList API.NominalDiffFloodTime instance Csv.ToField [API.NominalDiffFloodTime]
deriving anyclass instance Csv.ToNamedRecord API.Info
deriving anyclass instance Csv.DefaultOrdered API.Info
