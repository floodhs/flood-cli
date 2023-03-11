module Flood.CLI.Torrents where

import Flood.API.Effect
import Flood.API.Torrents qualified as API
import Flood.CLI.Torrents.AddFiles qualified as CLI.AddFiles
import Flood.CLI.Torrents.AddUrls qualified as CLI.AddUrls
import Flood.CLI.Torrents.List qualified as CLI.List
import Flood.CLI.Torrents.Search qualified as CLI.Search
import GHC.Generics
import Options.Applicative

data Command
  = List
  | Search CLI.Search.Settings
  | AddUrls API.AddUrlsSettings
  | AddFiles API.AddFilesSettings
  deriving (Generic, Show)

execute :: Command -> FloodT IO ()
execute = \case
  List -> CLI.List.execute
  Search settings -> CLI.Search.execute settings
  AddUrls settings -> CLI.AddUrls.execute settings
  AddFiles settings -> CLI.AddFiles.execute settings

parseCommand :: Parser Command
parseCommand =
  hsubparser $ mconcat
    [ command "list" $ info
        (pure List)
        (progDesc "List all torrents in CSV format")
    , command "search" $ info
        (fmap Search CLI.Search.parseSettings)
        (progDesc "Look up torrent hashes by search filters")
    , command "add-urls" $ info
        (fmap AddUrls CLI.AddUrls.parseSettings)
        (progDesc "Add torrents by URLs")
    , command "add-files" $ info
        (fmap AddFiles CLI.AddFiles.parseSettings)
        (progDesc "Add torrent files from filesystem")
    ]
