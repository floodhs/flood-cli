module Flood.CLI.Torrents.AddFiles where

import Data.List.NonEmpty (nonEmpty)
import Data.List.NonEmpty qualified as List.NonEmpty
import Flood.API.Effect
import Flood.API.Torrents qualified as API
import Flood.CLI.Interaction qualified as CLI
import Flood.CLI.Options
import Options.Applicative

execute :: API.AddFilesSettings -> FloodT IO ()
execute settings = do
  files <- traverse readFileBase64 settings.files
  API.addFiles settings { API.files }
  CLI.ok

parseSettings :: Parser API.AddFilesSettings
parseSettings = do
  files <-
    fmap List.NonEmpty.fromList $ some $ strArgument $ mconcat
      [ help "Torrent file names"
      , metavar "FILES"
      , action "file"
      ]
  destination <-
    optional $ strOption $ mconcat
      [ short 'd'
      , long "destination"
      , help "Destination folder"
      , metavar "FOLDER"
      ]
  tags <-
    fmap nonEmpty $ many $ strOption $ mconcat
      [ short 'T'
      , long "tag"
      , help "Torrent tag, repeat option for multiple"
      , metavar "TAG"
      ]
  start <-
    optional $ option readBool $ mconcat
      [ short 's'
      , long "start"
      , help "Override torrent start setting"
      , metavar "true|false"
      ]
  isBasePath <-
    optional $ option readBool $ mconcat
      [ long "base-path"
      , help "Override base path setting"
      , metavar "true|false"
      ]
  isCompleted <-
    optional $ option readBool $ mconcat
      [ long "completed"
      , help "Override completion setting"
      , metavar "true|false"
      ]
  isSequential <-
    optional $ option readBool $ mconcat
      [ long "sequential"
      , help "Override sequential seeding setting"
      , metavar "true|false"
      ]
  isInitialSeeding <-
    optional $ option readBool $ mconcat
      [ long "initial"
      , help "Override initial seeding setting"
      , metavar "true|false"
      ]
  pure API.AddFilesSettings { .. }
