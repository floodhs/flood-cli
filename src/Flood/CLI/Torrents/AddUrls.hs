module Flood.CLI.Torrents.AddUrls where

import Data.List.NonEmpty (nonEmpty)
import Data.List.NonEmpty qualified as List.NonEmpty
import Flood.API.Effect
import Flood.API.Torrents qualified as API
import Flood.CLI.Interaction qualified as CLI
import Flood.CLI.Options
import Options.Applicative

execute :: API.AddUrlsSettings -> FloodT IO ()
execute settings = do
  API.addUrls settings
  CLI.ok

parseSettings :: Parser API.AddUrlsSettings
parseSettings = do
  urls <-
    fmap List.NonEmpty.fromList $ some $ strArgument $ mconcat
      [ help "Magnet link or .torrent URL"
      , metavar "URL"
      ]
  destination <-
    optional $ strOption $ mconcat
      [ short 'd'
      , long "destination"
      , help "Destination folder"
      , metavar "FOLDER"
      ]
  cookies <-
    fmap nonEmpty $ many $ strOption $ mconcat
      [ short 'C'
      , long "cookie"
      , help "Cookie for getting torrent metainfo, repeat option for multiple"
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
  pure API.AddUrlsSettings { .. }
