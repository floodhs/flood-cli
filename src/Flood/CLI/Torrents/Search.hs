module Flood.CLI.Torrents.Search where

import Control.Applicative.Combinators (choice)
import Data.Coerce
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock
import Data.Time.Format.ISO8601
import Data.Foldable qualified as Foldable
import Flood.API.Effect
import Flood.API.Time qualified as API
import Flood.API.Torrents qualified as API
import Flood.CLI.Interaction qualified as CLI
import Flood.CLI.Options
import GHC.Generics
import Options.Applicative

execute :: Settings -> FloodT IO ()
execute filters = do
  infos <- fmap (Foldable.toList . (.torrents)) API.list
  mapM_ CLI.output
    [ info.hash.text
    | info <- infos
    , all (testFilter info) filters
    ]

type Settings = [Filter]

data Filter
  = Name Text
  | Tag Text
  | TrackerURI Text
  | Directory Text
  | Status API.Status
  | Active DateFilter
  | Added DateFilter
  | Created DateFilter
  | Finished DateFilter
  deriving (Generic, Show)

data DateFilter
  = After UTCTime
  | Before UTCTime
  deriving (Generic, Show)

testFilter :: API.Info -> Filter -> Bool
testFilter info = \case
  Name name -> name `Text.isInfixOf` info.name
  Tag tag -> tag `elem` fmap coerce info.tags
  TrackerURI trackerURI -> trackerURI `elem` fmap coerce info.trackerURIs
  Directory dir -> dir `Text.isPrefixOf` info.directory
  Status status -> status `elem` info.status
  Active date -> testDateFilter (coerce info.dateActive) date
  Added date -> testDateFilter (coerce info.dateAdded) date
  Created date -> testDateFilter (coerce info.dateCreated) date
  Finished date -> testDateFilter (coerce info.dateCreated) date

testDateFilter :: Maybe UTCTime -> DateFilter -> Bool
testDateFilter = \case
  Nothing -> pure False
  Just date -> \case
    After earlier -> earlier < date
    Before later -> later > date

parseSettings :: Parser [Filter]
parseSettings = do
  many $ choice
    [ fmap Name $ strOption $ mconcat
      [ short 'N'
      , long "name"
      , help "Torrent name (substring match)"
      , metavar "NAME"
      ]
    , fmap Tag $ strOption $ mconcat
      [ short 'T'
      , long "tag"
      , help "Tag (exact match)"
      , metavar "TAG"
      ]
    , fmap TrackerURI $ strOption $ mconcat
      [ short 'D'
      , long "tracker-domain"
      , help "Tracker domain name (exact match)"
      , metavar "URL"
      ]
    , fmap Directory $ strOption $ mconcat
      [ short 'D'
      , long "directory"
      , help "Torrent destination directory (prefix match)"
      , metavar "DIR"
      , action "directory"
      ]
    , fmap Status $ option autoFromKebab $ mconcat
      [ short 'S'
      , long "status"
      , help "Torrent status (exact match)"
      , metavar "STATUS"
      , completeWith ["complete", "seeding", "active", "inactive", "stopped", "checking", "error"]
      ]
    , fmap (Active . After) $ option (maybeReader iso8601ParseM) $ mconcat
      [ long "active-after"
      , help "Active after timestamp (ISO 8601)"
      , metavar "TIME"
      ]
    , fmap (Active . Before) $ option (maybeReader iso8601ParseM) $ mconcat
      [ long "active-before"
      , help "Active before timestamp (ISO 8601)"
      , metavar "TIME"
      ]
    , fmap (Added . After) $ option (maybeReader iso8601ParseM) $ mconcat
      [ long "added-after"
      , help "Added after timestamp (ISO 8601)"
      , metavar "TIME"
      ]
    , fmap (Added . Before) $ option (maybeReader iso8601ParseM) $ mconcat
      [ long "added-before"
      , help "Added before timestamp (ISO 8601)"
      , metavar "TIME"
      ]
    , fmap (Created . After) $ option (maybeReader iso8601ParseM) $ mconcat
      [ long "created-after"
      , help "Created after timestamp (ISO 8601)"
      , metavar "TIME"
      ]
    , fmap (Created . Before) $ option (maybeReader iso8601ParseM) $ mconcat
      [ long "created-before"
      , help "Created before timestamp (ISO 8601)"
      , metavar "TIME"
      ]
    , fmap (Finished . After) $ option (maybeReader iso8601ParseM) $ mconcat
      [ long "finished-after"
      , help "Finished after timestamp (ISO 8601)"
      , metavar "TIME"
      ]
    , fmap (Finished . Before) $ option (maybeReader iso8601ParseM) $ mconcat
      [ long "finished-before"
      , help "Finished before timestamp (ISO 8601)"
      , metavar "TIME"
      ]
    ]
