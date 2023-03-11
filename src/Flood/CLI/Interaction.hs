module Flood.CLI.Interaction where

import Control.Monad.IO.Class
import Data.Char
import Data.Coerce
import Data.Csv
import Data.List qualified as List
import Data.Text (Text, pack)
import Data.Text.Encoding
import Data.Text.IO qualified as Text
import Data.Time.Clock
import Data.Time.Clock.POSIX
import GHC.Generics
import Optics
import System.IO (stderr)

ok :: MonadIO m => m ()
ok = liftIO $ Text.putStrLn "ok"

output, debug, info, warn, error :: MonadIO m => Text -> m ()
output = liftIO . Text.putStrLn
debug = liftIO . Text.hPutStrLn stderr
info = liftIO . Text.hPutStrLn stderr
warn = liftIO . Text.hPutStrLn stderr
error = liftIO . Text.hPutStrLn stderr

newtype CsvShow a = CsvShow a
  deriving (Generic, Generic1)

instance Show a => ToField (CsvShow a) where
  toField = encodeUtf8 . pack . (_head %~ toLower) . show @a . coerce

newtype CsvSemicolonList a = CsvSemicolonList [a]
  deriving (Generic, Generic1)

instance ToField a => ToField (CsvSemicolonList a) where
  toField = mconcat . List.intersperse ";" . fmap (toField @a) . coerce

deriving via CsvShow Bool instance ToField Bool

instance ToField NominalDiffTime where
  toField = toField . show . nominalDiffTimeToSeconds

instance ToField UTCTime where
  toField = toField . utcTimeToPOSIXSeconds
