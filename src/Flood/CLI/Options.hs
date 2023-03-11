module Flood.CLI.Options where

import Control.Monad.IO.Class
import Data.ByteString qualified as ByteString
import Data.ByteString.Base64
import Data.Char
import Data.List.Split qualified as List
import Data.Text (Text, unpack)
import Optics
import Options.Applicative
import Text.Read

readBool :: ReadM Bool
readBool =
  maybeReader \case
    "true" -> Just True
    "false" -> Just False
    _ -> Nothing

readFileBase64 :: MonadIO m => Text -> m Text
readFileBase64 = fmap encodeBase64 . liftIO . ByteString.readFile . unpack

autoFromKebab :: Read a => ReadM a
autoFromKebab = maybeReader (readMaybe . concat @[] . fmap (_head %~ toUpper) . List.splitOn "-")
