module Main where

import Data.Text.Lazy qualified as Text.Lazy
import Data.Text.IO
import Flood.API.Auth qualified as API.Auth
import Flood.API.Effect
import Flood.CLI.Config
import Flood.CLI.Command
import Options.Applicative
import Text.Pretty.Simple
import UnliftIO.Exception
import UnliftIO.IO

main :: IO ()
main = do
  (config, cmd) <- customExecParser (prefs noBacktrack) parseMain
  runFloodT config (API.Auth.authenticate *> execute cmd)
    `catch` \(e :: SomeException) -> hPutStrLn stderr $ Text.Lazy.toStrict $ pShow e

parseMain :: ParserInfo (FloodConfig, Command)
parseMain =
  info (helper <*> liftA2 (,) parseFloodConfig parseCommand)
    ( fullDesc
    <> progDesc "Command-line interface for Flood torrent manager"
    )
