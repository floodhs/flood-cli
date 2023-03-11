module Flood.CLI.Command where

import Flood.API.Effect
import Flood.CLI.Auth qualified as Auth
import Flood.CLI.Torrents qualified as Torrents
import GHC.Generics
import Options.Applicative

data Command
  = Auth Auth.Command
  | Torrents Torrents.Command
  deriving (Generic, Show)

execute :: Command -> FloodT IO ()
execute = \case
  Auth cmd -> Auth.execute cmd
  Torrents cmd -> Torrents.execute cmd

parseCommand :: Parser Command
parseCommand =
  hsubparser $ mconcat
    [ command "auth" $ info
        (Auth <$> Auth.parseCommand)
        (progDesc "Manage users")
    , command "torrents" $ info
        (Torrents <$> Torrents.parseCommand)
        (progDesc "Manage torrents")
    ]
