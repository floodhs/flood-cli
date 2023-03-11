module Flood.CLI.Auth where

import Flood.API.Auth qualified as API
import Flood.API.Effect
import Flood.CLI.Interaction qualified as CLI
import Options.Applicative
import GHC.Generics


data Command
  = Verify
  deriving (Generic, Show, Eq)


parseCommand :: Parser Command
parseCommand =
  hsubparser $ mconcat
    [ command "verify" $ info
        (pure Verify)
        (progDesc "Verify user credentials")
    ]


execute :: Command -> FloodT IO ()
execute = \case

  Verify -> do
    API.verify
    CLI.ok
