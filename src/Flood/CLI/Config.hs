module Flood.CLI.Config where

import Flood.API.Effect
import Options.Applicative

parseFloodConfig :: Parser FloodConfig
parseFloodConfig = do
  url <-
    strOption $ mconcat
      [ long "host"
      , help "Flood API host URL"
      , showDefault
      , value "http://localhost"
      , metavar "HOST"
      ]
  port <-
    option auto $ mconcat
      [ long "port"
      , help "Flood API port"
      , showDefault
      , value 3000
      , metavar "PORT"
      ]
  username <-
    strOption $ mconcat
      [ long "username"
      , help "Flood API username"
      , showDefault
      , value "_config"
      , metavar "USER"
      ]
  password <-
    strOption $ mconcat
      [ long "password"
      , help "Flood API password"
      , showDefault
      , value ""
      , metavar "PASS"
      ]
  pure FloodConfig { .. }
