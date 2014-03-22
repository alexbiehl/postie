
module Main where

import Web.Postie

import Pipes.ByteString (stdout)

settings :: Settings
settings = defaultSettings {
    settingsTLS = Just $ (tlsSettings "server.crt" "server.key") {
      security = ConnectWithTLS
    }
  }

main :: IO ()
main = do
    runSettings settings app
  where
    app (Mail _ _ body) = do
      runEffect $ body >-> stdout
      return Accepted
