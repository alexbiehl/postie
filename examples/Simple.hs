
module Main where

import Web.Postie

import Pipes.ByteString (stdout)

settings :: Settings
settings = defaultSettings {
    settingsTLS = Just $ (tlsSettings "server.crt" "server.key") {
      security = DemandStartTLS
    },
    settingsOnOpen = \sid -> do
      putStrLn $ show sid ++ " session opened"
    ,
    settingsOnClose = \sid -> do
      putStrLn $ show sid ++ " session closed"
  }

main :: IO ()
main = do
    runSettings settings app
  where
    app (Mail sid _ _ body) = do
      putStrLn $ show sid ++ " receiving mail"
      runEffect $ body >-> stdout
      return Accepted
