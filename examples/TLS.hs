
module Main where

import Web.Postie

import Pipes.ByteString (stdout)

settings :: Settings
settings = def {
    settingsOnOpen = \sid _ -> do
      putStrLn $ show sid ++ " session opened"
    ,
    settingsOnClose = \sid -> do
      putStrLn $ show sid ++ " session closed"
    ,
    settingsOnMailFrom = \sid addr -> do
      putStrLn $ show sid ++ " mail from " ++ show addr
      return Accepted
    ,
    settingsOnRecipient = \sid addr -> do
      putStrLn $ show sid ++ " rcpt to " ++ show addr
      return Accepted
    ,
    settingsOnStartTLS = \sid -> do
      putStrLn $ show sid ++ " starttls"

    ,
    settingsTLS = Just def {
      certFile = "server.crt"
    , keyFile  = "server.key"
    }

  }

main :: IO ()
main = do
    runSettings settings app
  where
    app (Mail sid _ _ body) = do
      putStrLn $ show sid ++ " data"
      runEffect $ body >-> stdout
      return Accepted
