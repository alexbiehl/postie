module Main where

import Network.Mail.Postie
import Pipes.ByteString (stdout)

settings :: Settings
settings =
  def
    { settingsOnOpen = \sid _ -> putStrLn $ show sid ++ " session opened",
      settingsOnClose = \sid -> putStrLn $ show sid ++ " session closed",
      settingsOnMailFrom = \sid addr -> do
        putStrLn $ show sid ++ " mail from " ++ show addr
        return Accepted,
      settingsOnRecipient = \sid addr -> do
        putStrLn $ show sid ++ " rcpt to " ++ show addr
        return Accepted,
      settingsOnStartTLS = \sid -> putStrLn $ show sid ++ " starttls",
      settingsTLS =
        Just
          def
            { certFile = "examples/tls/server.crt",
              keyFile = "examples/tls/server.key"
            }
    }

main :: IO ()
main = runSettings settings app
  where
    app (Mail sid _ _ _ body) = do
      putStrLn $ show sid ++ " data"
      runEffect $ body >-> stdout
      return Accepted
