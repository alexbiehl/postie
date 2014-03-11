
module Main where

import Web.Postie

import Pipes.ByteString (stdout)

main :: IO ()
main = do
    run 8080 app
  where
    app (Mail _ _ body) = do
      runEffect $ body >-> stdout
      return Accepted
