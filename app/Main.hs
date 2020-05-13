module Main where

import Server
import System.Environment (getArgs)

main :: IO ()
main = do
  (port : _) <- getArgs
  runTCPServer (Just "127.0.0.1") port socks5Server
