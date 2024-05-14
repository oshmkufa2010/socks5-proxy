module Main where

import Server ( socks5Server )
import System.Environment (getArgs)
import Network.Simple.TCP ( serve, HostPreference(HostIPv4) )

main :: IO ()
main = do
  (port : _) <- getArgs
  serve HostIPv4 port $ \(socket, _) -> socks5Server socket
  -- runTCPServer (Just "127.0.0.1") port socks5Server
