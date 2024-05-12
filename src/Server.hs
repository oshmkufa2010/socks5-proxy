module Server
  ( runTCPServer,
    socks5Server,
  )
where

import Connection (ConnectionT, runConnectionT)
import Control.Concurrent (forkFinally)
import Control.Concurrent.Async (concurrently_)
import Control.Exception (bracket)
import qualified Control.Exception as E
import Control.Monad (forever, void, when)
import Control.Monad.Except (Except, ExceptT, runExceptT)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Protocol (buildSocks5Connection)
import System.IO.Error (userError)

runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPServer mhost port server = withSocketsDo $ do
  addr <- resolve
  E.bracket (open addr) close loop
  where
    resolve = do
      let hints =
            defaultHints
              { addrFlags = [AI_PASSIVE],
                addrSocketType = Stream
              }
      head <$> getAddrInfo (Just hints) mhost (Just port)
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      setSocketOption sock ReuseAddr 1
      withFdSocket sock setCloseOnExecIfNeeded
      bind sock $ addrAddress addr
      listen sock 1024
      return sock
    loop sock = forever $ do
      (conn, _peer) <- accept sock
      void $ forkFinally (server conn) (const $ gracefulClose conn 5000)

runSockets5Connection :: ConnectionT (ExceptT String IO) a -> Socket -> IO a
runSockets5Connection conn socket = do
  r <- runExceptT (runConnectionT conn socket)
  case r of
    Left e -> ioError $ userError e
    Right a -> pure a

socks5Server :: Socket -> IO ()
socks5Server clientSocket = do
  bracket (runSockets5Connection buildSocks5Connection clientSocket) (`gracefulClose` 5000) $ \socket ->
    concurrently_ (forward clientSocket socket) (forward socket clientSocket)
  where
    forward :: Socket -> Socket -> IO ()
    forward from to = do
      bs <- recv from 4096
      when (BS.length bs > 0) $ do
        sendAll to bs
        forward from to
