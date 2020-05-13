module Server
  (
    runTCPServer,
    socks5Server
  ) where

import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import qualified Control.Exception as E
import Control.Monad (void, forever)
import Control.Concurrent
import Control.Concurrent.Async
import Protocol (buildSocks5Connection, runSocketEff)
import qualified Data.ByteString as BS
import System.IO.Error (userError)

runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPServer mhost port server = withSocketsDo $ do
  addr <- resolve
  E.bracket (open addr) close loop
  where
    resolve = do
      let hints = defaultHints {
        addrFlags = [AI_PASSIVE]
        , addrSocketType = Stream
      }
      head <$> getAddrInfo (Just hints) mhost (Just port)
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      setSocketOption sock ReuseAddr 1
      withFdSocket sock $ setCloseOnExecIfNeeded
      bind sock $ addrAddress addr
      listen sock 1024
      return sock
    loop sock = forever $ do
      (conn, _peer) <- accept sock
      void $ forkFinally (server conn) (const $ gracefulClose conn 5000)

socks5Server :: Socket -> IO ()
socks5Server clientSocket = do
  mRemoteSocket <- runSocketEff buildSocks5Connection clientSocket
  case mRemoteSocket of
    Left msg -> do 
      E.ioError (userError msg)
    Right socket -> do
      a <- async (forward clientSocket socket)
      b <- async (forward socket clientSocket)
      waitCatch a
      waitCatch b
      gracefulClose socket 5000
  where 
    forward :: Socket -> Socket -> IO ()
    forward from to = do
      bs <- recv from 4096
      if BS.length bs > 0
      then do
        sendAll to bs
        forward from to
      else
        return ()
