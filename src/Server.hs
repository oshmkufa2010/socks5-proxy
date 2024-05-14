module Server
  ( socks5Server,
  )
where

import Connection (ConnectionT, runConnectionT)
import Control.Concurrent.Async (concurrently_)
import Control.Monad (when)
import Control.Monad.Except (Except, ExceptT, runExceptT)
import Control.Monad.Trans (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Network.Socket (Socket)
import Network.Socket.ByteString (recv, sendAll)
import Protocol (withSocks5Conn)
import System.IO.Error (userError)

runSockets5Connection :: ConnectionT (ExceptT String IO) a -> Socket -> IO a
runSockets5Connection conn socket = do
  r <- runExceptT (runConnectionT conn socket)
  case r of
    Left e -> ioError $ userError e
    Right a -> pure a

socks5Server :: Socket -> IO ()
socks5Server clientSocket = runSockets5Connection (withSocks5Conn $ \socket -> liftIO $ concurrently_ (forward clientSocket socket) (forward socket clientSocket)) clientSocket
  where
    forward :: Socket -> Socket -> IO ()
    forward from to = do
      bs <- recv from 4096
      when (BS.length bs > 0) $ do
        sendAll to bs
        forward from to
