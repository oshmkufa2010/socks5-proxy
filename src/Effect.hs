module Effect(
  SocketEff,
  runSocketEff,
  recvNBytes,
  sendAllBytes,
  throwSocketError,
) where

import Network.Socket
import qualified Data.ByteString as BS
import Data.ByteString (ByteString(..))
import Network.Socket.ByteString (recv, sendAll)
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad (when)
import Control.Exception (IOException)

type SocketEff a = ReaderT Socket (StateT ByteString IO) a

runSocketEff :: SocketEff a -> Socket -> IO a
runSocketEff eff socket = evalStateT (runReaderT eff socket) BS.empty

throwSocketError :: IOException -> SocketEff a
throwSocketError = throwError

recvBytesWithoutBuffer :: SocketEff ByteString
recvBytesWithoutBuffer = do
  socket <- ask
  bs <- liftIO (recv socket 4096)
  when (BS.length bs == 0) $ throwSocketError $ userError $ show socket ++ ": the peer has closed its half side of the connection."
  return bs

recvNBytes :: Int -> SocketEff ByteString
recvNBytes size
  | size <= 0 = return BS.empty
  | otherwise = do
    buffer <- get
    if BS.length buffer >= size
    then do
      put (BS.drop size buffer)
      return (BS.take size buffer)
    else do
      bs <- recvBytesWithoutBuffer
      put (BS.append buffer bs)
      recvNBytes size

sendAllBytes :: ByteString -> SocketEff ()
sendAllBytes bs = do
  socket <- ask
  liftIO (sendAll socket bs)
