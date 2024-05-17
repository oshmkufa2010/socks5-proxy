module Server
  ( socks5Server,
    Connection(..),
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
import Control.Concurrent.STM (modifyTVar, STM, TVar, atomically)
import Control.Concurrent (myThreadId, ThreadId)
import qualified Data.Map as M
import GHC.Conc (ThreadId(ThreadId))
import Control.Monad.Catch (finally)

data Connection = Connection { clientSocket :: Socket, serverSocket :: Socket, threadId :: ThreadId  } deriving Eq

runSockets5Connection :: ConnectionT (ExceptT String IO) a -> Socket -> IO a
runSockets5Connection conn socket = do
  r <- runExceptT (runConnectionT conn socket)
  case r of
    Left e -> ioError $ userError $ "error from protocal: " ++ e
    Right a -> pure a

socks5Server :: Socket -> TVar (M.Map ThreadId Connection) -> IO ()
socks5Server clientSocket connList = do
  threadId <- myThreadId

  let conn = withSocks5Conn $ \socket -> liftIO $ do let c = Connection { clientSocket = clientSocket, serverSocket = socket, threadId = threadId }
                                                     atomically $ modifyTVar connList (M.insert threadId c)
                                                     concurrently_ (forward clientSocket socket) (forward socket clientSocket)

  runSockets5Connection conn clientSocket `finally` do atomically $ modifyTVar connList (M.delete threadId)
  where
    forward :: Socket -> Socket -> IO ()
    forward from to = do
      bs <- recv from 4096
      when (BS.length bs > 0) $ do
        sendAll to bs
        forward from to
