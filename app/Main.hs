module Main where

import Server ( socks5Server, Connection (..) )
import System.Environment (getArgs)
import Network.Simple.TCP ( serve, HostPreference(HostIPv4) )
import Control.Concurrent.STM (newTVarIO, readTVar, TVar, readTVarIO, atomically, check)
import qualified Data.Map as M
import Control.Concurrent (ThreadId, forkIO)
import Control.Monad (join, forM_)
import Network.Socket (getSocketName, getPeerName)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Catch.Pure (MonadMask, finally)


main :: IO ()
main = do
  (port : _) <- getArgs
  connList <- newTVarIO M.empty
  cs <- readTVarIO connList
  forkIO $ printConnections connList
  serve HostIPv4 port $ \(socket, _) -> socks5Server socket connList

printConnections :: TVar (M.Map ThreadId Connection) -> IO ()
printConnections connList = do
  watch connList $ \cs -> do
                             putStrLn $ "connections count: " ++ show (M.size cs)
                            --  forM_ (M.elems cs) printConnection

printConnection :: Connection -> IO ()
printConnection Connection { serverSocket = serverSocket, clientSocket = clientSocket, threadId = threadId } = do
  serverAddr <- getPeerName serverSocket
  clientAddr <- getPeerName clientSocket
  putStrLn $ "ThreadId: " ++ show threadId ++ ", server addr: " ++ show serverAddr ++ ", client addr: " ++ show clientAddr

watch :: (MonadIO m, MonadMask m, Eq a) => TVar a -> (a -> m ()) -> m ()
watch tvar f = do
  a <- liftIO $ readTVarIO tvar
  f a `finally` loop tvar a f

  where
    loop :: (MonadIO m, MonadMask m, Eq a) => TVar a -> a -> (a -> m ()) -> m ()
    loop tvar a f = do
      new <- liftIO $ atomically $ do
        a' <- readTVar tvar
        check (a /= a')
        return a'
      f new `finally` loop tvar new f
