module Main where

import Server ( socks5Server, Connection )
import System.Environment (getArgs)
import Network.Simple.TCP ( serve, HostPreference(HostIPv4) )
import Control.Concurrent.STM (newTVarIO, readTVar, TVar, readTVarIO, atomically, retry)
import qualified Data.Map as M
import Control.Concurrent (ThreadId, forkIO)
import Control.Monad (join)

main :: IO ()
main = do
  (port : _) <- getArgs
  connList <- newTVarIO M.empty
  cs <- readTVarIO connList
  forkIO $ printConnections cs connList
  serve HostIPv4 port $ \(socket, _) -> socks5Server socket connList

printConnections :: M.Map ThreadId Connection -> TVar (M.Map ThreadId Connection) -> IO ()
printConnections cs connList = do
  join $ atomically $ do
    cs' <- readTVar connList
    if cs == cs'
      then retry
      else return $ do print $ M.size cs'
                       printConnections cs' connList
