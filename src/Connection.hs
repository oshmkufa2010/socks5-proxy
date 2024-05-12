{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Connection (ConnectionT, runConnectionT) where

import Data.ByteString (ByteString)
import Network.Socket (Socket)
import Control.Monad.Reader (ReaderT, MonadReader (..), runReaderT)
import Control.Monad.State (StateT, MonadState (..), evalStateT)
import qualified Data.ByteString as BS
import Control.Monad.IO.Class (liftIO)
import Protocol (MonadConnection (..))
import Control.Monad.Except (MonadError (..))
import Network.Socket.ByteString (sendAll, recv)
import Control.Monad (when)
import Control.Monad.Trans (MonadIO)

newtype ConnectionT m a = ConnectionT (ReaderT Socket (StateT ByteString m) a)
  deriving (Functor, Applicative, Monad, MonadReader Socket, MonadState ByteString, MonadIO)

runConnectionT :: Monad m => ConnectionT m a -> Socket -> m a
runConnectionT (ConnectionT conn) socket = evalStateT (runReaderT conn socket) BS.empty

instance MonadError e m => MonadError e (ConnectionT m) where
  throwError e = ConnectionT $ throwError e
  catchError (ConnectionT m) h = ConnectionT $ catchError m (\e -> case h e of (ConnectionT r) -> r)

instance (MonadIO m) => MonadConnection (ConnectionT m) where
  connPut bs = do
    socket <- ask
    liftIO (sendAll socket bs)

  connGetSome size
    | size <= 0 = return BS.empty
    | otherwise = do
      buffer <- get
      if BS.length buffer >= size
        then do
          put (BS.drop size buffer)
          return (BS.take size buffer)
        else do
          bs <- connGetChunk
          put (BS.append buffer bs)
          connGetSome size

  connGetChunk = do
    socket <- ask
    bs <- liftIO $ recv socket 4096
    when (BS.length bs == 0) $ liftIO $ ioError $ userError $ show socket ++ ": the peer has closed its half side of the connection."
    return bs
