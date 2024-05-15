{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}

module Protocol
  (
    withSocks5Conn,
    MonadConnection(..),
  )
where

import qualified Control.Exception as E
import Control.Monad (when)
import Control.Monad.Except
import Data.ByteString (ByteString (..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import Data.Word (Word8 (..))
-- import Network.Socket
import Data.List (intercalate)
import Data.Kind (Type)
import Network.Simple.TCP (connectSock, connect, SockAddr, Socket)
import Control.Monad.Catch (MonadMask, onException)

class MonadConnection (m :: Type -> Type) where
  connGetChunk :: m ByteString
  connGetSome :: Int -> m ByteString
  connPut :: ByteString -> m ()

-- newConnection :: String -> String -> IO Socket
-- newConnection host port = do
--   addr <- resolve
--   open addr
--   where
--     resolve = do
--       let hints = defaultHints {addrSocketType = Stream}
--       head <$> getAddrInfo (Just hints) (Just host) (Just port)
--     open addr = do
--       sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
--       connect sock $ addrAddress addr
--       return sock

recvVerAndMethod :: (MonadConnection m, MonadError String m) => m (Word8, Word8)
recvVerAndMethod = do
  result <- connGetSome 2
  let [ver, methodn] = BS.unpack result
  when (ver /= 0x05) $ throwError ("unsupported ver: " ++ show ver)
  methods <- connGetSome (fromIntegral methodn)
  when (0x00 `notElem` BS.unpack methods) $ throwError "no method supported"
  return (ver, 0x00)

recvTargetAddress :: (MonadConnection m, MonadError String m) => m (Word8, Word8, String, String, ByteString)
recvTargetAddress = do
  bs <- connGetSome 4
  let [ver, cmd, _, atyp] = BS.unpack bs
  (addr, encodedAddr) <- do
    case atyp of
      0x01 -> fmap (\ip -> (toIP ip, ip)) (connGetSome 4)
      0x04 -> fmap (\ip -> (toIP ip, ip)) (connGetSome 16)
      0x03 -> do
        len <- connGetSome 1
        domain <- connGetSome (fromIntegral (BS.head len))
        return (C.unpack domain, BS.append len domain)
      _ -> throwError "unsupported atyp"
  port <- connGetSome 2
  return (cmd, atyp, addr, toPort port, BS.append encodedAddr port)

  where
    toIP :: ByteString -> String
    toIP addr = intercalate "." (show . fromIntegral <$> BS.unpack addr)

    toPort :: ByteString -> String
    toPort port = show $ foldl (\b a -> b * 256 + a) 0 $ fromIntegral <$> BS.unpack port

sendAddress :: (MonadConnection m) => Word8 -> Word8 -> ByteString -> m ()
sendAddress ver atyp addr = connPut $ ver `BS.cons` 0x00 `BS.cons` 0x00 `BS.cons` atyp `BS.cons` addr

sendConnectFailed :: (MonadConnection m) => Word8 -> Word8 -> ByteString -> m ()
sendConnectFailed ver atyp addr = connPut $ ver `BS.cons` 0x01 `BS.cons` 0x00 `BS.cons` atyp `BS.cons` addr

buildSocks5Connection :: (MonadConnection m, MonadError String m, MonadIO m) => m Socket
buildSocks5Connection = do
  (ver, method) <- recvVerAndMethod
  connPut (BS.pack [ver, method])
  (cmd, atyp, addr, port, encodedAddrAndPort) <- recvTargetAddress
  when (cmd /= 0x01) $ throwError ("unsupported cmd: " ++ show cmd)
  mSocket :: Either E.SomeException (Socket, SockAddr) <- liftIO $ E.try $ connectSock addr port
  case mSocket of
    Left e -> do
      connPut $ ver `BS.cons` 0x01 `BS.cons` 0x00 `BS.cons` atyp `BS.cons` encodedAddrAndPort
      throwError $ "connection " ++ addr ++ " " ++ port ++ " error: " ++ show e
    Right (socket, _) -> do
      connPut $ ver `BS.cons` 0x00 `BS.cons` 0x00 `BS.cons` atyp `BS.cons` encodedAddrAndPort
      return socket

withSocks5Conn :: (MonadConnection m, MonadError String m, MonadIO m, MonadMask m) => (Socket -> m a) -> m a
withSocks5Conn f = do
  (ver, method) <- recvVerAndMethod
  connPut (BS.pack [ver, method])
  (cmd, atyp, addr, port, encodedAddrAndPort) <- recvTargetAddress
  when (cmd /= 0x01) $ throwError ("unsupported cmd: " ++ show cmd)
  connect addr port (\(socket, _) -> sendAddress ver atyp encodedAddrAndPort >> f socket) `onException` sendConnectFailed ver atyp encodedAddrAndPort
