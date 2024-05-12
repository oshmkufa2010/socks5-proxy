{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Protocol
  ( buildSocks5Connection,
  )
where

import qualified Control.Exception as E
import Control.Monad (when)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.ByteString (ByteString (..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import Data.Word (Word8 (..))
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Data.List (intercalate)

type SocketEff m = (MonadReader Socket m, MonadState ByteString m, MonadIO m, MonadError String m)

recvBytesWithoutBuffer :: SocketEff m => m ByteString
recvBytesWithoutBuffer = do
  socket <- ask
  bs <- liftIO (recv socket 4096)
  when (BS.length bs == 0) $ throwError $ show socket ++ ": the peer has closed its half side of the connection."
  return bs

recvNBytes :: SocketEff m => Int -> m ByteString
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

sendAllBytes :: SocketEff m => ByteString -> m ()
sendAllBytes bs = do
  socket <- ask
  liftIO (sendAll socket bs)

newConnection :: String -> String -> IO Socket
newConnection host port = do
  addr <- resolve
  open addr
  where
    resolve = do
      let hints = defaultHints {addrSocketType = Stream}
      head <$> getAddrInfo (Just hints) (Just host) (Just port)
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      connect sock $ addrAddress addr
      return sock

toIP :: ByteString -> String
toIP addr = intercalate "." (show . fromIntegral <$> BS.unpack addr)

toPort :: ByteString -> String
toPort port = show $ foldl (\b a -> b * 256 + a) 0 $ fromIntegral <$> BS.unpack port

recvVerAndMethod :: SocketEff m => m (Word8, Word8)
recvVerAndMethod = do
  result <- recvNBytes 2
  let [ver, methodn] = BS.unpack result
  when (ver /= 0x05) $ throwError ("unsupported ver: " ++ show ver)
  methods <- recvNBytes (fromIntegral methodn)
  when (0x00 `notElem` BS.unpack methods) $ throwError "no method supported"
  return (ver, 0x00)

recvTargetAddress :: SocketEff m => m (Word8, Word8, String, String, ByteString)
recvTargetAddress = do
  bs <- recvNBytes 4
  let [ver, cmd, _, atyp] = BS.unpack bs
  (addr, encodedAddr) <- do
    case atyp of
      0x01 -> fmap (\ip -> (toIP ip, ip)) (recvNBytes 4)
      0x04 -> fmap (\ip -> (toIP ip, ip)) (recvNBytes 16)
      0x03 -> do
        len <- recvNBytes 1
        domain <- recvNBytes (fromIntegral (BS.head len))
        return (C.unpack domain, BS.append len domain)
      _ -> throwError "unsupported atyp"
  port <- recvNBytes 2
  return (cmd, atyp, addr, toPort port, BS.append encodedAddr port)

buildSocks5Connection :: SocketEff m => m Socket
buildSocks5Connection = do
  (ver, method) <- recvVerAndMethod
  sendAllBytes (BS.pack [ver, method])
  (cmd, atyp, addr, port, encodedAddrAndPort) <- recvTargetAddress
  when (cmd /= 0x01) $ throwError ("unsupported cmd: " ++ show cmd)
  mSocket :: Either E.SomeException Socket <- liftIO $ E.try $ newConnection addr port
  case mSocket of
    Left e -> do
      sendAllBytes $ ver `BS.cons` 0x01 `BS.cons` 0x00 `BS.cons` atyp `BS.cons` encodedAddrAndPort
      throwError $ "connection " ++ addr ++ " " ++ port ++ " error: " ++ show e
    Right socket -> do
      sendAllBytes $ ver `BS.cons` 0x00 `BS.cons` 0x00 `BS.cons` atyp `BS.cons` encodedAddrAndPort
      return socket
