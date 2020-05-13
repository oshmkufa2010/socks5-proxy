module Protocol(
  buildSocks5Connection,
  runSocketEff,
) where

import qualified Data.ByteString as BS
import qualified Control.Exception as E
import Data.ByteString (ByteString(..))
import Network.Socket
import Data.Word (Word8(..))
import Control.Monad.IO.Class
import Effect
import Control.Monad (when)
import Data.List (intersperse)

newConnection:: String -> String -> IO Socket
newConnection host port = do
  addr <- resolve
  open addr
  where
    resolve = do
      let hints = defaultHints { addrSocketType = Stream }
      head <$> getAddrInfo (Just hints) (Just host) (Just port)
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      connect sock $ addrAddress addr
      return sock

toIPv4 :: ByteString -> String
toIPv4 addr = concat $ intersperse "." $ fmap (show . fromIntegral) $ BS.unpack addr

toPort :: ByteString -> String
toPort port = show $ foldl (\b a -> b * 256 + a) 0 $ fmap fromIntegral $ BS.unpack port

recvVerAndMethod :: SocketEff (Word8, Word8)
recvVerAndMethod = do
  result <- recvNBytes 2
  let [ver, methodn] = BS.unpack result
  when (ver /= 0x05) $ throwSocketError ("unsupported ver: " ++ (show ver))
  methods <- recvNBytes (fromIntegral methodn)
  when (0x00 `notElem` (BS.unpack methods)) $ throwSocketError "no method supported"
  return (ver, 0x00)

recvTargetAddress :: SocketEff (Word8, Word8, ByteString, ByteString)
recvTargetAddress = do
  bs <- recvNBytes 4
  let [ver, cmd, _, atyp] = BS.unpack bs
  addrLen <- do
    case atyp of
      0x01 -> return 4
      0x03 -> throwSocketError "unsupported atyp: domain"
      0x04 -> return 16
      _ -> throwSocketError "unsupported atyp"
  addr <- recvNBytes addrLen
  port <- recvNBytes 2 
  return (cmd, atyp, addr, port)

buildSocks5Connection :: SocketEff Socket
buildSocks5Connection = do
  (ver, method) <- recvVerAndMethod
  sendAllBytes (BS.pack [ver, method])
  (cmd, atyp, addr, port) <- recvTargetAddress
  when (cmd /= 0x01) $ throwSocketError ("unsupported cmd: " ++ (show cmd))
  remoteSocket <- liftIO (E.try $ newConnection (toIPv4 addr) (toPort port) :: IO (Either E.SomeException Socket))
  case remoteSocket of
    Left e -> do
      sendAllBytes $ ver `BS.cons` 0x01 `BS.cons` 0x00 `BS.cons` atyp `BS.cons` (addr `BS.append` port)
      throwSocketError (show e)
    Right socket -> do
      sendAllBytes $ ver `BS.cons` 0x00 `BS.cons` 0x00 `BS.cons` atyp `BS.cons` (addr `BS.append` port)
      return socket
