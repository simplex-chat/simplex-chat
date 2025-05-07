{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Simplex.Chat.Remote.Protocol where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Crypto.Hash (SHA512)
import qualified Crypto.Hash as CH
import Data.Aeson (FromJSON (..), ToJSON (..), (.=))
import qualified Data.Aeson as J
import qualified Data.Aeson.Key as JK
import qualified Data.Aeson.KeyMap as JM
import qualified Data.Aeson.TH as JQ
import qualified Data.Aeson.Types as JT
import qualified Data.ByteArray as BA
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Builder (Builder, byteString, lazyByteString)
import qualified Data.ByteString.Lazy as LB
import Data.String (fromString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Word (Word32)
import qualified Network.HTTP.Types as N
import qualified Network.HTTP2.Client as H
import Network.Transport.Internal (decodeWord32, encodeWord32)
import Simplex.Chat.Controller
import Simplex.Chat.Remote.Transport
import Simplex.Chat.Remote.Types
import Simplex.FileTransfer.Description (FileDigest (..))
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Crypto.File (CryptoFile (..))
import Simplex.Messaging.Crypto.Lazy (LazyByteString)
import Simplex.Messaging.Encoding
import Simplex.Messaging.Parsers (defaultJSON, dropPrefix, taggedObjectJSON, pattern SingleFieldJSONTag, pattern TaggedObjectJSONData, pattern TaggedObjectJSONTag)
import qualified Simplex.Messaging.TMap as TM
import Simplex.Messaging.Transport (TSbChainKeys)
import Simplex.Messaging.Transport.Buffer (getBuffered)
import Simplex.Messaging.Transport.HTTP2 (HTTP2Body (..), HTTP2BodyChunk, getBodyChunk)
import Simplex.Messaging.Transport.HTTP2.Client (HTTP2Client, HTTP2Response (..), closeHTTP2Client, sendRequestDirect)
import Simplex.Messaging.Util (liftError', liftEitherWith, liftError, tshow)
import Simplex.RemoteControl.Client (xrcpBlockSize)
import qualified Simplex.RemoteControl.Client as RC
import Simplex.RemoteControl.Types (CtrlSessKeys (..), HostSessKeys (..), RCErrorType (..), SessionCode)
import System.FilePath (takeFileName, (</>))
import UnliftIO

data RemoteCommand
  = RCSend {command :: Text} -- TODO maybe ChatCommand here?
  | RCRecv {wait :: Int} -- this wait should be less than HTTP timeout
  | -- local file encryption is determined by the host, but can be overridden for videos
    RCStoreFile {fileName :: String, fileSize :: Word32, fileDigest :: FileDigest} -- requires attachment
  | RCGetFile {file :: RemoteFile}
  deriving (Show)

data RemoteResponse
  = RRChatResponse {chatResponse :: RRResult ChatResponse}
  | RRChatEvent {chatEvent :: Maybe (RRResult ChatEvent)} -- 'Nothing' on poll timeout
  | RRFileStored {filePath :: String}
  | RRFile {fileSize :: Word32, fileDigest :: FileDigest} -- provides attachment , fileDigest :: FileDigest
  | RRProtocolError {remoteProcotolError :: RemoteProtocolError} -- The protocol error happened on the server side
  deriving (Show)

data RRResult r
  = RRResult {result :: r}
  | RRError {error :: ChatError}
  deriving (Show)

resultToEither :: RRResult r -> Either ChatError r
resultToEither = \case
  RRResult r -> Right r
  RRError e -> Left e
{-# INLINE resultToEither #-}

eitherToResult :: Either ChatError r -> RRResult r
eitherToResult = either RRError RRResult
{-# INLINE eitherToResult #-}

$(pure [])

-- Force platform-independent encoding as the types aren't UI-visible
instance ToJSON r => ToJSON (RRResult r) where
  toEncoding = $(JQ.mkToEncoding (defaultJSON {J.sumEncoding = J.UntaggedValue}) ''RRResult)
  toJSON = $(JQ.mkToJSON (defaultJSON {J.sumEncoding = J.UntaggedValue}) ''RRResult)

instance FromJSON r => FromJSON (RRResult r) where
  parseJSON = $(JQ.mkParseJSON (defaultJSON {J.sumEncoding = J.UntaggedValue}) ''RRResult)

$(JQ.deriveJSON (taggedObjectJSON $ dropPrefix "RC") ''RemoteCommand)
$(JQ.deriveJSON (taggedObjectJSON $ dropPrefix "RR") ''RemoteResponse)

-- * Client side / desktop

mkRemoteHostClient :: HTTP2Client -> HostSessKeys -> SessionCode -> FilePath -> HostAppInfo -> CM RemoteHostClient
mkRemoteHostClient httpClient sessionKeys sessionCode storePath HostAppInfo {encoding, deviceName, encryptFiles} = do
  let HostSessKeys {chainKeys, idPrivKey, sessPrivKey} = sessionKeys
      signatures = RSSign {idPrivKey, sessPrivKey}
  encryption <- liftIO $ mkRemoteCrypto sessionCode chainKeys signatures
  pure
    RemoteHostClient
      { hostEncoding = encoding,
        hostDeviceName = deviceName,
        httpClient,
        encryption,
        encryptHostFiles = encryptFiles,
        storePath
      }

mkCtrlRemoteCrypto :: CtrlSessKeys -> SessionCode -> CM RemoteCrypto
mkCtrlRemoteCrypto CtrlSessKeys {chainKeys, idPubKey, sessPubKey} sessionCode =
  let signatures = RSVerify {idPubKey, sessPubKey}
   in liftIO $ mkRemoteCrypto sessionCode chainKeys signatures

mkRemoteCrypto :: SessionCode -> TSbChainKeys -> RemoteSignatures -> IO RemoteCrypto
mkRemoteCrypto sessionCode chainKeys signatures = do
  sndCounter <- newTVarIO 0
  rcvCounter <- newTVarIO 0
  skippedKeys <- liftIO TM.emptyIO
  pure RemoteCrypto {sessionCode, sndCounter, rcvCounter, chainKeys, skippedKeys, signatures}

closeRemoteHostClient :: RemoteHostClient -> IO ()
closeRemoteHostClient RemoteHostClient {httpClient} = closeHTTP2Client httpClient

-- ** Commands

remoteSend :: RemoteHostClient -> ByteString -> ExceptT RemoteProtocolError IO (Either ChatError ChatResponse)
remoteSend c cmd =
  sendRemoteCommand' c Nothing RCSend {command = decodeUtf8 cmd} >>= \case
    RRChatResponse cr -> pure $ resultToEither cr
    r -> badResponse r

remoteRecv :: RemoteHostClient -> Int -> ExceptT RemoteProtocolError IO (Maybe (Either ChatError ChatEvent))
remoteRecv c ms =
  sendRemoteCommand' c Nothing RCRecv {wait = ms} >>= \case
    RRChatEvent cEvt_ -> pure $ resultToEither <$> cEvt_
    r -> badResponse r

remoteStoreFile :: RemoteHostClient -> FilePath -> FilePath -> ExceptT RemoteProtocolError IO FilePath
remoteStoreFile c localPath fileName = do
  (fileSize, fileDigest) <- getFileInfo localPath
  let send h = sendRemoteCommand' c (Just (h, fileSize)) RCStoreFile {fileName, fileSize, fileDigest}
  withFile localPath ReadMode send >>= \case
    RRFileStored {filePath = filePath'} -> pure filePath'
    r -> badResponse r

remoteGetFile :: RemoteHostClient -> FilePath -> RemoteFile -> ExceptT RemoteProtocolError IO ()
remoteGetFile c destDir rf@RemoteFile {fileSource = CryptoFile {filePath}} =
  sendRemoteCommand c Nothing RCGetFile {file = rf} >>= \case
    (rfKN, getChunk, RRFile {fileSize, fileDigest}) -> do
      -- TODO we could optimize by checking size and hash before receiving the file
      let localPath = destDir </> takeFileName filePath
      receiveEncryptedFile rfKN getChunk fileSize fileDigest localPath
    (_, _, r) -> badResponse r

-- TODO validate there is no attachment in response
sendRemoteCommand' :: RemoteHostClient -> Maybe (Handle, Word32) -> RemoteCommand -> ExceptT RemoteProtocolError IO RemoteResponse
sendRemoteCommand' c attachment_ rc = do
  (_, _, r) <- sendRemoteCommand c attachment_ rc
  pure r

sendRemoteCommand :: RemoteHostClient -> Maybe (Handle, Word32) -> RemoteCommand -> ExceptT RemoteProtocolError IO (C.SbKeyNonce, Int -> IO ByteString, RemoteResponse)
sendRemoteCommand RemoteHostClient {httpClient, hostEncoding, encryption} file_ cmd = do
  (corrId, cmdKN, sfKN) <- atomically $ getRemoteSndKeys encryption
  encCmd <- encryptEncodeHTTP2Body corrId cmdKN encryption $ J.encode cmd
  encFile_ <- mapM (prepareEncryptedFile sfKN) file_
  let req = httpRequest encFile_ encCmd
  HTTP2Response {response, respBody} <- liftError' (RPEHTTP2 . tshow) $ sendRequestDirect httpClient req Nothing
  (rfKN, header, getNext) <- parseDecryptHTTP2Body encryption response respBody
  rr <- liftEitherWith (RPEInvalidJSON . fromString) $ J.eitherDecode header >>= JT.parseEither J.parseJSON . convertJSON hostEncoding localEncoding
  pure (rfKN, getNext, rr)
  where
    httpRequest encFile_ cmdBld = H.requestStreaming N.methodPost "/" mempty $ \send flush -> do
      send cmdBld
      forM_ encFile_ (`sendEncryptedFile` send)
      flush

badResponse :: RemoteResponse -> ExceptT RemoteProtocolError IO a
badResponse = \case
  RRProtocolError e -> throwError e
  -- TODO handle chat errors?
  r -> throwError $ RPEUnexpectedResponse $ tshow r

-- * Transport-level wrappers

convertJSON :: PlatformEncoding -> PlatformEncoding -> J.Value -> J.Value
convertJSON _remote@PEKotlin _local@PEKotlin = id
convertJSON PESwift PESwift = id
convertJSON PESwift PEKotlin = owsf2tagged
convertJSON PEKotlin PESwift = Prelude.error "unsupported convertJSON: K/S" -- guarded by handshake

-- | Convert swift single-field sum encoding into tagged/discriminator-field
owsf2tagged :: J.Value -> J.Value
owsf2tagged = fst . convert
  where
    convert val = case val of
      J.Object o
        | JM.size o == 2 ->
            case JM.toList o of
              [OwsfTag, o'] -> tagged o'
              [o', OwsfTag] -> tagged o'
              _ -> props
        | otherwise -> props
        where
          props = (J.Object $ fmap owsf2tagged o, False)
      J.Array a -> (J.Array $ fmap owsf2tagged a, False)
      _ -> (val, False)
    -- `tagged` converts the pair of single-field object encoding to tagged encoding.
    -- It sets innerTag returned by `convert` to True to prevent the tag being overwritten.
    tagged (k, v) = (J.Object pairs, True)
      where
        (v', innerTag) = convert v
        pairs = case v' of
          -- `innerTag` indicates that internal object already has tag,
          -- so the current tag cannot be inserted into it.
          J.Object o
            | innerTag -> pair
            | otherwise -> JM.insert TaggedObjectJSONTag tag o
          _ -> pair
        tag = J.String $ JK.toText k
        pair = JM.fromList [TaggedObjectJSONTag .= tag, TaggedObjectJSONData .= v']

pattern OwsfTag :: (JK.Key, J.Value)
pattern OwsfTag = (SingleFieldJSONTag, J.Bool True)

-- ```
-- commandBody = encBody sessSignature idSignature (attachment / noAttachment)
-- responseBody = encBody attachment; should match counter in the command
-- encBody = nonce encLength32 encrypted(tlsunique counter body)
-- attachment = %x01 nonce encLength32 encrypted(attachment)
-- noAttachment = %x00
-- tlsunique = length 1*OCTET
-- nonce = 24*24 OCTET
-- counter = 8*8 OCTET ; int64
-- encLength32 = 4*4 OCTET ; uint32, includes authTag
-- ```

-- See https://github.com/simplex-chat/simplexmq/blob/master/rfcs/2023-10-25-remote-control.md for encoding

encryptEncodeHTTP2Body :: Word32 -> C.SbKeyNonce -> RemoteCrypto -> LazyByteString -> ExceptT RemoteProtocolError IO Builder
encryptEncodeHTTP2Body corrId cmdKN RemoteCrypto {sessionCode, signatures} s = do
  ct <- liftError PRERemoteControl $ RC.rcEncryptBody cmdKN $ LB.fromStrict (smpEncode sessionCode) <> s
  let ctLen = encodeWord32 (fromIntegral $ LB.length ct)
      signed = LB.fromStrict (encodeWord32 corrId <> ctLen) <> ct
  sigs <- bodySignatures signed
  pure $ lazyByteString signed <> sigs
  where
    bodySignatures :: LazyByteString -> ExceptT RemoteProtocolError IO Builder
    bodySignatures signed = case signatures of
      RSSign {idPrivKey, sessPrivKey} -> do
        let hc = CH.hashUpdates (CH.hashInit @SHA512) (LB.toChunks signed)
            ssig = sign sessPrivKey hc
            idsig = sign idPrivKey $ CH.hashUpdate hc ssig
        pure $ byteString $ smpEncode (ssig, idsig)
      _ -> pure mempty
    sign :: C.PrivateKeyEd25519 -> CH.Context SHA512 -> ByteString
    sign k = C.signatureBytes . C.sign' k . BA.convert . CH.hashFinalize

-- | Parse and decrypt HTTP2 request/response
parseDecryptHTTP2Body :: HTTP2BodyChunk a => RemoteCrypto -> a -> HTTP2Body -> ExceptT RemoteProtocolError IO (C.SbKeyNonce, LazyByteString, Int -> IO ByteString)
parseDecryptHTTP2Body rc@RemoteCrypto {sessionCode, signatures} hr HTTP2Body {bodyBuffer} = do
  (corrId, ct) <- getBody
  (cmdKN, rfKN) <- ExceptT $ atomically $ getRemoteRcvKeys rc corrId
  s <- liftError PRERemoteControl $ RC.rcDecryptBody cmdKN ct
  s' <- parseBody s
  pure (rfKN, s', getNext)
  where
    getBody :: ExceptT RemoteProtocolError IO (Word32, LazyByteString)
    getBody = do
      corrIdStr <- liftIO $ getNext 4
      ctLenStr <- liftIO $ getNext 4
      let ctLen = decodeWord32 ctLenStr
      when (ctLen > fromIntegral (maxBound :: Int)) $ throwError RPEInvalidSize
      chunks <- liftIO $ getLazy $ fromIntegral ctLen
      let hc = CH.hashUpdates (CH.hashInit @SHA512) [corrIdStr, ctLenStr]
          hc' = CH.hashUpdates hc chunks
      verifySignatures hc'
      pure (decodeWord32 corrIdStr, LB.fromChunks chunks)
    getLazy :: Int -> IO [ByteString]
    getLazy 0 = pure []
    getLazy n = do
      let sz = min n xrcpBlockSize
      bs <- getNext sz
      let n' = if B.length bs < sz then 0 else max 0 (n - xrcpBlockSize)
      (bs :) <$> getLazy n'
    verifySignatures :: CH.Context SHA512 -> ExceptT RemoteProtocolError IO ()
    verifySignatures hc = case signatures of
      RSVerify {sessPubKey, idPubKey} -> do
        ssig <- getSig
        idsig <- getSig
        verifySig sessPubKey ssig hc
        verifySig idPubKey idsig $ CH.hashUpdate hc $ C.signatureBytes ssig
      _ -> pure ()
      where
        getSig = do
          len <- liftIO $ B.head <$> getNext 1
          liftError' RPEInvalidBody $ C.decodeSignature <$> getNext (fromIntegral len)
        verifySig key sig hc' = do
          let signed = BA.convert $ CH.hashFinalize hc'
          unless (C.verify' key sig signed) $ throwError $ PRERemoteControl RCECtrlAuth
    parseBody :: LazyByteString -> ExceptT RemoteProtocolError IO LazyByteString
    parseBody s = case LB.uncons s of
      Nothing -> throwError $ RPEInvalidBody "empty body"
      Just (scLen, rest) -> do
        (sessCode', s') <- takeBytes (fromIntegral scLen) rest
        unless (sessCode' == sessionCode) $ throwError PRESessionCode
        pure s'
      where
        takeBytes n s' = do
          let (bs, rest) = LB.splitAt n s'
          unless (LB.length bs == n) $ throwError PRESessionCode
          pure (LB.toStrict bs, rest)
    getNext sz = getBuffered bodyBuffer sz Nothing $ getBodyChunk hr
