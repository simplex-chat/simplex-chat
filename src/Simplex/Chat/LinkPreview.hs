{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Simplex.Chat.LinkPreview where

import Control.Applicative (optional, (<|>))
import Control.Concurrent.STM (TVar, atomically)
import qualified Control.Exception as E
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Crypto.Random (ChaChaDRG)
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.Bifunctor (first)
import qualified Data.ByteString.Base64 as B64
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import Data.Char (chr, digitToInt, intToDigit, isAsciiLower, isAsciiUpper, isDigit, isHexDigit, ord, toLower, toUpper)
import Data.Default (def)
import Data.FileEmbed (embedFile)
import Data.IORef
import Data.IP (AddrRange, IP (..), IPv4, IPv6, fromIPv6w, fromSockAddr, isMatchedTo, toHostAddress, toHostAddress6, toIPv4w)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as L
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe, mapMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import qualified Data.X509.CertificateStore as XS
import Data.X509.Memory (readSignedObjectFromMemory)
import Network.Socket
import Network.Socks5 (SocksAddress (..), SocksCredentials (..), SocksHostAddress (..), defaultSocksConf, socksConnect, socksConnectAuth)
import qualified Network.TLS as TLS
import Simplex.Chat.Image (imageDataPreview)
import Simplex.Chat.Protocol (LinkPreview (..))
import qualified Simplex.Messaging.Crypto as C
import Simplex.Messaging.Client (NetworkConfig (..), NetworkRequestMode, netTimeoutInt, transportClientConfig)
import Simplex.Messaging.Encoding.String (strEncode)
import Simplex.Messaging.Transport (closeTLS, defaultSupportedParamsHTTPS)
import Simplex.Messaging.Transport.Client (SocksAuth (..), SocksProxy (..), SocksProxyWithAuth (..), TransportClientConfig (..), TransportHost (..))
import Simplex.Messaging.Util (catchOwn', safeDecodeUtf8)
import System.Timeout (timeout)
import Text.Read (readMaybe)

data LinkPreviewConfig = LinkPreviewConfig
  { rootCerts :: XS.CertificateStore,
    addressPolicy :: AddressPolicy,
    previewDeadline :: Int
  }

data AddressPolicy = APPublic | APPublicAndLoopback

defaultLinkPreviewConfig :: LinkPreviewConfig
defaultLinkPreviewConfig =
  LinkPreviewConfig
    { rootCerts = XS.makeCertificateStore $ readSignedObjectFromMemory linkPreviewRoots,
      addressPolicy = APPublic,
      previewDeadline = 30_000000
    }

linkPreviewRoots :: ByteString
linkPreviewRoots = $(embedFile "src/Simplex/Chat/LinkPreview/roots.pem")

maxRedirects :: Int
maxRedirects = 5

maxHeaderBytes :: Int
maxHeaderBytes = 32768

maxHeaders :: Int
maxHeaders = 100

maxChunkLineBytes :: Int
maxChunkLineBytes = 1024

maxHeadBytes :: Int
maxHeadBytes = 1048576

maxImageBytes :: Int
maxImageBytes = 5242880

maxDescriptionLength :: Int
maxDescriptionLength = 500

data Fetcher = Fetcher
  { previewConfig :: LinkPreviewConfig,
    networkConfig :: NetworkConfig,
    requestMode :: NetworkRequestMode,
    socksCredentials :: Maybe SocksCredentials
  }

data Page
  = HtmlPage {pageTitle :: Text, pageDescription :: Text, pageImage :: Uri}
  | ImagePage ByteString
  deriving (Eq, Show)

getLinkPreview :: LinkPreviewConfig -> NetworkConfig -> NetworkRequestMode -> TVar ChaChaDRG -> Text -> IO (Either String LinkPreview)
getLinkPreview previewConfig@LinkPreviewConfig {previewDeadline} networkConfig requestMode g uriText = runExceptT $ do
  uri <- liftEither $ linkUri uriText
  socksCredentials <- liftIO $ previewCredentials networkConfig g
  let fetcher = Fetcher {previewConfig, networkConfig, requestMode, socksCredentials}
  withTimeout previewDeadline "timeout" $ ExceptT $ runExceptT (linkPreview fetcher uriText uri) `catchOwn'` (pure . Left . show)

linkPreview :: Fetcher -> Text -> Uri -> ExceptT String IO LinkPreview
linkPreview fetcher uriText uri@Uri {uriPath} =
  fetch fetcher uri pageContent >>= \case
    HtmlPage {pageTitle, pageDescription, pageImage} -> preview pageTitle pageDescription =<< fetch fetcher pageImage (\_ Response {body} -> imageBody body)
    ImagePage bytes -> preview (safeDecodeUtf8 $ snd $ B.breakEnd (== '/') uriPath) "" bytes
  where
    preview title description bytes = do
      image <- ExceptT $ imageDataPreview bytes
      pure LinkPreview {uri = uriText, title, description, image, content = Nothing}

previewCredentials :: NetworkConfig -> TVar ChaChaDRG -> IO (Maybe SocksCredentials)
previewCredentials NetworkConfig {socksProxy} g = case socksProxy of
  Just (SocksProxyWithAuth auth _) -> case auth of
    SocksAuthUsername {username, password} -> pure $ Just $ SocksCredentials username password
    SocksAuthNull -> pure Nothing
    SocksIsolateByAuth -> Just . (`SocksCredentials` "") . B64.encode <$> atomically (C.randomBytes 16 g)
  Nothing -> pure Nothing

withTimeout :: Int -> String -> ExceptT String IO a -> ExceptT String IO a
withTimeout t err action = ExceptT $ fromMaybe (Left err) <$> timeout t (runExceptT action)

data Uri = Uri
  { uriHost :: TransportHost,
    uriPort :: Maybe Int,
    uriPath :: ByteString,
    uriQuery :: Maybe ByteString
  }
  deriving (Eq, Show)

data UriRef = UriRef
  { refScheme :: Maybe ByteString,
    refAuthority :: Maybe ByteString,
    refPath :: ByteString,
    refQuery :: Maybe ByteString
  }

linkUri :: Text -> Either String Uri
linkUri = httpUri <=< resolveRef Nothing . uriRef . encodeUtf8

imageUri :: Uri -> ByteString -> Either String Uri
imageUri page = httpUri <=< resolveRef (Just page) . uriRef

redirectUri :: Uri -> ByteString -> Either String Uri
redirectUri request = httpsUri <=< resolveRef (Just request) . uriRef

httpUri :: (ByteString, Uri) -> Either String Uri
httpUri (scheme, uri)
  | scheme == "https" || scheme == "http" = Right uri
  | otherwise = Left $ "unsupported scheme: " <> B.unpack scheme

httpsUri :: (ByteString, Uri) -> Either String Uri
httpsUri (scheme, uri)
  | scheme == "https" = Right uri
  | otherwise = Left $ "redirect to scheme: " <> B.unpack scheme

uriRef :: ByteString -> UriRef
uriRef s = UriRef {refScheme, refAuthority, refPath, refQuery}
  where
    noFragment = B.takeWhile (/= '#') s
    (refScheme, afterScheme) = case B.break (`B.elem` ":/?") noFragment of
      (scheme, rest) | not (B.null scheme), Just rest' <- B.stripPrefix ":" rest -> (Just scheme, rest')
      _ -> (Nothing, noFragment)
    (refAuthority, afterAuthority) = case B.stripPrefix "//" afterScheme of
      Just rest -> first Just $ B.break (`B.elem` "/?") rest
      Nothing -> (Nothing, afterScheme)
    (refPath, query) = B.break (== '?') afterAuthority
    refQuery = B.drop 1 query <$ guard (not $ B.null query)

resolveRef :: Maybe Uri -> UriRef -> Either String (ByteString, Uri)
resolveRef base_ UriRef {refScheme, refAuthority, refPath, refQuery} = case (refScheme, refAuthority, base_) of
  (Just scheme, Just authority, _) -> (asciiLower scheme,) <$> authorityUri authority (removeDotSegments refPath) refQuery
  (Just _, Nothing, _) -> Left "no host"
  (Nothing, _, Nothing) -> Left "relative link"
  (Nothing, Just authority, Just _) -> ("https",) <$> authorityUri authority (removeDotSegments refPath) refQuery
  (Nothing, Nothing, Just base@Uri {uriPath, uriQuery})
    | B.null refPath -> Right ("https", base {uriQuery = refQuery <|> uriQuery})
    | "/" `B.isPrefixOf` refPath -> Right ("https", base {uriPath = removeDotSegments refPath, uriQuery = refQuery})
    | B.null uriPath -> Right ("https", base {uriPath = removeDotSegments $ "/" <> refPath, uriQuery = refQuery})
    | otherwise -> Right ("https", base {uriPath = removeDotSegments $ B.dropWhileEnd (/= '/') uriPath <> refPath, uriQuery = refQuery})

authorityUri :: ByteString -> ByteString -> Maybe ByteString -> Either String Uri
authorityUri authority uriPath uriQuery = do
  uriHost <- linkHost host
  uriPort <- case B.uncons port of
    Nothing -> Right Nothing
    Just (':', "") -> Right Nothing
    Just (':', digits) | B.length digits <= 5 && B.all isDigit digits, Just (n, "") <- B.readInt digits, n > 0 && n < 65536 -> Right $ Just n
    _ -> Left $ "invalid port: " <> B.unpack port
  pure Uri {uriHost, uriPort, uriPath, uriQuery}
  where
    hostPort = snd $ B.breakEnd (== '@') authority
    (host, port)
      | "[" `B.isPrefixOf` hostPort = let (ipv6, rest) = B.break (== ']') hostPort in (ipv6 <> B.take 1 rest, B.drop 1 rest)
      | otherwise = B.break (== ':') hostPort

linkHost :: ByteString -> Either String TransportHost
linkHost h
  | Just ('[', bracketed) <- B.uncons h, Just (ipv6, ']') <- B.unsnoc bracketed =
      maybe (Left $ "invalid host: " <> B.unpack h) (Right . THIPv6 . toHostAddress6) (readMaybe $ B.unpack ipv6 :: Maybe IPv6)
  | Just ipv4 <- (readMaybe (B.unpack h) :: Maybe IPv4) = Right $ THIPv4 $ hostAddressToTuple $ toHostAddress ipv4
  | B.null name || not (B.all hostNameChar name) = Left $ "invalid host: " <> B.unpack h
  | ".onion" `B.isSuffixOf` name = Right $ THOnionHost name
  | otherwise = Right $ THDomainName $ B.unpack name
  where
    name = asciiLower h
    hostNameChar c = isAsciiLower c || isDigit c || c == '-' || c == '.' || c == '_'

removeDotSegments :: ByteString -> ByteString
removeDotSegments = B.concat . reverse . go []
  where
    go out s
      | B.null s = out
      | Just s' <- B.stripPrefix "../" s = go out s'
      | Just s' <- B.stripPrefix "./" s = go out s'
      | Just s' <- B.stripPrefix "/./" s = go out ("/" <> s')
      | s == "/." = go out "/"
      | Just s' <- B.stripPrefix "/../" s = go (drop 1 out) ("/" <> s')
      | s == "/.." = go (drop 1 out) "/"
      | s == "." || s == ".." = out
      | otherwise = let (segment, s') = firstSegment s in go (segment : out) s'
    firstSegment s = case B.uncons s of
      Just ('/', s') -> first ("/" <>) $ B.break (== '/') s'
      _ -> B.break (== '/') s

data Conn = Conn
  { sendBytes :: ByteString -> IO (),
    recvBytes :: ExceptT String IO ByteString,
    unread :: IORef ByteString
  }

newConn :: (ByteString -> IO ()) -> ExceptT String IO ByteString -> IO Conn
newConn sendBytes recvBytes = Conn sendBytes recvBytes <$> newIORef ""

recvConn :: Conn -> ExceptT String IO ByteString
recvConn Conn {recvBytes, unread} = liftIO (atomicModifyIORef' unread ("",)) >>= \s -> if B.null s then recvBytes else pure s

unrecv :: Conn -> ByteString -> IO ()
unrecv Conn {unread} s = unless (B.null s) $ modifyIORef' unread (s <>)

withConnection :: Fetcher -> Uri -> (Conn -> ExceptT String IO a) -> ExceptT String IO a
withConnection Fetcher {previewConfig = LinkPreviewConfig {rootCerts, addressPolicy}, networkConfig, requestMode, socksCredentials} Uri {uriHost, uriPort} action =
  ExceptT $ E.bracket (runExceptT $ withTimeout connectTimeout "connection timeout" siteSocket) (mapM_ close) $ \case
    Left e -> pure $ Left e
    Right sock -> E.bracket (TLS.contextNew sock tlsParams) closeTLS $ \ctx -> runExceptT $ do
      withTimeout connectTimeout "TLS handshake timeout" $ liftIO $ TLS.handshake ctx
      action =<< liftIO (newConn (TLS.sendData ctx . LB.fromStrict) (withTimeout readTimeout "read timeout" $ liftIO $ TLS.recvData ctx))
  where
    TransportClientConfig {socksProxy, tcpConnectTimeout = connectTimeout} = transportClientConfig networkConfig requestMode uriHost True Nothing
    NetworkConfig {tcpTimeout} = networkConfig
    readTimeout = netTimeoutInt tcpTimeout requestMode
    port = maybe 443 fromIntegral uriPort
    siteSocket = case socksProxy of
      Just (SocksProxy proxy) -> do
        forM_ (literalAddress uriHost port) $ \addr -> unless (allowedAddress addressPolicy addr) $ throwError $ "prohibited address: " <> show addr
        let conf = defaultSocksConf proxy
            dest = SocksAddress (socksHostAddress uriHost) port
        liftIO $ fst <$> maybe (socksConnect conf dest) (socksConnectAuth conf dest) socksCredentials
      Nothing -> do
        addrs <- siteAddresses
        case L.nonEmpty $ filter (allowedAddress addressPolicy) addrs of
          Just allowed -> liftIO $ connectAny allowed
          Nothing -> throwError $ "prohibited address: " <> unwords (map show addrs)
    siteAddresses = case uriHost of
      THDomainName name -> liftIO $ map addrAddress <$> getAddrInfo (Just defaultHints {addrSocketType = Stream}) (Just name) (Just $ show port)
      THOnionHost _ -> throwError "onion host without SOCKS proxy"
      _ -> pure $ maybeToList $ literalAddress uriHost port
    tlsParams =
      (TLS.defaultParamsClient (B.unpack $ strEncode uriHost) "")
        { TLS.clientUseServerNameIndication = hostName,
          TLS.clientShared = def {TLS.sharedCAStore = rootCerts},
          TLS.clientSupported = linkPreviewTLSParams
        }
    hostName = case uriHost of
      THDomainName _ -> True
      THOnionHost _ -> True
      _ -> False

linkPreviewTLSParams :: TLS.Supported
linkPreviewTLSParams = defaultSupportedParamsHTTPS {TLS.supportedGroups = TLS.supportedGroups defaultSupportedParamsHTTPS <> [TLS.P256, TLS.P384]}

literalAddress :: TransportHost -> PortNumber -> Maybe SockAddr
literalAddress host port = case host of
  THIPv4 addr -> Just $ SockAddrInet port $ tupleToHostAddress addr
  THIPv6 addr -> Just $ SockAddrInet6 port 0 addr 0
  _ -> Nothing

socksHostAddress :: TransportHost -> SocksHostAddress
socksHostAddress = \case
  THIPv4 addr -> SocksAddrIPV4 $ tupleToHostAddress addr
  THIPv6 addr -> SocksAddrIPV6 addr
  THOnionHost h -> SocksAddrDomainName h
  THDomainName h -> SocksAddrDomainName $ B.pack h

connectAny :: NonEmpty SockAddr -> IO Socket
connectAny (addr :| addrs) = case L.nonEmpty addrs of
  Nothing -> open
  Just addrs' -> open `E.catch` \(_ :: E.IOException) -> connectAny addrs'
  where
    open = E.bracketOnError (socket sockFamily Stream defaultProtocol) close $ \sock -> sock <$ connect sock addr
    sockFamily = case addr of
      SockAddrInet {} -> AF_INET
      SockAddrInet6 {} -> AF_INET6
      SockAddrUnix {} -> AF_UNIX

allowedAddress :: AddressPolicy -> SockAddr -> Bool
allowedAddress policy = maybe False (allowedIP . fst) . fromSockAddr
  where
    allowedIP = \case
      IPv4 addr -> allowedIPv4 addr
      IPv6 addr
        | addr `isMatchedTo` "::ffff:0:0/96", (_, _, _, w) <- fromIPv6w addr -> allowedIPv4 $ toIPv4w w
        | otherwise -> not $ any (addr `isMatchedTo`) prohibitedIPv6
    allowedIPv4 addr = not $ any (addr `isMatchedTo`) prohibitedIPv4
    (prohibitedIPv4, prohibitedIPv6) = case policy of
      APPublic -> ("127.0.0.0/8" : localIPv4, "::1/128" : localIPv6)
      APPublicAndLoopback -> (localIPv4, localIPv6)

localIPv4 :: [AddrRange IPv4]
localIPv4 = ["0.0.0.0/8", "10.0.0.0/8", "100.64.0.0/10", "169.254.0.0/16", "172.16.0.0/12", "192.168.0.0/16", "224.0.0.0/4", "255.255.255.255/32"]

localIPv6 :: [AddrRange IPv6]
localIPv6 = ["::/128", "fc00::/7", "fe80::/10", "ff00::/8"]

data ResponseHead = ResponseHead {status :: Int, headers :: [(ByteString, ByteString)]}

data Response = Response {headers :: [(ByteString, ByteString)], body :: ExceptT String IO ByteString}

data BodyFraming = BFChunked | BFLength Int | BFClose

data ChunkedState = CSSize | CSData Int | CSEnd | CSDone

fetch :: Fetcher -> Uri -> (Uri -> Response -> ExceptT String IO a) -> ExceptT String IO a
fetch fetcher uri0 onContent = go maxRedirects uri0
  where
    go redirectsLeft uri =
      withConnection fetcher uri (requestUri uri) >>= \case
        Right a -> pure a
        Left location
          | redirectsLeft > 0 -> go (redirectsLeft - 1) =<< liftEither (redirectUri uri location)
          | otherwise -> throwError "too many redirects"
    requestUri uri conn = do
      liftIO $ sendBytes conn $ httpRequest uri
      ResponseHead {status, headers} <- recvResponseHead conn
      case lookup "location" headers of
        Just location | status `elem` [301, 302, 303, 307, 308] -> pure $ Left location
        _
          | status >= 200 && status < 300 -> Right <$> (onContent uri =<< response conn headers)
          | otherwise -> throwError $ "HTTP status " <> show status

httpRequest :: Uri -> ByteString
httpRequest Uri {uriHost, uriPort, uriPath, uriQuery} =
  B.concat
    [ "GET ",
      escape (if B.null uriPath then "/" else uriPath) <> maybe "" (("?" <>) . escape) uriQuery,
      " HTTP/1.1\r\nHost: ",
      host <> maybe "" ((":" <>) . B.pack . show) uriPort,
      "\r\nUser-Agent: ",
      userAgent,
      "\r\nAccept: text/html,application/xhtml+xml,image/png,image/jpeg;q=0.9,*/*;q=0.1\r\nAccept-Encoding: identity\r\nConnection: close\r\n\r\n"
    ]
  where
    escape = B.concatMap $ \c -> if c > ' ' && c < '\DEL' then B.singleton c else B.pack ['%', hexDigit (ord c `div` 16), hexDigit (ord c `mod` 16)]
    hexDigit = toUpper . intToDigit
    host = case uriHost of
      THIPv6 _ -> "[" <> strEncode uriHost <> "]"
      _ -> strEncode uriHost
    userAgent = case uriHost of
      THDomainName "x.com" -> "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_1) AppleWebKit/601.2.4 (KHTML, like Gecko) Version/9.0.1 Safari/601.2.4 facebookexternalhit/1.1 Facebot Twitterbot/1.0"
      _ -> "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/79.0.3945.130 Safari/537.36"

recvLine :: Conn -> Int -> ExceptT String IO ByteString
recvLine conn maxLength = go ""
  where
    go received = case B.breakSubstring "\r\n" received of
      (line, rest)
        | B.length line + 2 > maxLength -> throwError "HTTP line too long"
        | not (B.null rest) -> line <$ liftIO (unrecv conn $ B.drop 2 rest)
        | otherwise -> recvConn conn >>= \s -> if B.null s then throwError "connection closed" else go (received <> s)

recvResponseHead :: Conn -> ExceptT String IO ResponseHead
recvResponseHead conn = do
  statusLine <- recvLine conn maxHeaderBytes
  status <- liftEither $ first ("invalid HTTP status line: " <>) $ A.parseOnly statusP statusLine
  headers <- recvHeaders (maxHeaderBytes - B.length statusLine - 2) maxHeaders
  pure ResponseHead {status, headers}
  where
    statusP = "HTTP/1." *> A.digit *> A.char ' ' *> (foldl (\n d -> n * 10 + digitToInt d) 0 <$> A.count 3 A.digit)
    recvHeaders budget n =
      recvLine conn budget >>= \line ->
        if
          | B.null line -> pure []
          | n <= 0 -> throwError "too many HTTP headers"
          | otherwise -> (:) <$> liftEither (responseHeader line) <*> recvHeaders (budget - B.length line - 2) (n - 1)

responseHeader :: ByteString -> Either String (ByteString, ByteString)
responseHeader line = case B.break (== ':') line of
  (name, value)
    | not (B.null name) && B.all tokenChar name && not (B.null value) -> Right (asciiLower name, trim $ B.drop 1 value)
  _ -> Left $ "invalid HTTP header: " <> B.unpack line
  where
    tokenChar c = isAsciiLower c || isAsciiUpper c || isDigit c || c `B.elem` "!#$%&'*+-.^_`|~"

response :: Conn -> [(ByteString, ByteString)] -> ExceptT String IO Response
response conn headers = do
  forM_ (lookup "content-encoding" headers) $ \encoding ->
    unless (asciiLower encoding == "identity") $ throwError $ "unsupported content encoding: " <> B.unpack encoding
  framing <- liftEither $ case (lookup "transfer-encoding" headers, lookup "content-length" headers) of
    (Just encoding, _)
      | asciiLower encoding == "chunked" -> Right BFChunked
      | otherwise -> Left $ "unsupported transfer encoding: " <> B.unpack encoding
    (Nothing, Just len)
      | B.length len <= 18, B.all isDigit len, Just (n, "") <- B.readInt len -> Right $ BFLength n
      | otherwise -> Left $ "invalid content length: " <> B.unpack len
    (Nothing, Nothing) -> Right BFClose
  body <- liftIO $ case framing of
    BFChunked -> chunkedBody conn
    BFLength n -> limitBody n $ recvConn conn
    BFClose -> pure $ recvConn conn
  pure Response {headers, body}

chunkedBody :: Conn -> IO (ExceptT String IO ByteString)
chunkedBody conn = next <$> newIORef CSSize
  where
    next st =
      liftIO (readIORef st) >>= \case
        CSDone -> pure ""
        CSEnd ->
          recvLine conn maxChunkLineBytes >>= \line ->
            if B.null line then liftIO (writeIORef st CSSize) >> next st else throwError "invalid chunk end"
        CSSize ->
          recvLine conn maxChunkLineBytes >>= liftEither . chunkSize >>= \case
            0 -> "" <$ liftIO (writeIORef st CSDone)
            n -> liftIO (writeIORef st $ CSData n) >> next st
        CSData n -> do
          s <- recvConn conn
          let (chunk, rest) = B.splitAt n s
          liftIO $ do
            unrecv conn rest
            writeIORef st $ if
              | B.null s -> CSDone
              | B.length chunk == n -> CSEnd
              | otherwise -> CSData $ n - B.length chunk
          pure chunk
    chunkSize line = case B.takeWhile isHexDigit line of
      digits
        | not (B.null digits) && B.length digits <= 7 -> Right $ B.foldl' (\n d -> n * 16 + digitToInt d) 0 digits
        | otherwise -> Left $ "invalid chunk size: " <> B.unpack line

limitBody :: Int -> ExceptT String IO ByteString -> IO (ExceptT String IO ByteString)
limitBody limit next = next' <$> newIORef limit
  where
    next' remaining =
      liftIO (readIORef remaining) >>= \n ->
        if n <= 0
          then pure ""
          else do
            s <- B.take n <$> next
            liftIO $ writeIORef remaining $ n - B.length s
            pure s

readBody :: ExceptT String IO ByteString -> ExceptT String IO ByteString
readBody next = B.concat <$> go
  where
    go = next >>= \s -> if B.null s then pure [] else (s :) <$> go

imageBody :: ExceptT String IO ByteString -> ExceptT String IO ByteString
imageBody body = do
  bytes <- readBody =<< liftIO (limitBody (maxImageBytes + 1) body)
  when (B.length bytes > maxImageBytes) $ throwError "image too large"
  pure bytes

pageContent :: Uri -> Response -> ExceptT String IO Page
pageContent pageUri Response {headers, body} =
  if
    | mediaType `elem` ["text/html", "application/xhtml+xml"] -> do
        tags <- scanHead =<< liftIO (limitBody maxHeadBytes body)
        liftEither $ htmlPage pageUri charset tags
    | mediaType `elem` ["image/jpeg", "image/png"] -> ImagePage <$> imageBody body
    | otherwise -> throwError $ "unsupported content type: " <> B.unpack mediaType
  where
    (mediaType, charset) = maybe ("", Nothing) contentType $ lookup "content-type" headers

contentType :: ByteString -> (ByteString, Maybe ByteString)
contentType value = (asciiLower $ trim mediaType, listToMaybe $ mapMaybe charsetParam $ B.split ';' params)
  where
    (mediaType, params) = B.break (== ';') value
    charsetParam param = case B.break (== '=') param of
      (name, val) | asciiLower (trim name) == "charset" -> Just $ unquote $ trim $ B.drop 1 val
      _ -> Nothing

data HeadTags = HeadTags
  { ogTitle :: Maybe ByteString,
    ogDescription :: Maybe ByteString,
    ogImage :: Maybe ByteString,
    titleText :: Maybe ByteString,
    nameDescription :: Maybe ByteString,
    metaCharset :: Maybe ByteString
  }
  deriving (Eq, Show)

instance Semigroup HeadTags where
  HeadTags a b c d e f <> HeadTags a' b' c' d' e' f' = HeadTags (a <|> a') (b <|> b') (c <|> c') (d <|> d') (e <|> e') (f <|> f')

instance Monoid HeadTags where
  mempty = HeadTags Nothing Nothing Nothing Nothing Nothing Nothing

data HeadToken = HTTags HeadTags | HTEnd

scanHead :: ExceptT String IO ByteString -> ExceptT String IO HeadTags
scanHead next =
  A.parseWith next headP "" >>= \case
    A.Done _ tags -> pure tags
    A.Fail _ _ e -> throwError $ "head scanner: " <> e
    A.Partial _ -> throwError "head scanner: incomplete input"

headP :: A.Parser HeadTags
headP = go mempty
  where
    go tags = (tags <$ A.endOfInput) <|> (headTokenP >>= next tags)
    next tags = \case
      HTEnd -> pure tags
      HTTags t
        | ogComplete tags' -> pure tags'
        | otherwise -> go tags'
        where
          tags' = tags <> t
    ogComplete HeadTags {ogTitle, ogDescription, ogImage} = isJust ogTitle && isJust ogDescription && isJust ogImage

headTokenP :: A.Parser HeadToken
headTokenP =
  (noTags <$ A.takeWhile1 (/= '<'))
    <|> (noTags <$ ("<!--" *> skipPastCI "-->"))
    <|> (noTags <$ (A.char '<' *> A.satisfy (A.inClass "!?") *> A.skipWhile (/= '>') *> optional (A.char '>')))
    <|> ("</" *> endTagP)
    <|> (A.char '<' *> startTagP)
    <|> (noTags <$ A.anyChar)
  where
    noTags = HTTags mempty
    endTagP = do
      name <- tagNameP
      A.skipWhile (/= '>') <* optional (A.char '>')
      pure $ if name == "head" then HTEnd else noTags
    startTagP = do
      name <- tagNameP
      attrs <- A.many' attributeP
      A.skipWhile htmlSpace <* optional (A.char '/') <* A.char '>'
      case name of
        "meta" -> pure $ HTTags $ metaTags attrs
        "title" -> (\t -> HTTags mempty {titleText = Just t}) <$> textBeforeCI "</title"
        "script" -> noTags <$ skipPastCI "</script"
        "style" -> noTags <$ skipPastCI "</style"
        "body" -> pure HTEnd
        _ -> pure noTags
    tagNameP = asciiLower <$> A.takeWhile1 (\c -> isAsciiLower c || isAsciiUpper c || isDigit c)
    attributeP = do
      name <- A.skipWhile htmlSpace *> (asciiLower <$> A.takeWhile1 (\c -> not (htmlSpace c) && A.notInClass "\"'<>/=" c))
      value <- (A.skipWhile htmlSpace *> A.char '=' *> A.skipWhile htmlSpace *> valueP) <|> pure ""
      pure (name, value)
    valueP = quoted '"' <|> quoted '\'' <|> A.takeWhile (\c -> not (htmlSpace c) && c /= '>' && c /= '<')
    quoted q = A.char q *> A.takeWhile (/= q) <* A.char q
    htmlSpace c = c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\f'

skipPastCI :: ByteString -> A.Parser ()
skipPastCI marker = loop
  where
    loop = A.skipWhile (A.notInClass "<-") *> (void (A.stringCI marker) <|> A.endOfInput <|> (A.anyChar *> loop))

textBeforeCI :: ByteString -> A.Parser ByteString
textBeforeCI marker = withoutMarker . fst <$> A.match (skipPastCI marker)
  where
    withoutMarker s =
      let n = B.length s - B.length marker
       in if n >= 0 && asciiLower (B.drop n s) == marker then B.take n s else s

metaTags :: [(ByteString, ByteString)] -> HeadTags
metaTags attrs =
  HeadTags
    { ogTitle = property "og:title",
      ogDescription = property "og:description",
      ogImage = property "og:image",
      titleText = Nothing,
      nameDescription = content <* guard (attr "name" == Just "description"),
      metaCharset = lookup "charset" attrs <|> (guard (attr "http-equiv" == Just "content-type") *> (snd . contentType =<< content))
    }
  where
    attr name = asciiLower <$> lookup name attrs
    content = lookup "content" attrs
    property name = content <* guard (attr "property" == Just name)

htmlPage :: Uri -> Maybe ByteString -> HeadTags -> Either String Page
htmlPage pageUri charset HeadTags {ogTitle, ogDescription, ogImage, titleText, nameDescription, metaCharset} = do
  decode <- charsetDecoder $ charset <|> metaCharset
  let firstValue = firstText decode . catMaybes
  pageTitle <- maybe (Left "no title") Right =<< firstValue [ogTitle, titleText]
  pageDescription <- maybe "" (T.take maxDescriptionLength) <$> firstValue [ogDescription, nameDescription]
  imageRef <- maybe (Left "no image") Right =<< firstValue [ogImage]
  pageImage <- imageUri pageUri $ encodeUtf8 imageRef
  pure HtmlPage {pageTitle, pageDescription, pageImage}

firstText :: (ByteString -> Either String Text) -> [ByteString] -> Either String (Maybe Text)
firstText decode = \case
  [] -> Right Nothing
  v : vs -> decode v >>= \t -> let t' = htmlValue t in if T.null t' then firstText decode vs else Right $ Just t'
  where
    htmlValue = T.unwords . T.words . decodeEntities

charsetDecoder :: Maybe ByteString -> Either String (ByteString -> Either String Text)
charsetDecoder = \case
  Nothing -> Right utf8
  Just charset
    | name `elem` ["utf-8", "utf8", "us-ascii"] -> Right utf8
    | name `elem` ["iso-8859-1", "windows-1252"] -> Right $ Right . windows1252
    | otherwise -> Left $ "unsupported charset: " <> B.unpack name
    where
      name = asciiLower $ unquote $ trim charset
  where
    utf8 = first show . decodeUtf8'

windows1252 :: ByteString -> Text
windows1252 = T.pack . map (\c -> fromMaybe c $ lookup c table) . B.unpack
  where
    table = zip ['\x80' .. '\x9f'] "\x20AC\x81\x201A\x0192\x201E\x2026\x2020\x2021\x02C6\x2030\x0160\x2039\x0152\x8D\x017D\x8F\x90\x2018\x2019\x201C\x201D\x2022\x2013\x2014\x02DC\x2122\x0161\x203A\x0153\x9D\x017E\x0178"

decodeEntities :: Text -> Text
decodeEntities t = case T.breakOn "&" t of
  (before, "") -> before
  (before, rest) -> before <> entity (T.drop 1 rest)
  where
    entity s = case T.breakOn ";" $ T.take 10 s of
      (name, end) | not (T.null end), Just c <- entityChar name -> T.singleton c <> decodeEntities (T.drop (T.length name + 1) s)
      _ -> "&" <> decodeEntities s
    entityChar = \case
      "amp" -> Just '&'
      "lt" -> Just '<'
      "gt" -> Just '>'
      "quot" -> Just '"'
      "apos" -> Just '\''
      "nbsp" -> Just '\xa0'
      name -> case T.uncons name of
        Just ('#', ref) -> case T.uncons ref of
          Just (x, hex) | x == 'x' || x == 'X' -> codePoint 16 isHexDigit hex
          _ -> codePoint 10 isDigit ref
        _ -> Nothing
    codePoint :: Int -> (Char -> Bool) -> Text -> Maybe Char
    codePoint base isBaseDigit digits
      | not (T.null digits) && T.all isBaseDigit digits && n > 0 && n <= 0x10FFFF && (n < 0xD800 || n > 0xDFFF) = Just $ chr n
      | otherwise = Nothing
      where
        n = T.foldl' (\a d -> a * base + digitToInt d) 0 digits

asciiLower :: ByteString -> ByteString
asciiLower = B.map $ \c -> if isAsciiUpper c then toLower c else c

trim :: ByteString -> ByteString
trim = B.dropWhile space . B.dropWhileEnd space
  where
    space c = c == ' ' || c == '\t'

unquote :: ByteString -> ByteString
unquote s = case B.uncons s of
  Just (q, s') | q == '"' || q == '\'', Just (s'', q') <- B.unsnoc s', q' == q -> s''
  _ -> s
