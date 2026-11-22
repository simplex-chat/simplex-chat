{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module ChatTests.LinkPreviews where

import ChatClient
import ChatTests.DBUtils
import ChatTests.Utils
import Codec.Picture (PixelRGBA8 (..), generateImage)
import Codec.Picture.Png (encodePng)
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Concurrent.Async (race_, withAsync)
import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Monad
import Control.Monad.Reader (runReaderT)
import qualified Data.Aeson as J
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.IORef
import Data.List (isInfixOf, isPrefixOf)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.X509.CertificateStore as XS
import ImageTests (pngSideAbove16384)
import Network.HTTP.Types (found302, hContentEncoding, hContentType, hLocation, notFound404, ok200)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import qualified Network.Wai as W
import qualified Network.Wai.Handler.Warp as W
import qualified Network.Wai.Handler.WarpTLS as WT
import Simplex.Chat.Controller (ChatConfig (..), ChatError (..), ChatErrorType (..), ChatResponse (..), CommandSource (..), SimpleNetCfg (..), defaultSimpleNetCfg)
import Simplex.Chat.Library.Commands (execChatCommand)
import Simplex.Chat.LinkPreview (AddressPolicy (..), LinkPreviewConfig (..), defaultLinkPreviewConfig)
import Simplex.Chat.Options (ChatOpts (..), CoreChatOpts (..))
import Simplex.Chat.Protocol (LinkPreview (..), MsgContent (MCLink, preview, text))
import Simplex.Chat.Types (ImageData (..))
import Simplex.Messaging.Transport.Client (SocksAuth (..), SocksProxy (..), SocksProxyWithAuth (..))
import System.Timeout (timeout)
import Test.Hspec hiding (it)

chatLinkPreviewTests :: SpecWith TestParams
chatLinkPreviewTests = do
  it "should get preview of a page" testPagePreview
  it "should resolve page image against the final URI" testRelativeImage
  it "should get preview of an image" testImagePreview
  it "should not use site icons" testNoPageImage
  it "should follow at most 5 redirects to public https addresses" testRedirects
  it "should prohibit loopback addresses with public address policy" testPublicAddressPolicy
  it "should prohibit loopback addresses through SOCKS proxy with public address policy" testPublicAddressPolicySocks
  it "should fetch http links as https" testHttpLinks
  it "should stop reading the page after og tags" testEndlessBody
  it "should stop reading the page after the head limit" testLargeHead
  it "should fail on slow server" testSlowServer
  it "should fail on compressed body and oversized image" testRefusedContent
  it "should fail on certificate of unknown CA" testUnknownCA
  it "should send preview with long non-Latin title and description" testLongTextPreview
  it "should use SOCKS proxy with credentials per preview" testSocksIsolate
  it "should use SOCKS proxy with configured credentials" testSocksUsername
  it "should fetch onion pages over HTTP through SOCKS proxy" testOnionHttp
  it "should show link preview in CLI" testLinkPreviewView
  it "should record link preview fixture" testLinkPreviewFixture

data LinkServer = LinkServer
  { requestedPaths :: TVar [ByteString],
    streamClosed :: TMVar Int
  }

data SocksRequest = SocksRequest
  { socksAuth :: Maybe (ByteString, ByteString),
    socksDestination :: SocksDestination,
    socksPort :: Int
  }

data SocksDestination = SDIPv4 | SDDomain ByteString | SDIPv6
  deriving (Eq, Show)

linkServerPort :: TestParams -> ServiceName
linkServerPort = testPort 5

socksServerPort :: TestParams -> ServiceName
socksServerPort = testPort 6

plainServerPort :: TestParams -> ServiceName
plainServerPort = testPort 7

pageUrl :: TestParams -> String -> String
pageUrl ps path = "https://localhost:" <> linkServerPort ps <> path

onionUrl :: TestParams -> String -> String
onionUrl ps path = "http://linkpreview.onion:" <> plainServerPort ps <> path

previewOf :: TestCC -> String -> IO (Either ChatError ChatResponse)
previewOf TestCC {chatController = cc} link = execChatCommand CSLocal (B.pack $ "/_link preview " <> link) 0 `runReaderT` cc

linkPreviewCfg :: IO ChatConfig
linkPreviewCfg = linkPreviewPolicyCfg APPublicAndLoopback

linkPreviewPolicyCfg :: AddressPolicy -> IO ChatConfig
linkPreviewPolicyCfg addressPolicy = do
  rootCerts <- XS.makeCertificateStore <$> XS.readCertificates "tests/fixtures/tls/link-preview/ca.crt"
  pure testCfg {linkPreviewConfig = LinkPreviewConfig {rootCerts, addressPolicy, previewDeadline = 10_000000}}

socksOpts :: TestParams -> SocksAuth -> ChatOpts
socksOpts ps auth = testOpts {coreOptions = testCoreOpts {simpleNetCfg = defaultSimpleNetCfg {socksProxy = Just $ SocksProxyWithAuth auth proxy}}}
  where
    proxy = SocksProxy $ SockAddrInet (fromIntegral (read $ socksServerPort ps :: Int)) (tupleToHostAddress (127, 0, 0, 1))

withLinkPreviewChat :: TestParams -> ChatConfig -> ChatOpts -> (LinkServer -> TestCC -> IO ()) -> IO ()
withLinkPreviewChat ps cfg opts test = withLinkServer ps $ \server -> testChatCfgOpts cfg opts aliceProfile (test server) ps

testPagePreview :: HasCallStack => TestParams -> IO ()
testPagePreview ps = do
  cfg <- linkPreviewCfg
  withLinkPreviewChat ps cfg testOpts $ \LinkServer {requestedPaths} alice -> do
    Right (CRLinkPreview LinkPreview {uri, title, description, image = ImageData image}) <- previewOf alice (pageUrl ps "/page")
    uri `shouldBe` T.pack (pageUrl ps "/page")
    title `shouldBe` "Link preview title"
    description `shouldBe` "Link preview description"
    image `shouldSatisfy` ("data:image/png;base64," `T.isPrefixOf`)
    readTVarIO requestedPaths `shouldReturn` ["/page", "/image.png"]

testRelativeImage :: HasCallStack => TestParams -> IO ()
testRelativeImage ps = do
  cfg <- linkPreviewCfg
  withLinkPreviewChat ps cfg testOpts $ \LinkServer {requestedPaths} alice -> do
    Right (CRLinkPreview LinkPreview {uri, title}) <- previewOf alice (pageUrl ps "/moved")
    uri `shouldBe` T.pack (pageUrl ps "/moved")
    title `shouldBe` "Link preview title"
    readTVarIO requestedPaths `shouldReturn` ["/moved", "/dir/page", "/dir/image.png"]

testImagePreview :: HasCallStack => TestParams -> IO ()
testImagePreview ps = do
  cfg <- linkPreviewCfg
  withLinkPreviewChat ps cfg testOpts $ \_ alice -> do
    Right (CRLinkPreview LinkPreview {title, description, image = ImageData image}) <- previewOf alice (pageUrl ps "/image.png")
    title `shouldBe` "image.png"
    description `shouldBe` ""
    image `shouldSatisfy` ("data:image/png;base64," `T.isPrefixOf`)

testNoPageImage :: HasCallStack => TestParams -> IO ()
testNoPageImage ps = do
  cfg <- linkPreviewCfg
  withLinkPreviewChat ps cfg testOpts $ \LinkServer {requestedPaths} alice -> do
    previewOf alice (pageUrl ps "/no-image") >>= (`shouldBeError` "no image")
    readTVarIO requestedPaths `shouldReturn` ["/no-image"]

testRedirects :: HasCallStack => TestParams -> IO ()
testRedirects ps = do
  cfg <- linkPreviewCfg
  withLinkPreviewChat ps cfg testOpts $ \LinkServer {requestedPaths} alice -> do
    Right (CRLinkPreview LinkPreview {title}) <- previewOf alice (pageUrl ps "/redirect/5")
    title `shouldBe` "Link preview title"
    readTVarIO requestedPaths `shouldReturn` ["/redirect/5", "/redirect/4", "/redirect/3", "/redirect/2", "/redirect/1", "/redirect/0", "/image.png"]
    atomically $ writeTVar requestedPaths []
    previewOf alice (pageUrl ps "/redirect/6") >>= (`shouldBeError` "too many redirects")
    readTVarIO requestedPaths `shouldReturn` ["/redirect/6", "/redirect/5", "/redirect/4", "/redirect/3", "/redirect/2", "/redirect/1"]
    previewOf alice (pageUrl ps "/to-private") >>= (`shouldBeError` "prohibited address: 10.0.0.1:443")
    previewOf alice (pageUrl ps "/to-http") >>= (`shouldBeError` "redirect to scheme: http")

testPublicAddressPolicy :: HasCallStack => TestParams -> IO ()
testPublicAddressPolicy ps = do
  cfg <- linkPreviewPolicyCfg APPublic
  withLinkPreviewChat ps cfg testOpts $ \LinkServer {requestedPaths} alice -> do
    previewOf alice (pageUrl ps "/page") >>= \case
      Left (ChatError (CELinkPreview e)) -> e `shouldSatisfy` ("prohibited address: " `isPrefixOf`)
      r -> expectationFailure $ "unexpected result: " <> show r
    readTVarIO requestedPaths `shouldReturn` []

testPublicAddressPolicySocks :: HasCallStack => TestParams -> IO ()
testPublicAddressPolicySocks ps = withSocksServer ps $ \socksRequests -> do
  cfg <- linkPreviewPolicyCfg APPublic
  withLinkPreviewChat ps cfg (socksOpts ps SocksIsolateByAuth) $ \LinkServer {requestedPaths} alice -> do
    previewOf alice ("https://127.0.0.1:" <> linkServerPort ps <> "/page") >>= (`shouldBeError` ("prohibited address: 127.0.0.1:" <> linkServerPort ps))
    map socksDestination . linkRequests (linkServerPort ps) <$> readTVarIO socksRequests `shouldReturn` []
    readTVarIO requestedPaths `shouldReturn` []

testHttpLinks :: HasCallStack => TestParams -> IO ()
testHttpLinks ps = do
  cfg <- linkPreviewCfg
  withLinkPreviewChat ps cfg testOpts $ \LinkServer {requestedPaths} alice -> do
    Right (CRLinkPreview LinkPreview {title}) <- previewOf alice ("http://localhost:" <> linkServerPort ps <> "/page")
    title `shouldBe` "Link preview title"
    previewOf alice (onionUrl ps "/page") >>= (`shouldBeError` "onion host without SOCKS proxy")
    readTVarIO requestedPaths `shouldReturn` ["/page", "/image.png"]

testEndlessBody :: HasCallStack => TestParams -> IO ()
testEndlessBody ps = do
  cfg <- linkPreviewCfg
  withLinkPreviewChat ps cfg testOpts $ \LinkServer {streamClosed} alice -> do
    Right (CRLinkPreview LinkPreview {title}) <- previewOf alice (pageUrl ps "/endless")
    title `shouldBe` "Link preview title"
    Just sent <- timeout 10_000000 $ atomically $ takeTMVar streamClosed
    sent `shouldSatisfy` (< 1048576)

testLargeHead :: HasCallStack => TestParams -> IO ()
testLargeHead ps = do
  cfg <- linkPreviewCfg
  withLinkPreviewChat ps cfg testOpts $ \LinkServer {streamClosed} alice -> do
    previewOf alice (pageUrl ps "/large-head") >>= (`shouldBeError` "no image")
    timeout 10_000000 (atomically $ takeTMVar streamClosed) >>= (`shouldSatisfy` (/= Nothing))

testSlowServer :: HasCallStack => TestParams -> IO ()
testSlowServer ps = do
  cfg@ChatConfig {linkPreviewConfig} <- linkPreviewCfg
  withLinkPreviewChat ps cfg {linkPreviewConfig = linkPreviewConfig {previewDeadline = 2_000000}} testOpts $ \_ alice ->
    previewOf alice (pageUrl ps "/slow") >>= (`shouldBeError` "timeout")

testRefusedContent :: HasCallStack => TestParams -> IO ()
testRefusedContent ps = do
  cfg <- linkPreviewCfg
  withLinkPreviewChat ps cfg testOpts $ \_ alice -> do
    previewOf alice (pageUrl ps "/gzip") >>= (`shouldBeError` "unsupported content encoding: gzip")
    previewOf alice (pageUrl ps "/oversized") >>= (`shouldBeError` "image width exceeds user limit")

testUnknownCA :: HasCallStack => TestParams -> IO ()
testUnknownCA ps =
  withLinkPreviewChat ps testCfg {linkPreviewConfig = defaultLinkPreviewConfig {addressPolicy = APPublicAndLoopback}} testOpts $ \_ alice ->
    previewOf alice (pageUrl ps "/page") >>= \case
      Left (ChatError (CELinkPreview e)) -> e `shouldSatisfy` ("certificate has unknown CA" `isInfixOf`)
      r -> expectationFailure $ "unexpected result: " <> show r

testLongTextPreview :: HasCallStack => TestParams -> IO ()
testLongTextPreview ps = do
  cfg <- linkPreviewCfg
  withLinkServer ps $ \_ -> testChatCfg2 cfg aliceProfile bobProfile test ps
  where
    test :: HasCallStack => TestCC -> TestCC -> IO ()
    test alice bob = do
      connectUsers alice bob
      let link = pageUrl ps "/long-text"
      Right (CRLinkPreview preview@LinkPreview {title, description, image = ImageData image}) <- previewOf alice link
      title `shouldBe` T.replicate 300 "\1046"
      description `shouldBe` T.replicate 500 "\20013"
      T.length image `shouldSatisfy` (<= 14000 - 602 - 1502)
      let content = J.encode $ J.object ["msgContent" J..= MCLink {text = T.pack link, preview}]
      alice ##> ("/_send @2 json [" <> T.unpack (decodeUtf8 $ LB.toStrict content) <> "]")
      alice <# ("@bob " <> link)
      bob <# ("alice> " <> link)

testSocksIsolate :: HasCallStack => TestParams -> IO ()
testSocksIsolate ps = withSocksServer ps $ \socksRequests -> do
  cfg <- linkPreviewCfg
  withLinkPreviewChat ps cfg (socksOpts ps SocksIsolateByAuth) $ \_ alice -> do
    replicateM_ 2 $ previewOf alice (pageUrl ps "/page") >>= (`shouldSatisfy` isPreview)
    requests <- linkRequests (linkServerPort ps) <$> readTVarIO socksRequests
    map socksDestination requests `shouldBe` replicate 4 (SDDomain "localhost")
    case map socksAuth requests of
      [Just (u1, ""), Just (u2, ""), Just (u3, ""), Just (u4, "")] -> do
        u1 `shouldBe` u2
        u3 `shouldBe` u4
        u1 `shouldNotBe` u3
      creds -> expectationFailure $ "unexpected credentials: " <> show creds

testSocksUsername :: HasCallStack => TestParams -> IO ()
testSocksUsername ps = withSocksServer ps $ \socksRequests -> do
  cfg <- linkPreviewCfg
  withLinkPreviewChat ps cfg (socksOpts ps SocksAuthUsername {username = "user", password = "pass"}) $ \_ alice -> do
    previewOf alice (pageUrl ps "/page") >>= (`shouldSatisfy` isPreview)
    requests <- linkRequests (linkServerPort ps) <$> readTVarIO socksRequests
    map socksAuth requests `shouldBe` replicate 2 (Just ("user", "pass"))

testOnionHttp :: HasCallStack => TestParams -> IO ()
testOnionHttp ps = withSocksServer ps $ \socksRequests -> do
  cfg <- linkPreviewCfg
  withLinkPreviewChat ps cfg (socksOpts ps SocksIsolateByAuth) $ \LinkServer {requestedPaths} alice -> do
    Right (CRLinkPreview LinkPreview {uri, title}) <- previewOf alice (onionUrl ps "/page")
    uri `shouldBe` T.pack (onionUrl ps "/page")
    title `shouldBe` "Link preview title"
    previewOf alice ("http://localhost:" <> linkServerPort ps <> "/page") >>= (`shouldSatisfy` isPreview)
    readTVarIO requestedPaths `shouldReturn` ["/page", "/image.png", "/page", "/image.png"]
    map socksDestination . linkRequests (plainServerPort ps) <$> readTVarIO socksRequests `shouldReturn` replicate 2 (SDDomain "linkpreview.onion")
    map socksDestination . linkRequests (linkServerPort ps) <$> readTVarIO socksRequests `shouldReturn` replicate 2 (SDDomain "localhost")

testLinkPreviewView :: HasCallStack => TestParams -> IO ()
testLinkPreviewView ps = do
  cfg <- linkPreviewCfg
  withLinkPreviewChat ps cfg testOpts $ \_ alice -> do
    alice ##> ("/_link preview " <> pageUrl ps "/page")
    alice <## "link preview: Link preview title"
    alice ##> ("/_link preview " <> pageUrl ps "/no-image")
    alice <## "link preview error: no image"

testLinkPreviewFixture :: HasCallStack => TestParams -> IO ()
testLinkPreviewFixture ps = withSocksServer ps $ \_ -> do
  cfg <- linkPreviewCfg
  withLinkPreviewChat ps cfg (socksOpts ps SocksIsolateByAuth) $ \_ alice -> do
    let origin = "https://link-preview.test:" <> linkServerPort ps
        links = [origin <> "/page", origin <> "/no-image"]
    apiFixture "previews/link_preview" alice [(origin, "LINK_URL")] FixtureRequests {frMarkdown = links, frUris = [], frCommands = map ("/_link preview " <>) links}

isPreview :: Either ChatError ChatResponse -> Bool
isPreview = \case
  Right CRLinkPreview {} -> True
  _ -> False

shouldBeError :: HasCallStack => Either ChatError ChatResponse -> String -> Expectation
shouldBeError r e = case r of
  Left (ChatError (CELinkPreview e')) -> e' `shouldBe` e
  _ -> expectationFailure $ "unexpected result: " <> show r

linkRequests :: ServiceName -> [SocksRequest] -> [SocksRequest]
linkRequests port = filter $ \SocksRequest {socksPort} -> socksPort == read port

withLinkServer :: TestParams -> (LinkServer -> IO a) -> IO a
withLinkServer ps action = do
  server <- LinkServer <$> newTVarIO [] <*> newEmptyTMVarIO
  tlsStarted <- newEmptyMVar
  plainStarted <- newEmptyMVar
  let settings port started =
        W.setPort (read port) $
          W.setHost "127.0.0.1" $
            W.setOnException (\_ _ -> pure ()) $
              W.setBeforeMainLoop (putMVar started ()) W.defaultSettings
      tls = WT.tlsSettings "tests/fixtures/tls/link-preview/server.crt" "tests/fixtures/tls/link-preview/server.key"
  withAsync (WT.runTLS tls (settings (linkServerPort ps) tlsStarted) $ linkServerApp server) $ \_ ->
    withAsync (W.runSettings (settings (plainServerPort ps) plainStarted) $ linkServerApp server) $ \_ ->
      takeMVar tlsStarted >> takeMVar plainStarted >> action server

linkServerApp :: LinkServer -> W.Application
linkServerApp LinkServer {requestedPaths, streamClosed} req respond = do
  atomically $ modifyTVar' requestedPaths (<> [path])
  respond =<< case path of
    "/page" -> pure $ html $ pageHtml "/image.png"
    "/image.png" -> pure $ image testPng
    "/icon.png" -> pure $ image testPng
    "/no-image" -> pure $ html "<html><head><title>No image</title><link rel=\"icon\" href=\"/icon.png\"></head><body></body></html>"
    "/moved" -> pure $ redirect "/dir/page"
    "/dir/page" -> pure $ html $ pageHtml "image.png"
    "/dir/image.png" -> pure $ image testPng
    "/to-private" -> pure $ redirect "https://10.0.0.1/page"
    "/to-http" -> pure $ redirect "http://localhost/page"
    "/gzip" -> pure $ W.responseLBS ok200 [(hContentType, "text/html"), (hContentEncoding, "gzip")] "\x1f\x8b\x08\x00"
    "/oversized" -> pure $ html $ pageHtml "/oversized.png"
    "/oversized.png" -> pure $ image $ LB.fromStrict pngSideAbove16384
    "/slow" -> html (pageHtml "/image.png") <$ threadDelay 5_000000
    "/long-text" -> pure $ html longTextHtml
    "/photo.jpg" -> pure $ W.responseFile ok200 [(hContentType, "image/jpeg")] "tests/fixtures/test.jpg" Nothing
    "/endless" -> pure $ stream (pageHead "/image.png") 1024 50_000
    "/large-head" -> pure $ stream "<html><head><title>Large head</title>" 16384 0
    _
      | Just n <- B.stripPrefix "/redirect/" path, Just (k, "") <- B.readInt n ->
          pure $ if k > 0 then redirect ("/redirect/" <> B.pack (show $ k - 1)) else html (pageHtml "/image.png")
      | otherwise -> pure $ W.responseLBS notFound404 [] ""
  where
    path = W.rawPathInfo req
    html = W.responseLBS ok200 [(hContentType, "text/html; charset=utf-8")]
    image = W.responseLBS ok200 [(hContentType, "image/png")]
    redirect location = W.responseLBS found302 [(hLocation, location)] ""
    stream start size delay = W.responseStream ok200 [(hContentType, "text/html")] $ \write flush -> do
      sent <- newIORef 0
      let sendChunk s = write (BB.byteString s) >> flush >> modifyIORef' sent (+ B.length s)
      (sendChunk start >> forever (sendChunk (B.take size $ B.concat $ replicate (size `div` 8 + 1) "<p>a</p>") >> threadDelay delay))
        `E.onException` (readIORef sent >>= atomically . void . tryPutTMVar streamClosed)

pageHead :: ByteString -> ByteString
pageHead imageUrl =
  "<!DOCTYPE html><html><head><meta charset=\"utf-8\"><title>Page title</title>\
  \<meta property=\"og:title\" content=\"Link preview title\">\
  \<meta property=\"og:description\" content=\"Link preview description\">\
  \<meta property=\"og:image\" content=\""
    <> imageUrl
    <> "\">"

pageHtml :: ByteString -> LB.ByteString
pageHtml imageUrl = LB.fromStrict $ pageHead imageUrl <> "</head><body>Page</body></html>"

longTextHtml :: LB.ByteString
longTextHtml =
  LB.fromStrict $
    "<html><head><meta property=\"og:title\" content=\""
      <> encodeUtf8 (T.replicate 400 "\1046")
      <> "\"><meta property=\"og:description\" content=\""
      <> encodeUtf8 (T.replicate 600 "\20013")
      <> "\"><meta property=\"og:image\" content=\"/photo.jpg\"></head></html>"

testPng :: LB.ByteString
testPng = encodePng $ generateImage (\x y -> PixelRGBA8 (fromIntegral $ x * 16) (fromIntegral $ y * 16) 128 (fromIntegral $ 255 - x * 8)) 16 16

withSocksServer :: TestParams -> (TVar [SocksRequest] -> IO a) -> IO a
withSocksServer ps action = do
  socksRequests <- newTVarIO []
  E.bracket listenSocks close $ \sock ->
    withAsync (forever $ accept sock >>= \(conn, _) -> forkIO $ serveSocks socksRequests conn `E.finally` close conn) $ \_ ->
      action socksRequests
  where
    listenSocks = do
      sock <- socket AF_INET Stream defaultProtocol
      setSocketOption sock ReuseAddr 1
      bind sock $ SockAddrInet (fromIntegral (read $ socksServerPort ps :: Int)) (tupleToHostAddress (127, 0, 0, 1))
      listen sock 16
      pure sock

serveSocks :: TVar [SocksRequest] -> Socket -> IO ()
serveSocks socksRequests conn = do
  [5, n] <- BS.unpack <$> recvExactly 2
  methods <- BS.unpack <$> recvExactly (fromIntegral n)
  socksAuth <-
    if 2 `elem` methods
      then do
        sendAll conn "\5\2"
        [1, userLength] <- BS.unpack <$> recvExactly 2
        user <- recvExactly $ fromIntegral userLength
        [passwordLength] <- BS.unpack <$> recvExactly 1
        password <- recvExactly $ fromIntegral passwordLength
        sendAll conn "\1\0"
        pure $ Just (user, password)
      else Nothing <$ sendAll conn "\5\0"
  [5, 1, 0, addressType] <- BS.unpack <$> recvExactly 4
  socksDestination <- case addressType of
    1 -> SDIPv4 <$ recvExactly 4
    3 -> do
      [nameLength] <- BS.unpack <$> recvExactly 1
      SDDomain <$> recvExactly (fromIntegral nameLength)
    _ -> SDIPv6 <$ recvExactly 16
  [p1, p2] <- BS.unpack <$> recvExactly 2
  let socksPort = fromIntegral p1 * 256 + fromIntegral p2
  atomically $ modifyTVar' socksRequests (<> [SocksRequest {socksAuth, socksDestination, socksPort}])
  E.bracket (socket AF_INET Stream defaultProtocol) close $ \upstream -> do
    connect upstream $ SockAddrInet (fromIntegral socksPort) (tupleToHostAddress (127, 0, 0, 1))
    sendAll conn "\5\0\0\1\0\0\0\0\0\0"
    race_ (relay conn upstream) (relay upstream conn)
  where
    recvExactly = recvAll []
    recvAll received n
      | n <= 0 = pure $ BS.concat $ reverse received
      | otherwise = recv conn n >>= \s -> if BS.null s then E.throwIO $ userError "connection closed" else recvAll (s : received) (n - BS.length s)
    relay from to = recv from 16384 >>= \s -> unless (BS.null s) $ sendAll to s >> relay from to
