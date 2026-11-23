{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module LinkPreviewTests where

import Control.Monad (forM_)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Either (isLeft)
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.X509 (SignedCertificate)
import qualified Data.X509.CertificateStore as XS
import Data.X509.Memory (readSignedObjectFromMemory)
import Simplex.Chat.LinkPreview
import Simplex.Messaging.Client (NetworkConfig (..), SocksMode (..), defaultNetworkConfig)
import Simplex.Messaging.Transport.Client (TransportHost (..), defaultSocksProxyWithAuth)
import Test.Hspec
import Test.QuickCheck (Gen, choose, elements, forAll, ioProperty, listOf, property)

linkPreviewTests :: Spec
linkPreviewTests = do
  describe "head scanner" $ do
    it "should read og tags, title, description and charset" testScanTags
    it "should read quoted and unquoted attributes in any case" testScanAttributes
    it "should skip comments, scripts and styles" testScanSkipped
    it "should stop at the end of head, at body and after og tags" testScanStop
    it "should scan a body split into chunks as the whole body" $ property $ forAll splitBody $ \(sizes, body) ->
      ioProperty $ (==) <$> scanChunks (splitChunks sizes body) <*> scanChunks [body]
  describe "page" $ do
    it "should prefer og tags" testPagePriority
    it "should read the first og tag of each kind, with property or name attribute" testPageNamedOg
    it "should skip og tags with blank content" testPageBlankOg
    it "should decode entities and collapse whitespace" testPageValues
    it "should require title and image" testPageRequired
    it "should resolve image against the page" testPageImage
    it "should read at most the head limit of the page" testHeadLimit
    it "should limit image size" testImageLimit
    it "should refuse image by its declared length without reading it" testImageLength
    it "should read at most the limit and one byte of an endless image" testImageEndless
    it "should limit preview time to 15 seconds without SOCKS proxy" testPreviewDeadline
    it "should limit preview time to 30 seconds with SOCKS proxy in any SOCKS mode" testSocksPreviewDeadline
  describe "charsets" $ do
    it "should decode UTF-8 and windows-1252" testCharsets
    it "should refuse unsupported charsets and invalid UTF-8" testCharsetsRefused
  describe "URI" $ do
    it "should resolve RFC 3986 examples" testReferenceResolution
    it "should resolve network-path references and keep the port" testResolution
    it "should parse links" testLinks
    it "should use plain HTTP only for onion hosts" testOnionSchemes
  describe "address policy" $ do
    it "should prohibit local addresses and their IPv4-mapped forms" testAddressPolicy
  describe "HTTP" $ do
    it "should make request" testRequest
    it "should read chunked, Content-Length and close-delimited bodies" testBodies
    it "should limit response head" testHeadLimits
    it "should skip interim responses" testInterimResponses
    it "should limit chunks" testChunkLimits
    it "should refuse compressed bodies" testContentEncoding
  describe "root certificates" $ do
    it "should decode all embedded certificates" testRootCerts

listStream :: [ByteString] -> IO (ExceptT String IO ByteString)
listStream chunks = do
  ref <- newIORef $ filter (not . B.null) chunks
  pure $ liftIO $ atomicModifyIORef' ref $ \case
    [] -> ([], "")
    c : cs -> (cs, c)

endlessBody :: ByteString -> IO (IORef Int, ExceptT String IO ByteString)
endlessBody chunk = do
  pulled <- newIORef 0
  pure (pulled, liftIO $ chunk <$ modifyIORef' pulled (+ B.length chunk))

scanChunks :: [ByteString] -> IO (Either String HeadTags)
scanChunks chunks = runExceptT . scanHead =<< listStream chunks

scan :: ByteString -> IO HeadTags
scan s = scanChunks [s] >>= either fail pure

splitChunks :: [Int] -> ByteString -> [ByteString]
splitChunks sizes s = case sizes of
  n : ns | not (B.null s) -> let (chunk, rest) = B.splitAt n s in chunk : splitChunks ns rest
  _ -> [s]

splitBody :: Gen ([Int], ByteString)
splitBody = (,) <$> listOf (choose (1, 24)) <*> (B.concat <$> listOf (elements fragments))
  where
    fragments =
      [ "<!DOCTYPE html>",
        "<html>",
        "<head>",
        "</head>",
        "<body>",
        "<title>",
        "</TITLE>",
        "Title &amp; text",
        "<!--",
        "-->",
        "<script>",
        "</script>",
        "<style>",
        "</style>",
        "<meta property=\"og:title\" content=\"OG title\">",
        "<meta property='og:description' content='OG description'>",
        "<META PROPERTY=og:image CONTENT=/image.png>",
        "<meta name=\"og:title\" content=\"Name title\">",
        "<meta name=og:image content=/name.png>",
        "<meta name=description content=\"Meta description\">",
        "<meta charset=utf-8>",
        "<meta content=\"x\"",
        "<a href=x>",
        "</a>",
        "<br/>",
        "<?xml version=\"1.0\"?>",
        "<",
        ">",
        "\"",
        "'",
        "=",
        "/",
        "-",
        " ",
        "\r\n"
      ]

testScanTags :: IO ()
testScanTags =
  scan
    "<!DOCTYPE html><html><head><meta charset=\"utf-8\"><title>Page title</title>\
    \<meta name=\"description\" content=\"Meta description\">\
    \<meta property=\"og:title\" content=\"OG title\">\
    \<meta property=\"og:description\" content=\"OG description\">\
    \<meta property=\"og:image\" content=\"/image.png\"></head><body></body></html>"
    `shouldReturn` HeadTags
      { ogTitle = Just "OG title",
        ogDescription = Just "OG description",
        ogImage = Just "/image.png",
        titleText = Just "Page title",
        nameDescription = Just "Meta description",
        metaCharset = Just "utf-8"
      }

testScanAttributes :: IO ()
testScanAttributes = do
  scan "<META Property='og:title' CONTENT=Unquoted /><meta  property = \"og:image\"  content = \"/a b.png\" >"
    `shouldReturn` mempty {ogTitle = Just "Unquoted", ogImage = Just "/a b.png"}
  scan "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=windows-1252\"><TITLE>Title</Title>"
    `shouldReturn` mempty {metaCharset = Just "windows-1252", titleText = Just "Title"}

testScanSkipped :: IO ()
testScanSkipped = do
  scan "<!-- <meta property=og:title content=comment> --><meta property=og:title content=shown>"
    `shouldReturn` mempty {ogTitle = Just "shown"}
  scan "<script>document.write('<meta property=\"og:title\" content=\"script\">')</SCRIPT><meta property=og:title content=shown>"
    `shouldReturn` mempty {ogTitle = Just "shown"}
  scan "<style>a{}</style><title>a<b</title>" `shouldReturn` mempty {titleText = Just "a<b"}
  scan "<!-- <meta property=og:title content=comment>" `shouldReturn` mempty
  scan "<head><title>unterminated" `shouldReturn` mempty {titleText = Just "unterminated"}

testScanStop :: IO ()
testScanStop = do
  let og = "<meta property=og:title content=a><meta property=og:description content=b><meta property=og:image content=c>"
      namedOg = "<meta name=og:title content=a><meta name=og:description content=b><meta name=og:image content=c>"
      mixedOg = "<meta property=og:title content=a><meta name=og:description content=b><meta property=og:image content=c>"
  forM_ [og, namedOg, mixedOg] $ \s -> (s,) <$> scan (s <> "<title>late</title>") `shouldReturn` (s, mempty {ogTitle = Just "a", ogDescription = Just "b", ogImage = Just "c"})
  scan "<head><title>t</title></head><meta property=og:title content=late>" `shouldReturn` mempty {titleText = Just "t"}
  scan "<title>t</title><body><meta property=og:title content=late>" `shouldReturn` mempty {titleText = Just "t"}
  forM_ [og, namedOg, mixedOg, "<title>t</title></head>", "<body>"] $ \s -> case A.parse headP s of
    A.Done {} -> pure ()
    _ -> expectationFailure $ "scanner needs input after " <> B.unpack s

testBase :: Uri
testBase = either error id $ linkUri "https://example.com/dir/page"

testPage :: Maybe ByteString -> ByteString -> IO (Either String PageMeta)
testPage charset html = htmlPage testBase charset <$> scan html

htmlPageOf :: Text -> Text -> Text -> Either String PageMeta
htmlPageOf pageTitle pageDescription image = (\pageImage -> PageMeta {pageTitle, pageDescription, pageImage}) <$> linkUri image

testPagePriority :: IO ()
testPagePriority = do
  testPage Nothing "<title>Title</title><meta name=description content=Description><meta property=og:title content=OG><meta property=og:description content=\"OG description\"><meta property=og:image content=/i.png>"
    `shouldReturn` htmlPageOf "OG" "OG description" "https://example.com/i.png"
  testPage Nothing "<title>Title</title><meta name=description content=Description><meta property=og:title content=\" \"><meta property=og:image content=/i.png>"
    `shouldReturn` htmlPageOf "Title" "Description" "https://example.com/i.png"

testPageNamedOg :: IO ()
testPageNamedOg = do
  testPage Nothing "<title>Title</title><meta name=description content=Description><meta name=og:title content=Named><meta name=og:description content=\"Named description\"><meta name=og:image content=/n.png>"
    `shouldReturn` htmlPageOf "Named" "Named description" "https://example.com/n.png"
  testPage Nothing "<meta name=og:title content=Named><meta property=og:title content=OG><meta property=og:description content=\"OG description\"><meta name=og:description content=\"Named description\"><meta property=og:image content=/i.png><meta name=og:image content=/n.png>"
    `shouldReturn` htmlPageOf "Named" "OG description" "https://example.com/i.png"

testPageBlankOg :: IO ()
testPageBlankOg = do
  testPage Nothing "<meta property=og:title content=\"\"><meta property=og:description content=d><meta property=og:image content=/i.png><title>Title</title>"
    `shouldReturn` htmlPageOf "Title" "d" "https://example.com/i.png"
  testPage Nothing "<title>Title</title><meta name=og:image content=\" \"><meta property=og:image content=/i.png>"
    `shouldReturn` htmlPageOf "Title" "" "https://example.com/i.png"

testPageValues :: IO ()
testPageValues = do
  testPage Nothing "<title>  Tom &amp; Jerry\n &lt;3&gt; &#169; &#x1F600; &#X41; &unknown; &amp </title><meta property=og:image content=\"/i.png?a=1&amp;b=2\">"
    `shouldReturn` htmlPageOf "Tom & Jerry <3> \169 \128512 A &unknown; &amp" "" "https://example.com/i.png?a=1&b=2"
  let title = T.replicate 200000 "&amp;"
  testPage Nothing ("<title>" <> encodeUtf8 title <> "</title><meta property=og:image content=/i.png>")
    `shouldReturn` htmlPageOf (T.replicate 200000 "&") "" "https://example.com/i.png"

testPageRequired :: IO ()
testPageRequired = do
  testPage Nothing "<meta property=og:image content=/i.png>" `shouldReturn` Left "no title"
  testPage Nothing "<title>t</title><link rel=icon href=/icon.png>" `shouldReturn` Left "no image"
  testPage Nothing "<title>t</title><meta property=og:image content=data:image/png;base64,AAAA>" `shouldReturn` Left "no host"
  testPage Nothing "<title>t</title><meta property=og:image content=ftp://example.com/i.png>" `shouldReturn` Left "unsupported scheme: ftp"

testPageImage :: IO ()
testPageImage = do
  testPage Nothing "<title>t</title><meta property=og:image content=img.png>" `shouldReturn` htmlPageOf "t" "" "https://example.com/dir/img.png"
  testPage Nothing "<title>t</title><meta property=og:image content=//cdn.example.com/i.png>" `shouldReturn` htmlPageOf "t" "" "https://cdn.example.com/i.png"
  testPage Nothing "<title>t</title><meta property=og:image content=http://example.net/i.png>" `shouldReturn` htmlPageOf "t" "" "https://example.net/i.png"

testHeadLimit :: IO ()
testHeadLimit = do
  (pulled, body) <- endlessBody $ "<title>t</title>" <> B.replicate 1008 ' '
  runExceptT (pageContent testBase Response {headers = [("content-type", "text/html")], contentLength = Nothing, body}) >>= \case
    Left e -> e `shouldBe` "no image"
    Right _ -> expectationFailure "unexpected page"
  readIORef pulled `shouldReturn` maxHeadBytes

testImageLimit :: IO ()
testImageLimit = do
  (maxImageBytes, maxImageLinkBytes) `shouldBe` (2097152, 10485760)
  forM_ [maxImageBytes, maxImageLinkBytes] $ \limit -> do
    imageSize limit [B.replicate limit 'a'] `shouldReturn` Right limit
    imageSize limit [B.replicate limit 'a', "a"] `shouldReturn` Left "image too large"
  where
    imageSize limit chunks = fmap B.length <$> (readImage limit Nothing =<< listStream chunks)

testImageLength :: IO ()
testImageLength = do
  (pulled, body) <- endlessBody "a"
  readImage 1023 (Just 1024) body `shouldReturn` Left "image too large"
  readIORef pulled `shouldReturn` 0
  (readImage 1023 (Just 1023) =<< listStream [B.replicate 1023 'a']) `shouldReturn` Right (B.replicate 1023 'a')

testImageEndless :: IO ()
testImageEndless = do
  (pulled, body) <- endlessBody $ B.replicate 1024 'a'
  readImage 1023 Nothing body `shouldReturn` Left "image too large"
  readIORef pulled `shouldReturn` 1024

readImage :: Int -> Maybe Int -> ExceptT String IO ByteString -> IO (Either String ByteString)
readImage limit contentLength body = runExceptT $ imageBody limit Response {headers = [], contentLength, body}

testPreviewDeadline :: IO ()
testPreviewDeadline = linkPreviewDeadline defaultLinkPreviewConfig defaultNetworkConfig `shouldBe` 15_000000

testSocksPreviewDeadline :: IO ()
testSocksPreviewDeadline =
  forM_ [SMAlways, SMOnion] $ \socksMode ->
    (socksMode, linkPreviewDeadline defaultLinkPreviewConfig defaultNetworkConfig {socksProxy = Just defaultSocksProxyWithAuth, socksMode}) `shouldBe` (socksMode, 30_000000)

titleWith :: Maybe ByteString -> ByteString -> IO (Either String Text)
titleWith charset html = fmap (\PageMeta {pageTitle} -> pageTitle) <$> testPage charset (html <> "<meta property=og:image content=/i.png>")

testCharsets :: IO ()
testCharsets = do
  titleWith Nothing ("<title>" <> encodeUtf8 "\1055\1088\1080\1074\1077\1090" <> "</title>") `shouldReturn` Right "\1055\1088\1080\1074\1077\1090"
  titleWith (Just "Windows-1252") "<title>caf\233 \147quoted\148 \128</title>" `shouldReturn` Right "caf\233 \8220quoted\8221 \8364"
  titleWith Nothing "<meta charset=iso-8859-1><title>caf\233</title>" `shouldReturn` Right "caf\233"
  titleWith (Just "\"utf-8\"") ("<meta charset=windows-1252><title>" <> encodeUtf8 "caf\233" <> "</title>") `shouldReturn` Right "caf\233"
  titleWith (Just "us-ascii") "<title>ascii</title>" `shouldReturn` Right "ascii"

testCharsetsRefused :: IO ()
testCharsetsRefused = do
  titleWith (Just "shift_jis") "<title>t</title>" `shouldReturn` Left "unsupported charset: shift_jis"
  titleWith Nothing "<meta charset=koi8-r><title>t</title>" `shouldReturn` Left "unsupported charset: koi8-r"
  titleWith Nothing "<title>\255</title>" >>= (`shouldSatisfy` isLeft)

testReferenceResolution :: IO ()
testReferenceResolution = do
  Right base <- pure $ linkUri "https://a/b/c/d;p?q"
  forM_ examples $ \(ref, expected) -> (ref, imageUri base ref) `shouldBe` (ref, linkUri expected)
  imageUri base "g:h" `shouldBe` Left "no host"
  imageUri base "g://h" `shouldBe` Left "unsupported scheme: g"
  where
    examples =
      [ ("g", "https://a/b/c/g"),
        ("./g", "https://a/b/c/g"),
        ("g/", "https://a/b/c/g/"),
        ("/g", "https://a/g"),
        ("//g", "https://g"),
        ("?y", "https://a/b/c/d;p?y"),
        ("g?y", "https://a/b/c/g?y"),
        ("#s", "https://a/b/c/d;p?q"),
        ("g#s", "https://a/b/c/g"),
        ("g?y#s", "https://a/b/c/g?y"),
        (";x", "https://a/b/c/;x"),
        ("g;x", "https://a/b/c/g;x"),
        ("g;x?y#s", "https://a/b/c/g;x?y"),
        ("", "https://a/b/c/d;p?q"),
        (".", "https://a/b/c/"),
        ("./", "https://a/b/c/"),
        ("..", "https://a/b/"),
        ("../", "https://a/b/"),
        ("../g", "https://a/b/g"),
        ("../..", "https://a/"),
        ("../../", "https://a/"),
        ("../../g", "https://a/g"),
        ("../../../g", "https://a/g"),
        ("../../../../g", "https://a/g"),
        ("/./g", "https://a/g"),
        ("/../g", "https://a/g"),
        ("g.", "https://a/b/c/g."),
        (".g", "https://a/b/c/.g"),
        ("g..", "https://a/b/c/g.."),
        ("..g", "https://a/b/c/..g"),
        ("./../g", "https://a/b/g"),
        ("./g/.", "https://a/b/c/g/"),
        ("g/./h", "https://a/b/c/g/h"),
        ("g/../h", "https://a/b/c/h"),
        ("g;x=1/./y", "https://a/b/c/g;x=1/y"),
        ("g;x=1/../y", "https://a/b/c/y"),
        ("g?y/./x", "https://a/b/c/g?y/./x"),
        ("g?y/../x", "https://a/b/c/g?y/../x"),
        ("g#s/./x", "https://a/b/c/g"),
        ("g#s/../x", "https://a/b/c/g")
      ]

testResolution :: IO ()
testResolution = do
  Right page <- pure $ linkUri "https://example.com:8443/dir/page?x=1"
  imageUri page "img.png" `shouldBe` linkUri "https://example.com:8443/dir/img.png"
  imageUri page "//cdn.example.com/a.png" `shouldBe` linkUri "https://cdn.example.com/a.png"
  redirectUri page "/next" `shouldBe` linkUri "https://example.com:8443/next"
  redirectUri page "http://example.com/page" `shouldBe` Left "redirect to scheme: http"
  Right root <- pure $ linkUri "https://example.com"
  imageUri root "img.png" `shouldBe` linkUri "https://example.com/img.png"

testLinks :: IO ()
testLinks = do
  linkUri "https://Example.COM:443/path?q=1#frag" `shouldBe` Right Uri {uriScheme = USHttps, uriHost = THDomainName "example.com", uriPort = Nothing, uriPath = "/path", uriQuery = Just "q=1"}
  linkUri "http://example.com" `shouldBe` Right Uri {uriScheme = USHttps, uriHost = THDomainName "example.com", uriPort = Nothing, uriPath = "", uriQuery = Nothing}
  linkUri "https://user:password@example.com:/" `shouldBe` Right Uri {uriScheme = USHttps, uriHost = THDomainName "example.com", uriPort = Nothing, uriPath = "/", uriQuery = Nothing}
  linkUri "https://[::1]:8443/" `shouldBe` Right Uri {uriScheme = USHttps, uriHost = THIPv6 (0, 0, 0, 1), uriPort = Just 8443, uriPath = "/", uriQuery = Nothing}
  linkUri "https://10.0.0.1/" `shouldBe` Right Uri {uriScheme = USHttps, uriHost = THIPv4 (10, 0, 0, 1), uriPort = Nothing, uriPath = "/", uriQuery = Nothing}
  linkUri "https://127.0.0.1./" `shouldBe` Right Uri {uriScheme = USHttps, uriHost = THIPv4 (127, 0, 0, 1), uriPort = Nothing, uriPath = "/", uriQuery = Nothing}
  linkUri "https://Example.com.:8443/" `shouldBe` Right Uri {uriScheme = USHttps, uriHost = THDomainName "example.com", uriPort = Just 8443, uriPath = "/", uriQuery = Nothing}
  linkUri "https://example.onion/" `shouldBe` Right Uri {uriScheme = USHttps, uriHost = THOnionHost "example.onion", uriPort = Nothing, uriPath = "/", uriQuery = Nothing}
  linkUri "http://Example.onion.:80/" `shouldBe` Right Uri {uriScheme = USHttp, uriHost = THOnionHost "example.onion", uriPort = Nothing, uriPath = "/", uriQuery = Nothing}
  linkUri "ftp://example.com/" `shouldBe` Left "unsupported scheme: ftp"
  linkUri "example.com" `shouldBe` Left "relative link"
  linkUri "https:/path" `shouldBe` Left "no host"
  linkUri "https://example.com:0/" `shouldBe` Left "invalid port: :0"
  linkUri "https://example.com:65536/" `shouldBe` Left "invalid port: :65536"
  linkUri "https://exa mple.com/" `shouldBe` Left "invalid host: exa mple.com"
  linkUri "https://[::1/" `shouldBe` Left "invalid host: [::1"

testOnionSchemes :: IO ()
testOnionSchemes = do
  Right onion <- pure $ linkUri "http://example.onion:8080/dir/page"
  imageUri onion "img.png" `shouldBe` linkUri "http://example.onion:8080/dir/img.png"
  imageUri onion "//other.onion/i.png" `shouldBe` linkUri "http://other.onion/i.png"
  imageUri onion "//cdn.example.com/i.png" `shouldBe` linkUri "https://cdn.example.com/i.png"
  redirectUri onion "/next" `shouldBe` linkUri "http://example.onion:8080/next"
  redirectUri onion "//example.com/" `shouldBe` Left "redirect to scheme: http"
  Right page <- pure $ linkUri "https://example.com/"
  redirectUri page "http://example.onion/" `shouldBe` linkUri "http://example.onion/"
  map (fmap (\Uri {uriScheme} -> uriScheme) . linkUri) ["http://example.onion/", "https://example.onion/", "http://example.com/"]
    `shouldBe` [Right USHttp, Right USHttps, Right USHttps]

allowedHost :: AddressPolicy -> ByteString -> Bool
allowedHost policy host = case linkHost host of
  Right h | Just addr <- literalAddress h 443 -> allowedAddress policy addr
  _ -> error $ "not an IP address: " <> B.unpack host

testAddressPolicy :: IO ()
testAddressPolicy = do
  forM_ local $ \h -> (h, allowedHost APPublic h) `shouldBe` (h, False)
  forM_ public $ \h -> (h, allowedHost APPublic h) `shouldBe` (h, True)
  forM_ loopback $ \h -> (h, allowedHost APPublic h, allowedHost APPublicAndLoopback h) `shouldBe` (h, False, True)
  forM_ (filter (`notElem` loopback) local) $ \h -> (h, allowedHost APPublicAndLoopback h) `shouldBe` (h, False)
  forM_ public $ \h -> (h, allowedHost APPublicAndLoopback h) `shouldBe` (h, True)
  where
    loopback = ["127.0.0.1", "127.255.0.1", "[::1]", "[::ffff:127.0.0.1]"]
    local =
      loopback
        <> [ "0.0.0.0",
             "0.1.2.3",
             "10.1.2.3",
             "100.64.0.1",
             "100.127.255.254",
             "169.254.1.1",
             "172.16.0.1",
             "172.31.255.255",
             "192.168.1.1",
             "224.0.0.1",
             "239.255.255.255",
             "255.255.255.255",
             "[::]",
             "[fc00::1]",
             "[fd12:3456::1]",
             "[fe80::1]",
             "[ff02::1]",
             "[::ffff:0.0.0.0]",
             "[::ffff:10.0.0.1]",
             "[::ffff:100.64.0.1]",
             "[::ffff:169.254.1.1]",
             "[::ffff:172.16.0.1]",
             "[::ffff:192.168.1.1]",
             "[::ffff:224.0.0.1]",
             "[::ffff:255.255.255.255]"
           ]
    public = ["1.1.1.1", "8.8.8.8", "100.128.0.1", "172.32.0.1", "192.169.0.1", "[2001:4860:4860::8888]", "[::ffff:8.8.8.8]"]

testRequest :: IO ()
testRequest = do
  Right uri <- pure $ linkUri "https://example.com/a b/\252?q=1#frag"
  httpRequest uri
    `shouldBe` "GET /a%20b/%C3%BC?q=1 HTTP/1.1\r\n\
               \Host: example.com\r\n\
               \User-Agent: Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:160.0) Gecko/20100101 Firefox/160.0\r\n\
               \Accept: text/html,application/xhtml+xml,image/png,image/jpeg;q=0.9,*/*;q=0.1\r\n\
               \Accept-Encoding: identity\r\n\
               \Connection: close\r\n\r\n"
  Right root <- pure $ linkUri "https://example.com"
  httpRequest root `shouldSatisfy` ("GET / HTTP/1.1\r\n" `B.isPrefixOf`)
  Right x <- pure $ linkUri "https://x.com/user/status/1"
  httpRequest x `shouldSatisfy` ("\r\nUser-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_1) AppleWebKit/601.2.4 (KHTML, like Gecko) Version/9.0.1 Safari/601.2.4 facebookexternalhit/1.1 Facebot Twitterbot/1.0\r\n" `B.isInfixOf`)
  Right ipv6 <- pure $ linkUri "https://[::1]:8443/"
  httpRequest ipv6 `shouldSatisfy` ("\r\nHost: [::1]:8443\r\n" `B.isInfixOf`)
  Right explicitPort <- pure $ linkUri "https://example.com:443/"
  httpRequest explicitPort `shouldSatisfy` ("\r\nHost: example.com\r\n" `B.isInfixOf`)
  Right onion <- pure $ linkUri "http://example.onion:8080/"
  httpRequest onion `shouldSatisfy` ("\r\nHost: example.onion:8080\r\n" `B.isInfixOf`)

readResponse :: [ByteString] -> IO (Either String (Int, ByteString))
readResponse chunks = do
  conn <- newConn (\_ -> pure ()) =<< listStream chunks
  runExceptT $ do
    ResponseHead {status, headers} <- recvResponseHead conn
    Response {body} <- response conn headers
    (status,) <$> readBody body

testBodies :: IO ()
testBodies = do
  let chunked = "HTTP/1.1 200 OK\r\nContent-Type: text/html\r\nTransfer-Encoding: Chunked\r\n\r\n5;ext=1\r\nhello\r\n6\r\n world\r\n0\r\nTrailer: x\r\n\r\n"
  readResponse [chunked] `shouldReturn` Right (200, "hello world")
  readResponse (map B.singleton $ B.unpack chunked) `shouldReturn` Right (200, "hello world")
  let withLength = "HTTP/1.1 404 Not Found\r\ncontent-length: 5\r\n\r\nhello world"
  readResponse [withLength] `shouldReturn` Right (404, "hello")
  readResponse (map B.singleton $ B.unpack withLength) `shouldReturn` Right (404, "hello")
  readResponse ["HTTP/1.0 200 OK\r\n\r\nhel", "lo", " world"] `shouldReturn` Right (200, "hello world")
  readResponse ["HTTP/1.1 302 Found\r\nLocation: /next\r\nContent-Length: 0\r\n\r\n"] `shouldReturn` Right (302, "")

testHeadLimits :: IO ()
testHeadLimits = do
  readResponse ["HTTP/1.1 200 OK\r\nX-Large: " <> B.replicate 32768 'a' <> "\r\n\r\n"] `shouldReturn` Left "HTTP line too long"
  readResponse ["HTTP/1.1 200 OK\r\n" <> B.concat (replicate 101 "X-Header: a\r\n") <> "\r\n"] `shouldReturn` Left "too many HTTP headers"
  readResponse ["HTTP/1.1 200 OK\r\n" <> B.concat (replicate 100 "X-Header: a\r\n") <> "\r\nbody"] `shouldReturn` Right (200, "body")
  readResponse ["HTTP/1.1 200 OK\r\n" <> B.concat (replicate 3000 "X: a\r\n")] `shouldReturn` Left "too many HTTP headers"
  readResponse ["HTTP/1.1 200 OK\r\n" <> B.concat (replicate 99 ("X-Header: " <> B.replicate 320 'a' <> "\r\n")) <> "\r\n"] `shouldReturn` Left "HTTP line too long"
  readResponse ["HTTP/2 200\r\n\r\n"] >>= (`shouldSatisfy` isLeft)
  readResponse ["HTTP/1.1 200 OK\r\nno colon\r\n\r\n"] `shouldReturn` Left "invalid HTTP header: no colon"
  readResponse ["HTTP/1.1 200 OK\r\n"] `shouldReturn` Left "connection closed"

testInterimResponses :: IO ()
testInterimResponses = do
  readResponse ["HTTP/1.1 103 Early Hints\r\nLink: </s.css>; rel=preload\r\n\r\nHTTP/1.1 100 Continue\r\n\r\nHTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\nok"]
    `shouldReturn` Right (200, "ok")
  readResponse ["HTTP/1.1 101 Switching Protocols\r\n\r\n"] `shouldReturn` Right (101, "")
  readResponse [B.concat (replicate 2000 "HTTP/1.1 103 Early Hints\r\n\r\n")] `shouldReturn` Left "HTTP line too long"
  readResponse ["HTTP/1.1 103 Early Hints\r\n" <> B.concat (replicate 60 "Link: a\r\n") <> "\r\nHTTP/1.1 200 OK\r\n" <> B.concat (replicate 60 "X: a\r\n") <> "\r\n"]
    `shouldReturn` Left "too many HTTP headers"

testChunkLimits :: IO ()
testChunkLimits = do
  readResponse ["HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n100000000\r\nhello\r\n0\r\n\r\n"] `shouldReturn` Left "invalid chunk size: 100000000"
  readResponse ["HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n5\r\nhelloX\r\n0\r\n\r\n"] `shouldReturn` Left "invalid chunk end"
  readResponse ["HTTP/1.1 200 OK\r\nTransfer-Encoding: gzip\r\n\r\n"] `shouldReturn` Left "unsupported transfer encoding: gzip"
  readResponse ["HTTP/1.1 200 OK\r\nContent-Length: -1\r\n\r\n"] `shouldReturn` Left "invalid content length: -1"
  conn <- newConn (\_ -> pure ()) =<< listStream ["HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\nFFFFFFF\r\n", B.replicate 16384 'a', B.replicate 16384 'a']
  r <- runExceptT $ do
    ResponseHead {headers} <- recvResponseHead conn
    Response {body} <- response conn headers
    readBody =<< liftIO (limitBody 20000 body)
  r `shouldBe` Right (B.replicate 20000 'a')

testContentEncoding :: IO ()
testContentEncoding = do
  readResponse ["HTTP/1.1 200 OK\r\nContent-Encoding: gzip\r\nContent-Length: 4\r\n\r\nabcd"] `shouldReturn` Left "unsupported content encoding: gzip"
  readResponse ["HTTP/1.1 200 OK\r\nContent-Encoding: identity\r\nContent-Length: 4\r\n\r\nabcd"] `shouldReturn` Right (200, "abcd")

testRootCerts :: IO ()
testRootCerts = do
  let pemCerts = length $ filter ("-----BEGIN CERTIFICATE-----" `B.isPrefixOf`) $ B.lines linkPreviewRoots
  pemCerts `shouldSatisfy` (>= 100)
  length (readSignedObjectFromMemory linkPreviewRoots :: [SignedCertificate]) `shouldBe` pemCerts
  length (XS.listCertificates rootCerts) `shouldBe` pemCerts
  where
    LinkPreviewConfig {rootCerts} = defaultLinkPreviewConfig
