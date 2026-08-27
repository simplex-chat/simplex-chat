{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The site's Warp listener (D4, decision 1): one process, one origin, no CORS. It serves the
-- index page at @\/@, the built site under @\/assets\/\<buildHash\>\/@ and the catalog at
-- @\/api\/catalog@; @\/api\/checkout@ (D6) and @\/webhooks\/*@ (E3, F2) are added to the same
-- listener by later steps.
--
-- The listener binds @[web] host@, which defaults to @127.0.0.1@ (A6), so a default deployment is
-- not exposed without a reverse proxy in front of it.
module BadgeService.Web.Server
  ( WebServer,
    ServedPage (..),
    newWebServer,
    resolveWebPage,
    runWebServer,
  )
where

import BadgeService.Catalog (catalogTotals)
import BadgeService.Config (BadgeServiceConfig (web), BadgeServiceEnv (..), WebConfig (..))
import BadgeService.Store (getActiveCatalog, withServiceTransaction)
import BadgeService.Web.Assets
import Control.Logger.Simple
import qualified Data.Aeson as J
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Network.HTTP.Types (Header, ResponseHeaders, Status, hCacheControl, hContentType, internalServerError500, methodNotAllowed405, notFound404, ok200)
import Network.Wai (Application, Response, pathInfo, requestMethod, responseLBS)
import qualified Network.Wai.Handler.Warp as Warp
import Simplex.Messaging.Util (tshow)

-- | What a request is answered from: the served set and the index page with its tokens already
-- resolved. Built once at startup from the embedded bytes, and per request in @web_dir@ mode.
data ServedPage = ServedPage
  { spAssets :: ServedAssets,
    spIndex :: ByteString
  }
  deriving (Eq, Show)

data WebServer = WebServer
  { wsConfig :: WebConfig,
    wsService :: BadgeServiceEnv,
    -- | 'Nothing' in @web_dir@ mode, where the page is re-read from disk on every request.
    wsPage :: Maybe ServedPage
  }

-- | The served set with @index.html@'s tokens resolved against it, from the binary or, in
-- @web_dir@ mode, from disk. Every failure is a message naming what is wrong: an unresolvable
-- token here is why the service refuses to start rather than serving a page with a dead asset
-- link or a literal @\@\@token\@\@@ in it.
resolveWebPage :: WebConfig -> IO (Either String ServedPage)
resolveWebPage cfg@WebConfig {webDir} = servedPage cfg <$> maybe (pure embeddedAssets) readServedAssets webDir

servedPage :: WebConfig -> Either String ServedAssets -> Either String ServedPage
servedPage WebConfig {webSupportContact} assets' = do
  assets <- assets'
  indexBytes <- maybe (Left $ "the served set has no " <> T.unpack indexHtmlName) Right $ M.lookup indexHtmlName (assetFiles assets)
  indexText <- first (\e -> T.unpack indexHtmlName <> " is not valid UTF-8: " <> show e) (decodeUtf8' indexBytes)
  html <- substituteTokens webSupportContact assets indexText
  pure ServedPage {spAssets = assets, spIndex = encodeUtf8 html}

-- | 'Nothing' when @[web]@ is absent: the service then runs with no listener at all, which is
-- what a bot-only deployment (no payment provider, codes minted by @codes@) wants. Every other
-- failure is fatal at startup, before the bot answers anything.
newWebServer :: BadgeServiceEnv -> IO (Either String (Maybe WebServer))
newWebServer env = case web (config env) of
  Nothing -> pure $ Right Nothing
  Just cfg ->
    resolveWebPage cfg >>= \case
      Left e -> pure $ Left e
      Right page ->
        pure . Right . Just $
          WebServer
            { wsConfig = cfg,
              wsService = env,
              -- web_dir re-reads on every request, so that an edited file is visible on reload
              wsPage = if isJust (webDir cfg) then Nothing else Just page
            }

runWebServer :: WebServer -> IO ()
runWebServer ws@WebServer {wsConfig = WebConfig {webPort, webHost, webDir}} = do
  logInfo $ "badge service web listener on " <> webHost <> ":" <> tshow webPort <> maybe "" (\d -> " serving " <> T.pack d <> " (web_dir, development only)") webDir
  -- Warp.run takes a port alone and cannot bind a configured host, so runSettings is used.
  Warp.runSettings (Warp.setPort webPort $ Warp.setHost (fromString $ T.unpack webHost) Warp.defaultSettings) (webApp ws)

webApp :: WebServer -> Application
webApp ws req respond
  | requestMethod req `notElem` ["GET", "HEAD"] =
      respond $ textResponse methodNotAllowed405 [("Allow", "GET, HEAD")] "method not allowed"
  | otherwise = case pathInfo req of
      [] -> withPage serveIndex
      [""] -> withPage serveIndex
      ["api", "catalog"] -> respond =<< serveCatalog ws
      ("assets" : buildHash : name : names) -> withPage $ \page -> serveAsset ws page buildHash (T.intercalate "/" (name : names))
      _ -> respond notFoundResponse
  where
    withPage serve =
      currentPage ws >>= \case
        Right page -> respond $ serve page
        Left e -> do
          logError $ "badge service web assets are unreadable: " <> T.pack e
          respond $ textResponse internalServerError500 [] "internal error"

-- | In @web_dir@ mode the whole set, its hash and the substituted index are recomputed per
-- request, so an edited file is visible on reload; every response in that mode is @no-store@, or
-- an edited @styles.css@ would keep its URL under an @immutable@ response and the browser would
-- not re-fetch it until the service restarted.
currentPage :: WebServer -> IO (Either String ServedPage)
currentPage WebServer {wsConfig, wsPage} = maybe (resolveWebPage wsConfig) (pure . Right) wsPage

serveIndex :: ServedPage -> Response
serveIndex ServedPage {spIndex} =
  responseLBS ok200 (securityHeaders <> [(hContentType, assetContentType indexHtmlName), (hCacheControl, "no-cache")]) (LBS.fromStrict spIndex)

-- | The whole served set sits under one hash prefix (see "BadgeService.Web.Assets"), so a
-- request under any other prefix is 404: an old prefix is a stale page's cached URL, and there is
-- no version of the site to answer it with.
--
-- @index.html@ is served here as well as at @\/@, with the same substituted bytes and the same
-- @no-cache@, so that a token naming it resolves to a URL that works. @dev.html@ is not in the
-- set at all and 404s here like any other unknown name.
serveAsset :: WebServer -> ServedPage -> Text -> Text -> Response
serveAsset ws page@ServedPage {spAssets} buildHash name
  | buildHash /= assetsHash spAssets = notFoundResponse
  | name == indexHtmlName = serveIndex page
  | otherwise = case M.lookup name (assetFiles spAssets) of
      Nothing -> notFoundResponse
      Just bytes ->
        responseLBS
          ok200
          (securityHeaders <> [(hContentType, assetContentType name), (hCacheControl, assetCacheControl ws)])
          (LBS.fromStrict bytes)

assetCacheControl :: WebServer -> ByteString
assetCacheControl WebServer {wsConfig = WebConfig {webDir}}
  | isJust webDir = "no-store"
  | otherwise = "public, max-age=31536000, immutable"

-- | The catalog in the RPC encoding (A2), so the site and the app parse the same shape. It is
-- read from the database through the same 'getActiveCatalog' and 'catalogTotals' the RPC handler
-- uses, never from @Catalog.hs@'s defaults, so a price deprecated or disabled by an operator is
-- reflected without a rebuild (decision 8).
serveCatalog :: WebServer -> IO Response
serveCatalog WebServer {wsService = BadgeServiceEnv {store}} =
  withServiceTransaction store (fmap catalogTotals . getActiveCatalog) >>= \case
    Right catalog -> pure $ jsonResponse ok200 (J.encode catalog)
    Left e -> do
      logError $ "badge service /api/catalog failed: " <> tshow e
      pure $ jsonResponse internalServerError500 "{\"error\":\"internal\"}"

-- | On every response, including a 404 and an error: the site loads no cross-origin resource, so
-- @default-src 'self'@ blocks nothing it needs.
securityHeaders :: ResponseHeaders
securityHeaders =
  [ ("Content-Security-Policy", "default-src 'self'"),
    ("X-Content-Type-Options", "nosniff"),
    ("Referrer-Policy", "no-referrer"),
    ("X-Frame-Options", "DENY")
  ]

notFoundResponse :: Response
notFoundResponse = textResponse notFound404 [] "not found"

textResponse :: Status -> [Header] -> LBS.ByteString -> Response
textResponse status headers =
  responseLBS status (securityHeaders <> headers <> [(hContentType, "text/plain; charset=utf-8"), (hCacheControl, "no-store")])

jsonResponse :: Status -> LBS.ByteString -> Response
jsonResponse status =
  responseLBS status (securityHeaders <> [(hContentType, "application/json"), (hCacheControl, "no-store")])
