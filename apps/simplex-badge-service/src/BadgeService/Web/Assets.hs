{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

-- | The set of files the site serves, and the token resolver that turns @\@\@name\@\@@ in
-- @index.html@ into the URL that file is served at (D4).
--
-- Two rules here are load-bearing and are shared with @web/build.mjs@, which resolves the same
-- tokens at build time for @dist/dev.html@ (D1). If the two disagree, a token resolves in the
-- development build and fails in production:
--
-- * a file is named by its path relative to the root of the built output -- @main.js@, and
--   @img\/x.svg@ if an asset is ever nested -- and the two files that stay at @web\/@
--   (@index.html@ and @styles.css@) by their bare name;
-- * a token name is @[A-Za-z0-9_.-]+@, JavaScript's @[\\w.-]+@ (written out because Haskell's
--   'Data.Char.isAlphaNum' is Unicode-wide and JavaScript's @\\w@ is not). That charset cannot
--   contain a @\/@, so no token can name a nested asset at all. Harmless while every asset is
--   flat, and deliberately identical to @build.mjs@'s pattern rather than quietly wider here.
--   Scanning agrees too, down to where it resumes after a @\@\@@ that begins no token -- see
--   'substituteTokens'.
--
-- The build hash is ONE SHA-256 over the whole set, not one per file, and every file is served
-- under that single prefix. @tsc@ does not rewrite import specifiers (decision 7), so @main.js@
-- resolves @.\/ui.js@ against its own directory: a per-file hash would put every sibling module
-- at a different prefix and 404 the whole module graph. One prefix still changes whenever any
-- file changes, which is the cache-busting property that matters.
module BadgeService.Web.Assets
  ( ServedAssets (..),
    embeddedAssets,
    readServedAssets,
    assetContentType,
    substituteTokens,
    indexHtmlName,
  )
where

import qualified Control.Exception as E
import Control.Monad (foldM, forM)
import qualified Data.ByteArray.Encoding as BA
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.FileEmbed (embedDir, embedFile)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Simplex.Messaging.Crypto as C
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (splitDirectories, (</>))

-- | Every file the service serves, keyed as a token names it, and the one hash they are all
-- served under. Built once at startup from the embedded bytes, or per request from @web_dir@.
data ServedAssets = ServedAssets
  { assetsHash :: Text,
    assetFiles :: Map Text ByteString
  }
  deriving (Eq, Show)

-- | @index.html@ is served at @\/@ with its tokens substituted, and is in the served set as
-- well, so that @\@\@index.html\@\@@ resolves the same way it does in @build.mjs@.
indexHtmlName :: Text
indexHtmlName = "index.html"

-- | Served from @web\/@ rather than from @dist\/@ (D1), so a @web_dir@ edit needs no rebuild.
stylesheetName :: Text
stylesheetName = "styles.css"

-- | @npm run build@ writes this into @dist\/@ for design work over a local static server. It is
-- filtered out of the served set here and is never routed; 'embedDir' takes no predicate, so its
-- bytes are still in the binary.
devHtmlName :: Text
devHtmlName = "dev.html"

-- | The only token that does not name a file: its value is @[web] support_contact@.
supportContactToken :: Text
supportContactToken = "support_contact"

-- | The path a served file is fetched at. One prefix for the whole set -- see the module header.
assetUrlPath :: ServedAssets -> Text -> Text
assetUrlPath ServedAssets {assetsHash} name = "/assets/" <> assetsHash <> "/" <> name

mkServedAssets :: [(Text, ByteString)] -> Either String ServedAssets
mkServedAssets files = do
  m <- foldM insertUnique M.empty files
  pure ServedAssets {assetsHash = setHash m, assetFiles = m}
  where
    insertUnique m (name, bs)
      | M.member name m =
          Left $
            "two served files are both named " <> T.unpack name
              <> "; compiled modules, copied assets and the files at web/ share one flat namespace"
      | otherwise = Right $ M.insert name bs m

-- | One SHA-256 over the names and bytes of the whole set, in name order (see the module
-- header). Each file contributes its name and its own digest, both of fixed or newline-free
-- form, so no two different sets can produce the same input to the outer hash.
setHash :: Map Text ByteString -> Text
setHash m = base16 . C.sha256Hash . BS.concat $ concatMap entry (M.toAscList m)
  where
    entry (name, bs) = [encodeUtf8 name, "\n", base16Bytes (C.sha256Hash bs), "\n"]
    base16Bytes = BA.convertToBase BA.Base16
    base16 = decodeUtf8 . base16Bytes

-- | The @Content-Type@ for a served file, by extension. A file with no known extension is served
-- as @application\/octet-stream@ rather than guessed at: with @X-Content-Type-Options: nosniff@
-- the browser will not second-guess it either, so a wrong type fails visibly.
assetContentType :: Text -> ByteString
assetContentType name = case T.toLower (T.takeWhileEnd (/= '.') name) of
  "js" -> "text/javascript; charset=utf-8"
  "css" -> "text/css; charset=utf-8"
  "html" -> "text/html; charset=utf-8"
  "json" -> "application/json"
  "svg" -> "image/svg+xml"
  "png" -> "image/png"
  _ -> "application/octet-stream"

-- Embedded assets ------------------------------------------------------------

-- | The committed build (decision 2 and 7): read at COMPILE time, which is why D8 gates
-- @web\/dist\/@ in CI -- a stale @dist\/@ is embedded silently otherwise.
embeddedDist :: [(FilePath, ByteString)]
embeddedDist = $(embedDir "apps/simplex-badge-service/web/dist")

embeddedIndexHtml :: ByteString
embeddedIndexHtml = $(embedFile "apps/simplex-badge-service/web/index.html")

embeddedStylesheet :: ByteString
embeddedStylesheet = $(embedFile "apps/simplex-badge-service/web/styles.css")

-- | The served set as embedded in the binary. 'Left' only for a name collision, which is a build
-- mistake and is reported at startup by 'BadgeService.Web.Server.newWebServer'.
embeddedAssets :: Either String ServedAssets
embeddedAssets =
  mkServedAssets $
    [(indexHtmlName, embeddedIndexHtml), (stylesheetName, embeddedStylesheet)]
      <> [(name, bs) | (path, bs) <- embeddedDist, let name = toAssetName path, name /= devHtmlName]

-- | A path relative to the built output, as a token names it: POSIX separators whatever
-- 'embedDir' or the local filesystem used.
toAssetName :: FilePath -> Text
toAssetName = T.intercalate "/" . map T.pack . splitDirectories

-- web_dir assets -------------------------------------------------------------

-- | The same set read from disk instead of from the binary -- @[web] web_dir@, development only
-- (decision 2). The directory is @web\/@ itself: @dist\/@ under it, less @dev.html@, plus
-- @index.html@ and @styles.css@ beside it, which is exactly what is embedded, so the same tokens
-- resolve to the same URLs in both modes.
--
-- The set is ENUMERATED from the directory, and 'BadgeService.Web.Server' serves a request only
-- if its name is a key of this map. No request path is ever joined onto @dir@, so a @..@ segment
-- or an absolute path cannot escape the directory: it is simply not a key. (A symlink inside the
-- directory that points outside it would be followed, which is the operator's own doing in a
-- mode documented as development-only.)
readServedAssets :: FilePath -> IO (Either String ServedAssets)
readServedAssets dir = do
  let distDir = dir </> "dist"
  hasDist <- doesDirectoryExist distDir
  if not hasDist
    then pure . Left $ "[web] web_dir " <> dir <> ": no " <> distDir <> " directory (run npm run build there)"
    else do
      roots <- forM [indexHtmlName, stylesheetName] $ \name -> do
        let path = dir </> T.unpack name
        exists <- doesFileExist path
        if exists then readAsset name path else pure . Left $ "[web] web_dir " <> dir <> ": no " <> path
      names <- listAssetNames distDir
      dist <- forM (filter (/= devHtmlName) names) $ \name -> readAsset name (distDir </> T.unpack name)
      pure $ mkServedAssets =<< sequence (roots <> dist)

-- | A file the directory listing (or 'doesFileExist') just said was there can still fail to open:
-- a dangling symlink, a permission bit, a file deleted between the walk and the read. In a mode
-- whose whole point is that the directory changes under a running service, that is ordinary rather
-- than exceptional, so it becomes the same 'Left' every other web_dir problem is -- a 500 naming
-- the file in the log -- instead of an exception escaping into Warp.
readAsset :: Text -> FilePath -> IO (Either String (Text, ByteString))
readAsset name path =
  E.try (BS.readFile path) >>= \case
    Right bs -> pure $ Right (name, bs)
    Left (e :: E.IOException) -> pure . Left $ "[web] web_dir: cannot read " <> path <> ": " <> show e

-- | Every file under @dir@, as a name relative to it. Hidden files are skipped, matching both
-- 'embedDir' (which skips them too) and @build.mjs@, so @.gitkeep@ and an editor's swap file are
-- not part of the set and do not change the build hash.
listAssetNames :: FilePath -> IO [Text]
listAssetNames dir = map toAssetName <$> walk ""
  where
    walk prefix = do
      entries <- filter visible <$> listDirectory (dir </> prefix)
      concat <$> mapM (child prefix) entries
    child prefix name = do
      let path = if null prefix then name else prefix </> name
      isDir <- doesDirectoryExist (dir </> path)
      if isDir then walk path else pure [path]
    visible = \case
      '.' : _ -> False
      _ -> True

-- Token substitution ---------------------------------------------------------

-- | @index.html@ with every @\@\@name\@\@@ resolved: to the served URL of the file of that name,
-- or, for the one non-file token, to @[web] support_contact@. A token naming nothing is an
-- error, never a page served with a dead link or a literal token in it -- 'newWebServer' runs
-- this at startup for exactly that reason, so the service refuses to start rather than serve a
-- broken page.
--
-- The rule is generic on purpose: a later step that adds an asset adds a token and nothing else.
substituteTokens :: Text -> ServedAssets -> Text -> Either String Text
substituteTokens supportContact assets = go
  where
    go t = case T.breakOn "@@" t of
      (before, rest)
        | T.null rest -> Right before
        | otherwise ->
            let body = T.drop 2 rest
                (name, rest') = T.span isTokenChar body
             in if not (T.null name) && "@@" `T.isPrefixOf` rest'
                  then do
                    value <- resolve name
                    after <- go (T.drop 2 rest')
                    pure $ before <> value <> after
                  else do
                    -- Not a token. Resume ONE character on, not two: a JavaScript global regex
                    -- that fails at an index retries at the next index, so in "@@@x@@"
                    -- build.mjs matches the "@@x@@" starting at index 1. Skipping both @s here
                    -- would leave that literal in the page with no startup error, and the two
                    -- resolvers would disagree on an input build.mjs accepts.
                    after <- go (T.drop 1 rest)
                    pure $ before <> "@" <> after
    resolve name
      | name == supportContactToken = Right $ escapeHtml supportContact
      | M.member name (assetFiles assets) = Right $ assetUrlPath assets name
      | otherwise =
          Left $
            "index.html references @@" <> T.unpack name <> "@@, which is neither a served file ("
              <> T.unpack (T.intercalate ", " (M.keys (assetFiles assets)))
              <> ") nor the "
              <> T.unpack supportContactToken
              <> " token"
    isTokenChar c = isAsciiUpper c || isAsciiLower c || isDigit c || c == '_' || c == '.' || c == '-'

-- | Only the support contact needs this: it comes from the operator's ini and is substituted into
-- an @href@ attribute, so an unescaped quote in it would end the attribute. The asset URLs are
-- built here out of a hex digest and a token-charset name, and contain nothing to escape.
escapeHtml :: Text -> Text
escapeHtml = T.concatMap $ \case
  '&' -> "&amp;"
  '<' -> "&lt;"
  '>' -> "&gt;"
  '"' -> "&quot;"
  '\'' -> "&#39;"
  c -> T.singleton c
