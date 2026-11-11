{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Simplex.Chat.BuildCommits
  ( Commit (..),
    simplexChatCommitQ,
    simplexmqCommitQ,
    gitHeadCommit,
    cabalProjectCommit,
  )
where

import Control.Exception (IOException, try)
import Control.Monad ((>=>))
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char (isHexDigit)
import Data.Maybe (listToMaybe, mapMaybe)
import Language.Haskell.TH (Exp, Q, runIO)
import Language.Haskell.TH.Syntax (addDependentFile)
import System.Directory (doesDirectoryExist)
import System.FilePath (isAbsolute, (</>))

data Commit = Commit {hash :: ByteString, readFrom :: [FilePath]}
  deriving (Eq, Show)

simplexChatCommitQ :: Q Exp
simplexChatCommitQ = commitQ "simplex-chat-commit" $ gitHeadCommit "."

simplexmqCommitQ :: Q Exp
simplexmqCommitQ = commitQ "simplexmq-commit" $ cabalProjectCommit "."

-- Nix builds have no .git, so flake.nix writes the commits into these files before compiling
commitQ :: FilePath -> IO (Maybe Commit) -> Q Exp
commitQ file derive = do
  commit_ <- runIO $ maybe derive (pure . Just) =<< fileCommit file
  s <- case commit_ of
    Just Commit {hash, readFrom} -> B.unpack hash <$ mapM_ addDependentFile readFrom
    Nothing -> pure ""
  [|s|]

fileCommit :: FilePath -> IO (Maybe Commit)
fileCommit path = (>>= validCommit [path]) <$> readFileMaybe path

-- a worktree's .git is a file naming its git directory, and its branches are in the common directory
gitHeadCommit :: FilePath -> IO (Maybe Commit)
gitHeadCommit dir = do
  let dotGit = dir </> ".git"
  isDir <- doesDirectoryExist dotGit
  gitDir_ <-
    if isDir
      then pure $ Just dotGit
      else fmap (relativeTo dir . B.unpack . B.strip) . (B.stripPrefix "gitdir:" =<<) <$> readFileMaybe dotGit
  maybe (pure Nothing) headCommit gitDir_
  where
    headCommit gitDir = do
      commonDir <- maybe gitDir (relativeTo gitDir . B.unpack . B.strip) <$> readFileMaybe (gitDir </> "commondir")
      let headPath = gitDir </> "HEAD"
      readFileMaybe headPath >>= \case
        Nothing -> pure Nothing
        Just h -> case B.stripPrefix "ref:" (B.strip h) of
          Nothing -> pure $ validCommit [headPath] h
          Just ref -> refCommit headPath commonDir (B.strip ref)
    refCommit headPath commonDir ref = do
      let loosePath = commonDir </> B.unpack ref
          packedPath = commonDir </> "packed-refs"
      readFileMaybe loosePath >>= \case
        Just h -> pure $ validCommit [headPath, loosePath] h
        Nothing -> (>>= packedRef ref >=> validCommit [headPath, packedPath]) <$> readFileMaybe packedPath
    packedRef ref = listToMaybe . mapMaybe (refLine ref) . B.lines
    refLine ref line = case B.words line of
      [h, name] | name == ref -> Just h
      _ -> Nothing
    relativeTo base path = if isAbsolute path then path else base </> path

cabalProjectCommit :: FilePath -> IO (Maybe Commit)
cabalProjectCommit dir = do
  let path = dir </> "cabal.project"
  (>>= either (const Nothing) (validCommit [path]) . A.parseOnly tagP) <$> readFileMaybe path
  where
    tagP =
      A.manyTill' A.anyChar "location: https://github.com/simplex-chat/simplexmq.git"
        *> A.takeWhile (== ' ')
        *> A.endOfLine
        *> A.takeWhile (== ' ')
        *> "tag: "
        *> A.takeWhile (A.notInClass " \r\n")

validCommit :: [FilePath] -> ByteString -> Maybe Commit
validCommit paths s
  | B.length h == 40 && B.all isHexDigit h = Just Commit {hash = h, readFrom = paths}
  | otherwise = Nothing
  where
    h = B.strip s

readFileMaybe :: FilePath -> IO (Maybe ByteString)
readFileMaybe path = either (\(_ :: IOException) -> Nothing) Just <$> try (B.readFile path)
