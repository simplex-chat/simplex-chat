{-# LANGUAGE OverloadedStrings #-}

module BuildCommitsTests where

import qualified Data.ByteString.Char8 as B
import Simplex.Chat.BuildCommits
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (</>))
import Test.Hspec
import UnliftIO.Temporary (withSystemTempDirectory)

commitA, commitB :: B.ByteString
commitA = "0123456789abcdef0123456789abcdef01234567"
commitB = "89abcdef0123456789abcdef0123456789abcdef"

buildCommitsTests :: Spec
buildCommitsTests = do
  describe "simplex-chat commit from .git" $ do
    it "reads a detached HEAD" $ inDir $ \d -> do
      writeAt (d </> ".git/HEAD") $ commitA <> "\n"
      gitHeadCommit d `shouldReturn` Just (Commit commitA [d </> ".git/HEAD"])
    it "reads a branch from its loose ref" $ inDir $ \d -> do
      writeAt (d </> ".git/HEAD") "ref: refs/heads/master\n"
      writeAt (d </> ".git/refs/heads/master") $ commitA <> "\n"
      gitHeadCommit d `shouldReturn` Just (Commit commitA [d </> ".git/HEAD", d </> ".git/refs/heads/master"])
    it "reads a branch from packed refs" $ inDir $ \d -> do
      writeAt (d </> ".git/HEAD") "ref: refs/heads/master\n"
      writeAt (d </> ".git/packed-refs") $ "# pack-refs with: peeled fully-peeled sorted\n" <> commitB <> " refs/heads/other\n" <> commitA <> " refs/heads/master\n"
      gitHeadCommit d `shouldReturn` Just (Commit commitA [d </> ".git/HEAD", d </> ".git/packed-refs"])
    it "reads a worktree's HEAD and the branch in the common directory" $ inDir $ \d -> do
      let common = d </> "main/.git"
          gitDir = common </> "worktrees/wt"
      writeAt (d </> "wt/.git") $ "gitdir: " <> B.pack gitDir <> "\n"
      writeAt (gitDir </> "HEAD") "ref: refs/heads/feature\n"
      writeAt (gitDir </> "commondir") "../..\n"
      writeAt (common </> "refs/heads/feature") $ commitB <> "\n"
      gitHeadCommit (d </> "wt") `shouldReturn` Just (Commit commitB [gitDir </> "HEAD", gitDir </> "../.." </> "refs/heads/feature"])
    it "has no commit without .git" $ inDir $ \d ->
      gitHeadCommit d `shouldReturn` Nothing
    it "has no commit for a branch without a ref" $ inDir $ \d -> do
      writeAt (d </> ".git/HEAD") "ref: refs/heads/new\n"
      gitHeadCommit d `shouldReturn` Nothing
    it "has no commit for a HEAD that is not a hash" $ inDir $ \d -> do
      writeAt (d </> ".git/HEAD") "0123456789\n"
      gitHeadCommit d `shouldReturn` Nothing
  describe "simplexmq commit from cabal.project" $ do
    it "reads the tag of the simplexmq repository" $ inDir $ \d -> do
      writeAt (d </> "cabal.project") $
        "source-repository-package\n  type: git\n  location: https://github.com/simplex-chat/aeson.git\n  tag: "
          <> commitB
          <> "\n\nsource-repository-package\n  type: git\n  location: https://github.com/simplex-chat/simplexmq.git\n  tag: "
          <> commitA
          <> "\n"
      cabalProjectCommit d `shouldReturn` Just (Commit commitA [d </> "cabal.project"])
    it "reads this repository's cabal.project" $
      cabalProjectCommit "." >>= (`shouldSatisfy` maybe False ((== 40) . B.length . hash))
    it "has no commit without cabal.project" $ inDir $ \d ->
      cabalProjectCommit d `shouldReturn` Nothing
  where
    inDir = withSystemTempDirectory "build-commits"
    writeAt path s = do
      createDirectoryIfMissing True $ takeDirectory path
      B.writeFile path s
