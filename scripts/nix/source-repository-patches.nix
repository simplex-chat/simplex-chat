{ pkgs, cabalProject, sha256map, patchesByUrl }:
let
  parsed = pkgs.haskell-nix.haskellLib.parseSourceRepositoryPackages
    "cabal.project" sha256map {} cabalProject;
  repos = map (block: block.sourceRepo)
    (builtins.filter (block: block ? sourceRepo) parsed.sourceRepos);
  patchedSource = url: patches:
    let
      matches = builtins.filter (repo: repo.url == url) repos;
      repo = assert pkgs.lib.assertMsg (builtins.length matches == 1)
        "Expected exactly one pinned source repository for ${url}";
        builtins.head matches;
      rev = repo.rev or repo.ref;
    in {
      name = "${url}/${rev}";
      value = pkgs.applyPatches {
        src = pkgs.fetchgit {
          inherit (repo) url sha256;
          inherit rev;
        };
        inherit patches;
      };
    };
in builtins.listToAttrs (pkgs.lib.mapAttrsToList patchedSource patchesByUrl)
