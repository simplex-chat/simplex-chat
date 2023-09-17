---
title: Contributing guide
revision: 31.01.2023
---

| Updated 31.01.2023 | Languages: EN, [FR](/docs/lang/fr/CONTRIBUTING.md), [CZ](/docs/lang/cs/CONTRIBUTING.md) |

# Contributing guide

## Compiling with SQLCipher encryption enabled

Add `cabal.project.local` to project root with the location of OpenSSL headers and libraries and flag setting encryption mode:

```
cp scripts/cabal.project.local.mac cabal.project.local
# or
# cp scripts/cabal.project.local.linux cabal.project.local
```

## OpenSSL on MacOS

MacOS comes with LibreSSL as default, OpenSSL must be installed to compile SimpleX from source.

OpenSSL can be installed with `brew install openssl@1.1`

You will have to add `/opt/homebrew/opt/openssl@1.1/bin` to your PATH in order to have things working properly


## Project branches

- `stable` - stable release of the apps, can be used for updates to the previous stable release (GHC 9.6.2).

- `stable-android` - used to build stable Android core library with Nix (GHC 8.10.7).

- `stable-ios` - used to build stable iOS core library with Nix (GHC 8.10.7) – this branch should be the same as `stable-android` except Nix configuration files.

- `master` - branch for beta version releases (GHC 9.6.2).

- `master-android` - used to build beta Android core library with Nix (GHC 8.10.7).

- `master-ios` - used to build beta iOS core library with Nix (GHC 8.10.7) – this branch should be the same as `master-android` except Nix configuration files.


## Development & release process

1. Make PRs to `master` branch _only_.

2. To build Android core library, merge `master` branch to `master-android` branch, update code to be compatible with GHC 8.10.7, and push to GitHub.

3. To build iOS core library, merge `master-android` branch to `master-ios` branch, and push to GitHub.

4. To build Desktop and CLI apps, make tag in `master` branch, APK files should be attached to the release.

5. After the public release to App Store and Play Store, merge:
- `master` to `stable`
- `master` to `master-android` (and compile/update code)
- `master-android` to `master-ios`
- `master-android` to `stable-android`
- `master-ios` to `stable-ios`


## Differences between GHC 8.10.7 and GHC 9.6.2

1. The main difference is related to `DuplicateRecordFields` extension.

It is no longer possible in GHC 9.6.2 to specify type when using selectors, instead use OverloadedRecordDot extension and syntax:

```haskell
{-# LANGUAGE DuplicateRecordFields #-}
-- add this
{-# LANGUAGE OverloadedRecordDot #-}

-- and instead of
-- let x = field (record :: Record)

-- use
let x = record.field
```

It is still possible to specify type when using record update syntax, use this pragma to suppress compiler warning:

```haskell
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}

let r' = (record :: Record) {field = value}
```

2. Most monad functions now have to be imported from `Control.Monad`, and not from specific monad modules (e.g. `Control.Monad.Except`).

```haskell
import Control.Monad
```

[This PR](https://github.com/simplex-chat/simplex-chat/pull/2975/files) has all the differences.
