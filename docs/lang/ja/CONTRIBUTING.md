---
title: コントリビューティングガイド
revision: 31.01.2023
---

| Updated 31.01.2023 | Languages: [EN](/docs/CONTRIBUTING.md), [FR](/docs/lang/fr/CONTRIBUTING.md), [CZ](/docs/lang/cs/CONTRIBUTING.md) JA |

# コントリビューティングガイド

## SQLCipher暗号化を有効にしてコンパイルする

OpenSSLヘッダとライブラリの場所と暗号化モードを設定するフラグ`cabal.project.local` プロジェクトルートに追加します:

```
cp scripts/cabal.project.local.mac cabal.project.local
# or
# cp scripts/cabal.project.local.linux cabal.project.local
```

## MacでOpenSSLを使用する

MacOSにはデフォルトでLibreSSLがインストールされていますが、SimpleXをソースからコンパイルするにはOpenSSLをインストールする必要があります。

OpenSSLは`brew install openssl@1.1`でインストールできます。

正しく動作させるにはPATHに`/opt/homebrew/opt/openssl@1.1/bin`を追加する必要があります。


## プロジェクトブランチ

**simplex-chatリポジトリ**

- `stable` - アプリの安定版リリース。以前の安定リリース(GHC 9.6.3)へのアップデートに使用します。

- `stable-android` - Nix (GHC 8.10.7)で安定したAndroidコアライブラリを構築するために使用します。 - Android armv7a のみ。

- `stable-ios` - Nix (GHC 8.10.7) を使用して安定したiOSコアライブラリを構築するために使用します。このブランチはNix設定ファイルを除き、`stable-android`と同じである必要があります。 廃止済みです。

- `master` - ベータ版リリース(GHC 9.6.3)のブランチです。

- `master-ghc8107` - ベータ版リリース(GHC 8.10.7)用のブランチです。 廃止済みです。

- `master-android` - Nix (GHC 8.10.7)でベータAndroidコアライブラリを構築するために使用します。 - Android armv7a のみ。

- `master-ios` - Nix (GHC 8.10.7)でベータiOSコアライブラリを構築するために使用します。 廃止済みです。

- `windows-ghc8107` - Windowsコアライブラリビルド(GHC 8.10.7)用のブランチです。 廃止済み?

`master-ios`と`windows-ghc8107`ブランチは、Nix構成ファイルを除き、`master-ghc8107`と同じである必要があります。

**simplexmqリポジトリ**

- `master` - GHC 9.6.3 を使用しており、そのコミットはsimplex-chatリポジトリの`master`ブランチで使用する必要があります。

- `master-ghc8107` - このコミットは simplex-chat リポジトリの`master-android` (および`master-ios`)ブランチで使用する必要があります。廃止済みです。

## 開発とリリースプロセス

1. simplex-chatリポジトリとsimplexmqリポジトリのみに、`master`ブランチに PR を作成します。

2. simplexmqリポジトリが変更された場合、モバイル向けコアライブラリをビルドするには、その`master`ブランチを`master-ghc8107`ブランチにマージする必要があります。

3. Android、iOS、Windows用のコアライブラリを構築する場合:
- `master`ブランチを`master-android`ブランチにマージする。
- GHC 8.10.7と互換性のあるコードに更新する (以下を参照)。
- GitHubにプッシュする。

4. すべてのライブラリは`master`から、Android armv7aは`master-android`ブランチからビルドする必要があります。

5. デスクトップアプリとCLIアプリをビルドするには、`master`ブランチでタグを作成し、APKファイルをリリースに添付する必要があります。

6. App StoreとPlayストアへのリリース後にマージします:
- `master`から`stable`へ
- `master`から`master-android`(コードをコンパイル/更新する)
- `master-android`から`stable-android`へ

7. それとは別に、simplexmqリポジトリの`master`ブランチは、安定版リリース時に`stable` ブランチにマージする必要があります。


## GHC 8.10.7とGHC 9.6.3の違い

1. 主な違いは、`DuplicateRecordFields`拡張に関するものです。

GHC9.6.3ではセレクタを使うときに型を指定することができなくなり、代わりにOverloadedRecordDot拡張と構文が使われるようになりましたが、これはGHC8.10.7では削除する必要があります:

```haskell
{-# LANGUAGE DuplicateRecordFields #-}
-- use this in GHC 9.6.3 when needed
{-# LANGUAGE OverloadedRecordDot #-}

-- GHC 9.6.3 syntax
let x = record.field

-- GHC 8.10.7 syntax removed in GHC 9.6.3
let x = field (record :: Record)
```

レコード更新構文を使用する場合でも、型を指定することは可能です。コンパイラの警告を抑制するにはこのプラグマを使用します:

```haskell
-- use this in GHC 9.6.3 when needed
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}

let r' = (record :: Record) {field = value}
```

2. ほとんどのモナド関数は、特定のモナドモジュール(`Control.Monad.Except`など)ではなく、`Control.Monad`からインポートする必要があります。

```haskell
-- use this in GHC 9.6.3 when needed
import Control.Monad
```

[このPR](https://github.com/simplex-chat/simplex-chat/pull/2975/files)では全ての違いについて見ることができます。
