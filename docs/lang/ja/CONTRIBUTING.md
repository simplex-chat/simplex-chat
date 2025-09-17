---
title: Contributing guide
revision: 25.07.2025
---

| 更新日 25.07.2025 | 言語: JA, [EN](/docs/CONTRIBUTING.md), [FR](/docs/lang/fr/CONTRIBUTING.md), [CZ](/docs/lang/cs/CONTRIBUTING.md), [PL](/docs/lang/pl/CONTRIBUTING.md), JA |

# コントリビューションガイド

## SQLCipher暗号化を有効にしてコンパイルする

プロジェクトルートにOpenSSLヘッダーとライブラリの場所と暗号化モードを設定するフラグを記載した`cabal.project.local`を追加してください：

```
cp scripts/cabal.project.local.mac cabal.project.local
# または
# cp scripts/cabal.project.local.linux cabal.project.local
```

## MacOSでのOpenSSL

MacOSはデフォルトでLibreSSLが付属していますが、SimpleXをソースからコンパイルするにはOpenSSLをインストールする必要があります。

OpenSSLは`brew install openssl@3.0`でインストールできます

正常に動作させるためには、PATHに`/opt/homebrew/opt/openssl@3.0/bin`を追加する必要があります


## プロジェクトブランチ

**simplex-chatリポジトリにて**

- `stable` - アプリの安定版リリース、以前の安定版リリースの更新に使用可能（GHC 9.6.3）。

- `stable-android` - NixでAndroid安定版コアライブラリをビルドするために使用（GHC 8.10.7） - Android armv7aのみ。

- `master` - ベータバージョンリリースのブランチ（GHC 9.6.3と8.10.7の両方と互換性あり）。

- `master-android` - NixでAndroidベータ版コアライブラリをビルドするために使用（GHC 8.10.7） - Android armv7aのみ。

**simplexmqリポジトリにて**

- `master` - GHC 9.6.3と8.10.7の両方と互換性あり。

## 開発＆リリースプロセス

1. simplex-chatとsimplexmqリポジトリの両方について、`master`ブランチ_のみ_にPRを作成してください。

2. Android、iOS、windowsのコアライブラリをビルドするには：
- `master`ブランチを`master-android`ブランチにマージする。
- GitHubにプッシュする。

3. すべてのライブラリは`master`ブランチからビルドし、Android armv7aは`master-android`ブランチからビルドしてください。

4. DesktopとCLIアプリをビルドするには、`master`ブランチでタグを作成し、APKファイルをリリースに添付してください。

5. App StoreとPlay Storeへの公開リリース後、以下をマージしてください：
- `master`を`stable`に
- `master`を`master-android`に（コンパイル/更新を含む）
- `master-android`を`stable-android`に

6. 独立して、安定リリース時にはsimplexmqリポジトリの`master`ブランチを`stable`ブランチにマージしてください。

## ブランチとPR

PR名の最初の単語として変更スコープ（またはカンマ区切りのスコープ）を使用し、その後にコロンを付けてください。コミット名自体は小文字で現在時制にしてください。

simplex-chatリポジトリのPR名はリリースノートで使用されるため、変更ではなく解決された問題を記述する必要があります。可能なPRスコープ：
- ios
- android
- desktop
- core
- docs
- website
- ci

PRはスカッシュするため、レビュー後はブランチ履歴を書き換えないでください。

一部の複雑な機能については、準備が整ったらマージされる機能ブランチを作成します - それらに直接コミットせず、機能ブランチにPRを作成してください。

## GHC 8.10.7とGHC 9.6.3の違い

1. 主な違いは`DuplicateRecordFields`拡張に関連しています。

GHC 9.6.3では、セレクタを使用する際に型を指定することはできなくなりました。代わりにOverloadedRecordDot拡張と構文が使用されますが、これらはGHC 8.10.7では削除する必要があります：

```haskell
{-# LANGUAGE DuplicateRecordFields #-}
-- 必要に応じてGHC 9.6.3でこれを使用
{-# LANGUAGE OverloadedRecordDot #-}

-- GHC 9.6.3構文
let x = record.field

-- GHC 9.6.3で削除されたGHC 8.10.7構文
let x = field (record :: Record)
```

レコード更新構文を使用する際に型を指定することは依然として可能です。コンパイラ警告を抑制するには、このプラグマを使用してください：

```haskell
-- 必要に応じてGHC 9.6.3でこれを使用
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}

let r' = (record :: Record) {field = value}
```

2. ほとんどのmonad関数は、特定のmonadモジュール（例：`Control.Monad.Except`）からではなく、`Control.Monad`からインポートする必要があります。

```haskell
-- 必要に応じてGHC 9.6.3でこれを使用
import Control.Monad
```

[このPR](https://github.com/simplex-chat/simplex-chat/pull/2975/files)にすべての違いが記載されています。


## リモートデスクトップ接続のバージョン間の互換性向上

UIはすでにチャットとチャットアイテムのJSON変換失敗を処理でき、これはデバッグとダウングレードの両方に役立ちます。

リモート接続のバージョンを上げて異なるバージョンを非互換にすることは可能ですが、多くの場合ユーザーが異なるリリースサイクルのためモバイルアプリとデスクトップアプリを同時にアップグレードできないため、リモート接続のUXが低下します。

Android アプリユーザーにとって特に問題となります。新しいバージョンの上に古いバージョンをインストールできないため、Export/Import経由でのみダウングレードできます。

PR #6105で以下により改善されました：
- CInfoInvalidJSONコンストラクタを追加し、パースできないチャットがリモート接続経由で「無効なチャット」として表示される（UIがAPIに存在しないフィールドを持つ場合のように）
- CIContentのJSONパース処理を変更し、プラットフォーム固有のJSONパーサーでCIInvalidJSONにフォールバックする

リスト内の「無効な」チャットを避けるために、AChatタイプとサブタイプのJSONエンコーディングレベルでの前方互換性を維持する必要があります：
- これらのタイプに新しいフィールドをオプションとして追加
- 適切な場合、デフォルト値を提供するために新しいフィールドのタイプのFromJSONインスタンスに`omittedField`メソッドを追加
- プリミティブな非オプションフィールドをJSONインスタンスで`omittedField`を持つnewtypeとして定義

チャットアイテムでの無効なJSONへのフォールバックを避けるために、ChatItemタイプとサブタイプについても同様に行う必要があります。すべてのCIContentに使用されるタイプにフィールドを追加する際は特に重要です。そうしないと、すべてのアイテムが壊れてしまいます。
