---
title: Terminal CLI
revision: 31.01.2023
---

| 更新日 31.01.2023 | 言語: JA, [EN](/docs/CLI.md), [FR](/docs/lang/fr/CLI.md), [CZ](/docs/lang/cs/CLI.md), [PL](/docs/lang/pl/CLI.md), JA |

# SimpleX Chat ターミナル（コンソール）アプリ for Linux/MacOS/Windows

## 目次

- [ターミナルチャット機能](#ターミナルチャット機能)
- [インストール](#🚀-インストール)
  - [チャットクライアントのダウンロード](#チャットクライアントのダウンロード)
    - [Linux と MacOS](#linux-と-macos)
    - [Windows](#windows)
  - [ソースからのビルド](#ソースからのビルド)
    - [Docker を使用](#docker-を使用)
    - [任意のOSでHaskellを使用](#任意のosでhaskellを使用)
- [使用方法](#使用方法)
  - [チャットクライアントの実行](#チャットクライアントの実行)
  - [Tor経由でメッセージングサーバーにアクセス](#tor経由でメッセージングサーバーにアクセスbeta)
  - [SimpleXチャットの使い方](#simplexチャットの使い方)
  - [グループ](#グループ)
  - [ファイルの送信](#ファイルの送信)
  - [ユーザー連絡先アドレス](#ユーザー連絡先アドレス)

## ターミナルチャット機能

- 同一ターミナルウィンドウで複数の人との1対1チャット。
- グループメッセージング。
- 連絡先およびグループへのファイル送信。
- ユーザー連絡先アドレス - 複数回使用可能な連絡先リンクを介して接続を確立。
- ローカルSQLiteデータベースにメッセージを永続化。
- 自動入力される受信者名 - 接続が確立されると、送信者への返信時にメッセージを入力するだけ。
- アプリに事前設定されたデモSMPサーバーが利用可能 - または[独自のサーバーをデプロイ](https://github.com/simplex-chat/simplexmq#using-smp-server-and-smp-agent)することも可能。
- サーバーに見える グローバルアイデンティティや名前は一切なく、連絡先と会話の完全なプライバシーを確保。
- 2層のE2E暗号化（双方向接続用のdouble-ratchet、一時的なCurve448キーを使用したX3DH鍵合意、およびCurve25519キーを使用したSMPキュー用のNaCl crypto_box）と受信者キーの帯域外受け渡し（[SimpleXチャットの使い方](#simplexチャットの使い方)を参照）。
- メッセージ整合性検証（前のメッセージのダイジェストを含めることによる）。
- 自動生成されたEd448キーによるSMPサーバーでの各コマンド/メッセージの認証。
- TLS 1.3転送暗号化。
- SMPサーバーから受信者への追加のメッセージ暗号化によりトラフィック相関を軽減。

鍵交換に関与する公開鍵はアイデンティティとして使用されず、各連絡先に対してランダムに生成されます。

技術的な詳細については、[使用されている暗号化プリミティブ](https://github.com/simplex-chat/simplexmq/blob/master/protocol/overview-tjr.md#encryption-primitives-used)を参照してください。

<a name="🚀-インストール"></a>

## 🚀 インストール

### チャットクライアントのダウンロード

#### Linux と MacOS

`simplex-chat` を**インストール**または**更新**するには、インストールスクリプトを実行する必要があります。これを行うには、以下のcURLまたはWgetコマンドを使用してください：

```sh
curl -o- https://raw.githubusercontent.com/simplex-chat/simplex-chat/stable/install.sh | bash
```

```sh
wget -qO- https://raw.githubusercontent.com/simplex-chat/simplex-chat/stable/install.sh | bash
```

チャットクライアントがダウンロードされたら、ターミナルで `simplex-chat` コマンドで実行できます。

または、[最新の安定版リリース](https://github.com/simplex-chat/simplex-chat/releases)からシステム用のチャットバイナリを手動でダウンロードし、以下のように実行可能にすることもできます。

```sh
chmod +x <binary>
mv <binary> ~/.local/bin/simplex-chat
```

（または`PATH`上の他の好ましい場所）。

MacOSでは、[Gatekeeperによる実行を許可](https://support.apple.com/en-us/HT202491)する必要もあります。

#### Windows

```sh
move <binary> %APPDATA%/local/bin/simplex-chat.exe
```

### ソースからのビルド

> **注意：** アプリをビルドするには[stable branch](https://github.com/simplex-chat/simplex-chat/tree/stable)のソースコードを使用してください。

#### Docker を使用

Linuxでは、[カスタム出力でのdocker build](https://docs.docker.com/engine/reference/commandline/build/#custom-build-outputs)を使用してチャット実行可能ファイルをビルドできます：

```shell
git clone git@github.com:simplex-chat/simplex-chat.git
cd simplex-chat
git checkout stable
DOCKER_BUILDKIT=1 docker build --output ~/.local/bin .
```

> **注意：** `` version `GLIBC_2.28' not found `` エラーが発生した場合は、`haskell:8.10.7-stretch` ベースイメージで再ビルドしてください（ローカルの[Dockerfile](/Dockerfile)で変更）。

#### 任意のOSでHaskellを使用

1. [Haskell GHCup](https://www.haskell.org/ghcup/)、GHC 9.6.3、cabal 3.10.1.0をインストール：

```shell
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

`ghcup tui` を使用してGHCとcabalのバージョンを確認または追加できます。

2. ソースコードをクローン：

```shell
git clone git@github.com:simplex-chat/simplex-chat.git
cd simplex-chat
git checkout stable
# または特定のバージョンをビルドするには：
# git checkout v5.3.0-beta.8
```

`master` は開発ブランチで、不安定なコードが含まれている場合があります。

3. システムの準備：

Linux の場合：

```shell
apt-get update && apt-get install -y build-essential libgmp3-dev zlib1g-dev
cp scripts/cabal.project.local.linux cabal.project.local
```

Mac の場合：

```
brew install openssl@3.0
cp scripts/cabal.project.local.mac cabal.project.local
```

実際のopensslの場所を指すようにcabal.project.localを修正する必要がある場合があります。

4. アプリのビルド：

```shell
cabal update
cabal install simplex-chat
```

## 使用方法

### チャットクライアントの実行

チャットクライアントを開始するには、ターミナルから `simplex-chat` を実行します。

デフォルトでは、アプリデータディレクトリはホームディレクトリ（`~/.simplex`、またはWindowsでは `%APPDATA%/simplex`）に作成され、その中に2つのSQLiteデータベースファイル `simplex_v1_chat.db` と `simplex_v1_agent.db` が初期化されます。

データベースファイルに異なるファイルパスプレフィックスを指定するには、`-d` コマンドラインオプションを使用します：

```shell
$ simplex-chat -d alice
```

例えば上記を実行すると、現在のディレクトリに `alice_v1_chat.db` と `alice_v1_agent.db` データベースファイルが作成されます。

3つのデフォルトSMPサーバーがLinode上でホストされており、これらは[アプリに事前設定](https://github.com/simplex-chat/simplex-chat/blob/stable/src/Simplex/Chat/Options.hs#L42)されています。

独自のSMPサーバーをデプロイした場合は、`-s` オプションでクライアントを設定できます：

```shell
$ simplex-chat -s smp://LcJUMfVhwD8yxjAiSaDzzGF3-kLG4Uh0Fl_ZIjrRwjI=@smp.example.com
```

サーバーアドレスの前のBase64urlエンコードされた文字列は、TLSハンドシェイク中にクライアントによって検証されるサーバーのオフライン証明書フィンガープリントです。

デフォルトまたは他のサーバーを使用している人とも会話できます - これは接続を開始する際のメッセージキューの場所にのみ影響し、返信キューは相手のクライアントによって設定された別のサーバー上にある場合があります。

利用可能なすべてのオプションを確認するには `simplex-chat -h` を実行してください。

### Tor経由でメッセージングサーバーにアクセス（Beta）

Torをインストールし、ポート9050でSOCKS5プロキシとして実行します。例えば、Macでは次のようにできます：

```
brew install tor
brew services start tor
```

Tor経由でサーバーにアクセスするには `-x` オプションを使用します：

```
simplex-chat -x
```

SOCKS5プロキシのホストとポートを設定するには、`--socks-proxy=ipv4:port` または `--socks-proxy=:port` オプションも使用できます。例えば、他のホストやポートで実行している場合です。

### SimpleXチャットの使い方

チャットを開始すると、ローカルチャットプロファイルを作成するために「表示名」と任意の「フルネーム」を指定するよう促されます。表示名は連絡先があなたを参照するためのエイリアスです - これは一意ではなく、グローバルアイデンティティとしては機能しません。一部の連絡先が同じ表示名を選択した場合、チャットクライアントはローカル表示名に数字のサフィックスを追加します。

以下の図は、連絡先に接続してメッセージを送信する方法を示しています：

<div align="center">
  <img align="center" src="../images/how-to-use-simplex.svg">
</div>

ローカルプロファイルを設定したら、`/c`（`/connect` の略）を入力して新しい接続を作成し、招待を生成します。この招待を他のチャネル経由で連絡先に送信します。

`/connect` を複数回入力することで複数の招待を作成でき、接続したい対応する連絡先にこれらの招待を送信できます。

招待は一度だけ使用でき、たとえこれが傍受されても、連絡先が接続の確立を確認した後、攻撃者はこのキューを使ってメッセージを送信することはできません。[招待形式](https://github.com/simplex-chat/simplexmq/blob/master/protocol/agent-protocol.md#connection-request)の説明については、エージェントプロトコルを参照してください。

招待を受け取った連絡先は、`/c <invitation>` を入力して接続を受け入れる必要があります。これで接続が確立され、両方の当事者に通知されます。

その後、`@<name> <message>` コマンドを使用してメッセージを送信します。最後の連絡先にメッセージを送信するために、単にメッセージの入力を開始することもできます。

利用可能なコマンドのリストを確認するには、チャットで `/help` を使用してください。

### グループ

グループを作成するには `/g <group>` を使用し、`/a <group> <name>` で連絡先を追加します。その後、`#<group> <message>` を入力してグループにメッセージを送信できます。他のコマンドについては `/help groups` を使用してください。

![simplex-chat](../images/groups.gif)

> **注意**: グループは任意のサーバーに保存されません。メッセージが送信されるメンバーのリストとしてアプリデータベースで維持されます。

### ファイルの送信

`/f @<contact> <file_path>` で連絡先にファイルを送信できます - 受信者は送信される前にそれを受け入れる必要があります。他のコマンドについては `/help files` を使用してください。

![simplex-chat](../images/files.gif)

`/f #<group> <file_path>` でグループにファイルを送信することもできます。

### ユーザー連絡先アドレス

一回限りの招待リンクの代替として、`/ad`（`/address` の略）で長期間のアドレスを作成できます。作成されたアドレスは任意のチャネル経由で共有でき、他のユーザーが `/c <user_contact_address>` でリンクとして使用して連絡先リクエストを作成できます。

`/ac <name>` および `/rc <name>` コマンドで受信リクエストを受け入れたり拒否したりできます。

ユーザーアドレスは、複数回使用可能な接続リンクという意味で「長期間」です - ユーザーによって削除されるまで使用でき、その場合もすべての確立された接続は引き続きアクティブのままです（アドレスを変更すると人々があなたにメッセージを送信できなくなるメールの仕組みとは異なります）。

他のコマンドについては `/help address` を使用してください。

![simplex-chat](../images/user-addresses.gif)
