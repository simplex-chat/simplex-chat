---
title: Androidアプリでのファイルアクセス
revision: 07.02.2023
---

| 07.02.2023 | JA, [EN](/docs/ANDROID.md), [CZ](/docs/lang/cs/ANDROID.md), [FR](/docs/lang/fr/ANDROID.md), JA, [PL](/docs/lang/pl/ANDROID.md) |

# Androidアプリでのファイルアクセス

SimpleXはデータベースを使用し、AndroidのプライベートデータディレクトリにPreferencesを保存しています。このディレクトリには以下が含まれます：
- データベース
- 送受信したファイル
- 不要になったときに削除される一時ファイル
- ユーザー設定

SimpleXデータディレクトリ内に保存されているものを表示したい場合は、以下が必要です：
- Unix系オペレーティングシステム（またはWindowsでは[MinGW](https://www.mingw-w64.org/downloads/)）
- コンピュータにインストールされたADB（Android Debug Bridge）ツール（[こちらからダウンロード](https://developer.android.com/studio/releases/platform-tools)してインストール）
- USBまたはWi-Fi経由でコンピュータに接続されたデバイス

## プロセス：

- SimpleXを開き、`データベースパスフレーズ＆エクスポート`に移動し、`アプリデータバックアップ`を有効にします。これにより、他のステップが機能するようになります
- _オプション_: データベースの内容を表示したい場合は、データベースのパスフレーズをランダムから自分のものに変更してください。これを行うには、`データベースパスフレーズ＆エクスポート`画面でチャットを停止し、`データベースパスフレーズ`を開いて新しいパスフレーズを入力して確認し、更新してください。後でパスフレーズが再度求められた場合にすべてのデータを失うことになるので、忘れないでください
- ターミナルエミュレータを開き（Windows CMD/Powershellは動作しません）、バックアップの保存に使用したいディレクトリに移動します：

```bash
cd /tmp  # 例です
```
次に以下を実行します：
```bash
adb -d backup -f chat.ab -noapk chat.simplex.app && 
tail -n +5 chat.ab > chat.dat && 
printf "\x1f\x8b\x08\x00\x00\x00\x00\x00" | cat - chat.dat > chat.gz && 
tar -xvzf chat.gz
```

デバイスのロックを解除し、暗号化にパスワードを使用せずにバックアップ操作を確認してください。そうしないとコマンドが動作しません。

その後、バックアップが終了するはずです。`tar: Error is not recoverable: exiting now`というエラーが表示されても、その前にいくつかのファイル名が出力されている場合は、心配する必要はありません。

これで、バックアップされたファイルが`./apps/chat.simplex.app/`内に保存されます。

SimpleXの最新バージョンを使用している場合、データベースは暗号化されており、`sqlcipher`アプリケーションを使用し、復号化パスフレーズを知らずには内容を表示できないことにご注意ください（最初にアプリでランダムに生成されたものから自分のものに変更する必要があります）。

## データベースの復号化

データベースデータを表示するには、最初にそれを復号化する必要があります。お気に入りのパッケージマネージャーを使用して`sqlcipher`をインストールし、データベースがあるディレクトリで以下のコマンドを実行してください：
```bash
sqlcipher files_chat.db
pragma key="youDecryptionPassphrase";
# 正常に動作することを確認
select * from users;
```

`Parse error: no such table: users`というエラーが表示される場合は、正しいパスフレーズを入力していること、およびAndroidアプリでパスフレーズをランダムなものから変更していること（もちろん、このデータベースをAndroidデバイスから取得した場合）を確認してください。
