---
title: Androidアプリ内のファイルにアクセスする
revision: 07.02.2023
---

| Updated 07.02.2023 | [EN](/docs/ANDROID.md), [CZ](/docs/lang/cs/ANDROID.md), [FR](/docs/lang/fr/ANDROID.md) JA |

# Androidアプリ内のファイルにアクセスする

SimpleXはデータベースを使用し、その設定をAndroidのプライベートデータディレクトリ内に保存します。ディレクトリには次のものが含まれます:
- データベース
- 送受信ファイル
- 不要になったら削除される一時ファイル
- ユーザー設定


SimpleXのデータディレクトリに保存されているものを表示するには以下の環境が必要です:
- Unixベースのオペレーティングシステム(またはWindowsの[MinGW](https://www.mingw-w64.org/downloads/))
- ADB (Android Debug Bridge)がインストールされたコンピュータ。([ここからダウンロード](https://developer.android.com/studio/releases/platform-tools)してインストールできます)
- デバイスがUSBまたはWi-Fiでコンピュータに接続されていること

## 手順:

- SimpleXを開き、`データベースのパスフレーズとエクスポート`を開き、`アプリのデータバックアップ`を有効にします。これで他のステップも動作するようになります
- オプション: データベースの内容を表示したい場合は、`データベースのパスフレーズ`をランダムなものから自分のものに変更してください。これを行うには、`データベースのパスフレーズとエクスポート`画面でチャットを停止し、データベースパスフレーズを開き、新しいパスフレーズを入力して確認し、更新します。このパスフレーズを忘れると、後で再度パスフレーズを聞かれたときに、すべてのデータを失うことになります
- ターミナルエミュレータを開き（WindowsのCMD/Powershellは使えません）、バックアップを保存するディレクトリを変更します:

```bash
cd /tmp  # 例
```
次に、以下を実行します:
```bash
adb -d backup -f chat.ab -noapk chat.simplex.app && 
tail -n +5 chat.ab > chat.dat && 
printf "\x1f\x8b\x08\x00\x00\x00\x00\x00" | cat - chat.dat > chat.gz && 
tar -xvzf chat.gz
```

デバイスのロックを解除し、暗号化のパスワードを使用せずにバックアップ操作を確認します。そうしないと、コマンドが機能しません。

その後、バックアップを終了する必要があります。`tar: Error is not recoverable: exiting now`というエラーが表示された場合でも、心配する必要なく、問題ありません。

これで、バックアップされたファイルは `./apps/chat.simplex.app/`内に配置されます。

SimpleXの最新バージョンを使用している場合、データベースは暗号化されるため、`sqlcipher`アプリケーションを使用し、復号化パスフレーズを知らない限り、データベースの内容を見ることができないことに注意してください（最初にアプリでランダムに生成されたパスフレーズから自分のものに変更する必要があります）。

## データベースの復号化

データベースデータを表示するには、まずデータを復号化する必要があります。 お気に入りのパッケージマネージャーを使用して`sqlcipher`をインストールし、データベースのあるディレクトリで次のコマンドを実行します:
```bash
sqlcipher files_chat.db
pragma key="youDecryptionPassphrase";
# 正常に動作するか確認
select * from users;
```

`Parse error: no such table: users`が表示された場合は、正しいパスフレーズを入力していること、Androidアプリでパスフレーズをランダムから変更していることを確認してください (もちろん、このデータベースを Android デバイスから取得した場合)。
