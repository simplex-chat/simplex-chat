---
title: SimpleX Chatでカスタム WebRTC ICE サーバーを使用する
revision: 31.01.2023
---

| 更新日 31.01.2023 | 言語: JA, [EN](/docs/WEBRTC.md), [FR](/docs/lang/fr/WEBRTC.md), [CZ](/docs/lang/cs/WEBRTC.md), [PL](/docs/lang/pl/WEBRTC.md), JA |

# SimpleX Chatでカスタム WebRTC ICE サーバーを使用する

## STUN/TURN サーバーのデプロイ

このガイドでは、最も機能が豊富で実証済みの STUN/TURN サーバー実装である [`coturn`](https://github.com/coturn/coturn) と [`Ubuntu 20.04 LTS`](https://ubuntu.com/download/server) Linux ディストリビューションを使用します。

0. `stun.$YOUR_DOMAIN` と `turn.$YOUR_DOMAIN` の証明書を取得します。

   ここでは [Let's Encrypt](https://letsencrypt.org/getting-started/) を使用しています。

1. メインリポジトリから `coturn` パッケージをインストールします。

```sh
apt update && apt install coturn`
```

2. `/etc/default/coturn` から `TURNSERVER_ENABLED=1` のコメントアウトを解除します：

```sh
sed -i '/TURN/s/^#//g' /etc/default/coturn
```

3. `/etc/turnserver.conf` で `coturn` を設定します：

   また、各オプションのコメントもご確認ください。

```sh
# tls用に443ポートでも待機
alt-tls-listening-port=443
# TURNメッセージでフィンガープリントを使用
fingerprint
# 長期認証メカニズムを使用
lt-cred-mech
# 認証情報
user=$YOUR_LOGIN:$YOUR_PASSWORD
# サーバードメイン
server-name=$YOUR_DOMAIN
# 明示的なorigin/realmの関係が見つからない場合にユーザーに使用されるデフォルトのrealm
realm=$YOUR_DOMAIN
# 証明書のパス。coturnプロセスのユーザー/グループが読み取れることを確認してください
cert=/var/lib/turn/cert.pem
pkey=/var/lib/turn/key.pem
# 2066ビットの事前定義済み DH TLS キーを使用
dh2066
# journalctlにログ出力
syslog
# coturnサービスを実行するユーザー/グループ
proc-user=turnserver
proc-group=turnserver
# 脆弱な暗号化を無効化
no-tlsv1
no-tlsv1_1
no-tlsv1_2
```

4. `coturn` サービスを開始して有効化します：

```sh
systemctl enable coturn && systemctl start coturn
```

5. オプションで、`ufw` ファイアウォールを使用している場合は、関連ポートを開放します：

- **3478** – "プレーン" TURN/STUN；
- **5349** – TURN/STUN over TLS；
- **443** – TURN/STUN over TLS、ファイアウォールをバイパス可能；
- **49152:65535** – Coturnがデフォルトで TURN リレーに使用するポート範囲。

```sh
# Ubuntu の場合
sudo ufw allow 3478 && \
sudo ufw allow 443 && \
sudo ufw allow 5349 && \
sudo ufw allow 49152:65535/tcp && \
sudo ufw allow 49152:65535/udp

# Fedora の場合
sudo firewall-cmd --permanent --add-port=443/tcp && \
sudo firewall-cmd --permanent --add-port=443/udp && \
sudo firewall-cmd --permanent --add-port=5349/tcp && \
sudo firewall-cmd --permanent --add-port=5349/udp && \
sudo firewall-cmd --permanent --add-port=49152:65535/tcp && \
sudo firewall-cmd --permanent --add-port=49152:65535/udp && \
sudo firewall-cmd --reload
```

## モバイルアプリの設定

モバイルアプリでサーバーを使用するように設定するには：

1. `設定 / ネットワークとサーバー / WebRTC ICE サーバー` を開き、`ICE サーバーを設定` のトグルをオンにします。

2. フィールドにすべてのサーバーアドレスを1行に1つずつ入力します。例えば、サーバーがポート5349の場合：

```
stun:stun.example.com:5349
turn:username:password@turn.example.com:5349
```

これで完了です。これで独自のサーバー経由で音声・ビデオ通話を行うことができ、（E2E暗号化メッセージでの連絡先とのキー交換以外は）弊社のサーバーとデータを共有することはありません。

## トラブルシューティング

- **サーバーが利用可能かどうかを確認**：

  ターミナルで次のコマンドを実行します：

  ```sh
  ping <your_ip_or_domain>
  ```

  パケットが送信されていれば、サーバーは稼働しています！

- **ポートが開いているかどうかを確認**：

  ターミナルで次のコマンドを実行します：

  ```sh
  nc -zvw10 <your_ip_or_domain> 443 5349
  ```

  以下のような表示が出るはずです：

  ```
  Connection to <your_ip_or_domain> 443 port [tcp/https] succeeded!
  Connection to <your_ip_or_domain> 5349 port [tcp/*] succeeded!
  ```

- **STUN/TURN 接続をテスト**：

  1. [IceTest](https://icetest.info/) にアクセスします。

  2. **Build up ICE Server List** セクションで以下を追加します：

     <img src="../../stun_1.png">

     - `STUN: stun:<your_ip_or_domain>:<port>` と入力して `Add STUN` をクリック
     - `TURN: turn:<your_ip_or_domain>:<port>`、`Username: <your_login>`、`Credential: <your_pass>` と入力して `Add TURN` をクリック

     ここで `<port>` は 443 または 5349 です。

  3. **ICE server list** セクションでサーバーが表示されるはずです。すべて正しく設定されていれば、`Start test` をクリックします：

     <img src="../../stun_2.png">

  4. **Results** セクションで以下のような結果が表示されるはずです：

     <img src="../../stun_3.png">

     結果に `srflx` と `relay` の候補が表示されていれば、すべて正しく設定されています！
