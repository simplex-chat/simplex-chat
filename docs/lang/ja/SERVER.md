---
title: 独自のSMPサーバーをホスティングする
revision: 12.10.2024
---

# 独自のSMPサーバーをホスティングする

| 更新日：2024年12月10日 | 言語: JA, [EN](/docs/SERVER.md), [FR](/docs/lang/fr/SERVER.md), [CZ](/docs/lang/cs/SERVER.md), [PL](/docs/lang/pl/SERVER.md), JA |

## 目次

- [概要](#概要)
- [クイックスタート](#クイックスタート) with systemd service
- [インストールオプション](#インストールオプション)
   - [systemdサービス](#systemdサービス) with [インストールスクリプト](#インストールスクリプト) または [手動](#手動デプロイ)
   - [dockerコンテナ](#dockerコンテナ)
   - [Linode marketplace](#linode-marketplace)
- [サーバーバイナリの検証](#サーバーバイナリの検証)
- [設定](#設定)
   - [対話的に](#対話的に)
   - [コマンドラインオプションで](#コマンドラインオプションで)
- [追加設定](#追加設定)
- [サーバーセキュリティ](#サーバーセキュリティ)
   - [初期化](#初期化)
   - [秘密鍵](#秘密鍵)
   - [オンライン証明書のローテーション](#オンライン証明書のローテーション)
- [Tor: インストールと設定](#tor-インストールと設定)
   - [onionアドレスのためのインストール](#onionアドレスのためのインストール)
   - [SMP PROXYのためのSOCKSポート](#smp-proxyのためのsocksポート)
- [サーバー情報ページ](#サーバー情報ページ)
- [ドキュメント](#ドキュメント)
   - [SMPサーバーアドレス](#smpサーバーアドレス)
   - [Systemdコマンド](#systemdコマンド)
   - [制御ポート](#制御ポート)
   - [日次統計](#日次統計)
- [ビルドの再現](#ビルドの再現)
- [SMPサーバーの更新](#smpサーバーの更新)
- [サーバーを使用するためのアプリ設定](#サーバーを使用するためのアプリ設定)

## 概要

SMPサーバーは、SimpleXネットワークでメッセージを転送するためのリレーサーバーです。SimpleX Chatアプリにはプリセットされたサーバー（モバイルアプリではsmp11、smp12、smp14.simplex.im）がありますが、他のサーバーを使用するようにアプリの設定を簡単に変更できます。

SimpleXクライアントは、連絡先ごと（またはグループメンバーとのグループ接続ごと）に個別に、メッセージを受信するためのサーバーのみを決定します。これらのサーバーは一時的なものであり、配信アドレスは変更可能です。

SMPサーバーを作成するには、以下が必要です：

1. VPSまたはその他のサーバー。
2. サーバーに向けられた独自のドメイン（`smp.example.com`）
3. 基本的なLinuxの知識。

_注意_：アプリの設定でサーバーを変更した場合、新しい連絡先に対してのみ使用されるサーバーに影響し、既存の連絡先は自動的に新しいサーバーに移動しませんが、連絡先/メンバー情報ページの["受信アドレスの変更"](../blog/20221108-simplex-chat-v4.2-security-audit-new-website.md#change-your-delivery-address-beta)ボタンを使用して手動で移動できます。これは将来自動化される予定です。

## クイックスタート

systemdサービスとしてSMPサーバーを作成するには、以下が必要です：

- VPSまたはその他のサーバー。
- サーバーのIPv4およびIPv6アドレスを指定するAおよびAAAAレコードを持つサーバードメイン（`smp1.example.com`）
- 基本的なLinuxの知識。

*注意*：ドメイン名なしでSMPサーバーを実行することは可能ですが、近い将来、クライアントアプリケーションは招待リンクでサーバードメイン名を使用するようになります（現在使用している`simplex.chat`ドメインの代わりに）。サーバーにドメイン名とサーバーページ（後述）がない場合、クライアントはブラウザーで開くことができない`simplex:`スキームのリンクを生成します。

1. [インストールスクリプト](https://github.com/simplex-chat/simplexmq#using-installation-script)を使用してサーバーをインストールします。

2. ファイアウォールを調整します：

   ```sh
   ufw allow 80/tcp &&\
   ufw allow 443/tcp &&\
   ufw allow 5223/tcp
   ```

3. サーバーを初期化します：

   `smp1.example.com`を実際のサーバードメインに置き換えてください。

   ```sh
   su smp -c 'smp-server init --yes \
                           --store-log \
                           --no-password \
                           --control-port \
                           --socks-proxy \
                           --source-code \
                           --fqdn=smp1.example.com
   ```

4. torをインストールします：

   ```sh
   CODENAME="$(lsb_release -c | awk '{print $2}')"

   echo "deb [signed-by=/usr/share/keyrings/tor-archive-keyring.gpg] https://deb.torproject.org/torproject.org ${CODENAME} main
   deb-src [signed-by=/usr/share/keyrings/tor-archive-keyring.gpg] https://deb.torproject.org/torproject.org ${CODENAME} main" > /etc/apt/sources.list.d/tor.list &&\
   curl --proto '=https' --tlsv1.2 -sSf https://deb.torproject.org/torproject.org/A3C4F0F979CAA22CDBA8F512EE8CBC9E886DDD89.asc | gpg --dearmor | tee /usr/share/keyrings/tor-archive-keyring.gpg >/dev/null &&\
   apt update && apt install -y tor deb.torproject.org-keyring
   ```

5. torを設定します：

   ```sh
   tor-instance-create tor2 &&\
   mkdir /var/lib/tor/simplex-smp/ &&\
   chown debian-tor:debian-tor /var/lib/tor/simplex-smp/ &&\
   chmod 700 /var/lib/tor/simplex-smp/
   ```

   ```sh
   vim /etc/tor/torrc
   ```

   以下を貼り付けてください：

   ```sh
   # ログを有効にする（そうでなければ、torはonionアドレスをデプロイしないようです）
   Log notice file /var/log/tor/notices.log
   # シングルホップルーティングを有効にする（以下の2つのオプションは3つ目の依存関係です） - レイテンシーを減らしますが、サーバーの匿名性が低下します - SMP-serverのonionアドレスがクライアントでパブリックアドレスと一緒に使用されるので、これは問題ありません。onion専用アドレスでSMP-serverをデプロイする場合は、標準設定を維持してください。
   SOCKSPort 0
   HiddenServiceNonAnonymousMode 1
   HiddenServiceSingleHopMode 1
   # smp-server隠しサービスホストディレクトリとポートマッピング
   HiddenServiceDir /var/lib/tor/simplex-smp/
   HiddenServicePort 5223 localhost:5223
   HiddenServicePort 443 localhost:443
   ```

   ```sh
   vim /etc/tor/instances/tor2/torrc
   ```

   以下を貼り付けてください：

   ```sh
   # torをsystemdデーモンにログ
   Log notice syslog
   # socksプロキシのためにローカル9050ポートをリッスン
   SocksPort 9050
   ```

6. torを開始します：

   ```sh
   systemctl enable tor &&\
   systemctl start tor &&\
   systemctl restart tor &&\
   systemctl enable --now tor@tor2
   ```

7. Caddyをインストールします：

   ```sh
   sudo apt install -y debian-keyring debian-archive-keyring apt-transport-https curl &&\
   curl -1sLf 'https://dl.cloudsmith.io/public/caddy/stable/gpg.key' | sudo gpg --dearmor -o /usr/share/keyrings/caddy-stable-archive-keyring.gpg &&\
   curl -1sLf 'https://dl.cloudsmith.io/public/caddy/stable/debian.deb.txt' | sudo tee /etc/apt/sources.list.d/caddy-stable.list &&\
   sudo apt update && sudo apt install caddy
   ```

8. Caddyを設定します：

   ```sh
   vim /etc/caddy/Caddyfile
   ```

   `smp1.example.com`を実際のサーバードメインに置き換えてください。以下を貼り付けてください：

   ```
   http://smp1.example.com {
      redir https://smp1.example.com{uri} permanent
   }

   smp1.example.com:8443 {
      tls {
         key_type rsa4096
      }
   }
   ```

   ```sh
   vim /usr/local/bin/simplex-servers-certs
   ```

   `smp1.example.com`を実際のサーバードメインに置き換えてください。以下を貼り付けてください：

   ```sh
   #!/usr/bin/env sh
   set -eu

   user='smp'
   group="$user"

   domain='smp1.example.com'
   folder_in="/var/lib/caddy/.local/share/caddy/certificates/acme-v02.api.letsencrypt.org-directory/${domain}"
   folder_out='/etc/opt/simplex'
   key_name='web.key'
   cert_name='web.crt'

   # Caddyディレクトリからsmpサーバーディレクトリに証明書をコピー
   cp "${folder_in}/${domain}.crt" "${folder_out}/${cert_name}"
   # 正しい権限を割り当て
   chown "$user":"$group" "${folder_out}/${cert_name}"

   # Caddyディレクトリからsmpサーバーディレクトリに証明書キーをコピー
   cp "${folder_in}/${domain}.key" "${folder_out}/${key_name}"
   # 正しい権限を割り当て
   chown "$user":"$group" "${folder_out}/${key_name}"
   ```

   ```sh
   chmod +x /usr/local/bin/simplex-servers-certs
   ```

   ```sh
   sudo crontab -e
   ```

   以下を貼り付けてください：

   ```sh
   # 毎週日曜日の00:20
   20 0 * * 0 /usr/local/bin/simplex-servers-certs
   ```

9. Caddyサービスを有効化して開始します：

   "good to go"が表示されるまで待ちます。

   ```sh
   systemctl enable --now caddy &&\
   sleep 10 &&\
   /usr/local/bin/simplex-servers-certs &&\
   echo 'good to go'
   ```

10. smp-serverを有効化して開始します：

    ```sh
    systemctl enable --now smp-server.service
    ```

11. アドレスを表示します：

    ```sh
    smp="$(journalctl --output cat -q _SYSTEMD_INVOCATION_ID="$(systemctl show -p InvocationID --value smp-server)" | grep -m1 'Server address:' | awk '{print $NF}' | sed 's/:443.*//')"
    tor="$(cat /var/lib/tor/simplex-smp/hostname)"

    echo "$smp,$tor"
    ```

## インストールオプション

SMPサーバーは以下のいずれかの方法でインストールできます：

- [systemdサービス](#systemdサービス)
   - [インストールスクリプト](#インストールスクリプト)を使用 - **推奨**
   - または[手動](#手動デプロイ)
- DockerHubからの[Dockerコンテナ](#dockerコンテナ)
- [Linode marketplace](#linode-marketplace)

### systemdサービス

#### インストールスクリプト

このインストールスクリプトは、バイナリ、systemdサービス、およびバックアップ、更新、アンインストールを管理する追加スクリプトを自動的にインストールします。柔軟性、簡単な更新、および実際のサーバーでのテスト済みであることから、これが推奨オプションです。

**注意**：現在、Ubuntuディストリビューションのみがサポートされています。

サーバー上で以下のスクリプトを実行してください：

```sh
curl --proto '=https' --tlsv1.2 -sSf https://raw.githubusercontent.com/simplex-chat/simplexmq/stable/install.sh -o simplex-server-install.sh &&\
if echo '53fcdb4ceab324316e2c4cda7e84dbbb344f32550a65975a7895425e5a1be757 simplex-server-install.sh' | sha256sum -c; then
  chmod +x ./simplex-server-install.sh
  ./simplex-server-install.sh
  rm ./simplex-server-install.sh
else
  echo "SHA-256チェックサムが正しくありません！"
  rm ./simplex-server-install.sh
fi
```

`1`を入力してEnterを押し、`smp-server`をインストールします。

#### 手動デプロイ

手動インストールは、最も柔軟性を提供する最も高度なデプロイメントです。通常、上級ユーザーにのみ推奨されます。

1. バイナリをインストール：

   - プリコンパイル済みバイナリを使用：

     ```sh
     curl -L https://github.com/simplex-chat/simplexmq/releases/latest/download/smp-server-ubuntu-20_04-x86-64 -o /usr/local/bin/smp-server && chmod +x /usr/local/bin/smp-server
     ```

   - ソースからコンパイル：

     [ソースからビルド：ディストリビューションを使用](https://github.com/simplex-chat/simplexmq#using-your-distribution)を参照してください

2. `smp-server`のユーザーとグループを作成：

   ```sh
   sudo useradd -m smp
   ```

3. 必要なディレクトリを作成し、権限を割り当て：

   ```sh
   sudo mkdir -p /var/opt/simplex /etc/opt/simplex
   sudo chown smp:smp /var/opt/simplex /etc/opt/simplex
   ```

4. ファイアウォールで`smp-server`ポートを許可：

   ```sh
   # Ubuntu用
   sudo ufw allow 5223/tcp
   sudo ufw allow 443/tcp
   sudo ufw allow 80/tcp
   # Fedora用
   sudo firewall-cmd --permanent --add-port=5223/tcp --add-port=443/tcp --add-port=80/tcp && \
   sudo firewall-cmd --reload
   ```

5. **オプション** — `systemd`を使用するディストリビューションの場合、以下の内容で`/etc/systemd/system/smp-server.service`ファイルを作成：

   ```sh
   [Unit]
   Description=SMP server systemd service

   [Service]
   User=smp
   Group=smp
   Type=simple
   ExecStart=/usr/local/bin/smp-server start +RTS -N -RTS
   ExecStopPost=/usr/bin/env sh -c '[ -e "/var/opt/simplex/smp-server-store.log" ] && cp "/var/opt/simplex/smp-server-store.log" "/var/opt/simplex/smp-server-store.log.bak"'
   LimitNOFILE=65535
   KillSignal=SIGINT
   TimeoutStopSec=infinity
   AmbientCapabilities=CAP_NET_BIND_SERVICE

   [Install]
   WantedBy=multi-user.target
   ```

   そして`sudo systemctl daemon-reload`を実行します。

### Dockerコンテナ

Docker Composeを使用してsmp-serverをデプロイできます。これは人気があり、比較的簡単にデプロイできるため、2番目の推奨オプションです。

このデプロイメントは2つのDocker Composeファイルを提供します：**自動**と**手動**です。確信が持てない場合は、**自動**を選択してください。

これにより、[Docker Hub](https://hub.docker.com/r/simplexchat)からイメージがダウンロードされます。

#### Docker: 自動設定

この設定は、SMPサーバーを設定するための迅速で簡単な方法を提供します：CaddyがLet's Encrypt証明書を自動管理し、HTTPをHTTPSにリダイレクトし、smp-serverが[サーバー情報ページ](#サーバー情報ページ)とSMPプロトコルの両方を443ポートで提供します。5223ポートはフォールバックとして使用されます。

**注意**：`80`と`443`ポートが他のサーバーによって使用されていない必要があります。

1. `smp-server`ディレクトリを作成し、そこに移動：

  ```sh
  mkdir smp-server && cd smp-server
  ```

2. 以下の内容で`docker-compose.yml`ファイルを作成：

  ここから取得することもできます - [docker-compose-smp-complete.yml](https://raw.githubusercontent.com/simplex-chat/simplexmq/refs/heads/stable/scripts/docker/docker-compose-smp-complete.yml)。`docker-compose.yml`にリネームすることを忘れないでください。

  ```yaml
  name: SimpleX Chat - smp-server

  services:
    oneshot:
      image: ubuntu:latest
      environment:
        CADDYCONF: |
          ${CADDY_OPTS:-}

          http://{$$ADDR} {
              redir https://{$$ADDR}{uri} permanent
          }

          {$$ADDR}:8443 {
              tls {
                  key_type rsa4096
              }
          }
      command: sh -c 'if [ ! -f /etc/caddy/Caddyfile ]; then printf "$${CADDYCONF}" > /etc/caddy/Caddyfile; fi'
      volumes:
        - ./caddy_conf:/etc/caddy

    caddy:
      image: caddy:latest
      depends_on:
        oneshot:
          condition: service_completed_successfully
      cap_add:
        - NET_ADMIN
      environment:
        ADDR: ${ADDR?"ドメインを指定してください。"}
      volumes:
        - ./caddy_conf:/etc/caddy
        - caddy_data:/data
        - caddy_config:/config
      ports:
        - 80:80
      restart: unless-stopped
      healthcheck:
        test: "test -d /data/caddy/certificates/${CERT_PATH:-acme-v02.api.letsencrypt.org-directory}/${ADDR} || exit 1"
        interval: 1s
        retries: 60

    smp-server:
      image: ${SIMPLEX_IMAGE:-simplexchat/smp-server:latest}
      depends_on:
        caddy:
          condition: service_healthy
      environment:
        ADDR: ${ADDR?"ドメインを指定してください。"}
        PASS: ${PASS:-}
      volumes:
        - ./smp_configs:/etc/opt/simplex
        - ./smp_state:/var/opt/simplex
        - type: volume
          source: caddy_data
          target: /certificates
          volume:
            subpath: "caddy/certificates/${CERT_PATH:-acme-v02.api.letsencrypt.org-directory}/${ADDR}"
      ports:
        - 443:443
        - 5223:5223
      restart: unless-stopped

  volumes:
    caddy_data:
    caddy_config:
  ```

3. 同じディレクトリに、以下の内容で`.env`ファイルを作成：

  ここから取得することもできます - [docker-compose-smp-complete.env](https://raw.githubusercontent.com/simplex-chat/simplexmq/refs/heads/stable/scripts/docker/docker-compose-smp-complete.env)。`.env`にリネームすることを忘れないでください。

  設定に応じて変数を変更してください。

  ```env
  # 必須
  ADDR=your_ip_or_addr

  # オプション
  #PASS='123123'
  ```

4. コンテナを起動：

  ```sh
  docker compose up
  ```

#### Docker: 手動設定

何をしているかわかっている場合、この設定は443ポートでの[サーバー情報ページ](#サーバー情報ページ)を提供するCaddyによって自動管理されるLet's Encrypt証明書なしでベアなSMPサーバー設定を提供し、5223ポートがプライマリとして設定されます。

この設定により、80および443ポートを自分で管理する能力を保持できます。デメリットとして、SMPサーバーを443ポートで提供することはできません。

1. `smp-server`ディレクトリを作成し、そこに移動：

  ```sh
  mkdir smp-server && cd smp-server
  ```

2. 以下の内容で`docker-compose.yml`ファイルを作成：

  ここから取得することもできます - [docker-compose-smp-manual.yml](https://raw.githubusercontent.com/simplex-chat/simplexmq/refs/heads/stable/scripts/docker/docker-compose-smp-manual.yml)。`docker-compose.yml`にリネームすることを忘れないでください。

  ```yaml
  name: SimpleX Chat - smp-server

  services:
    smp-server:
      image: ${SIMPLEX_IMAGE:-simplexchat/smp-server:latest}
      environment:
        WEB_MANUAL: ${WEB_MANUAL:-1}
        ADDR: ${ADDR?"ドメインを指定してください。"}
        PASS: ${PASS:-}
      volumes:
        - ./smp_configs:/etc/opt/simplex
        - ./smp_state:/var/opt/simplex
      ports:
        - 5223:5223
      restart: unless-stopped
  ```

3. 同じディレクトリに、以下の内容で`.env`ファイルを作成：

  ここから取得することもできます - [docker-compose-smp-manual.env](https://raw.githubusercontent.com/simplex-chat/simplexmq/refs/heads/stable/scripts/docker/docker-compose-smp-manual.env)。`.env`にリネームすることを忘れないでください。

  設定に応じて変数を変更してください。

  ```env
  # 必須
  ADDR=your_ip_or_addr

  # オプション
  #PASS='123123'
  WEB_MANUAL=1
  ```

4. コンテナを起動：

  ```sh
  docker compose up
  ```

### Linode marketplace

新しいLinode VMを作成する際にsmp-serverをデプロイできます。参考：[Linode Marketplace](https://www.linode.com/marketplace/apps/simplex-chat/simplex-chat/)

## サーバーバイナリの検証

v6.3以降、サーバービルドは[再現可能](#ビルドの再現)です。

これにより、GitHubビルドの整合性を確認するサーバーリリースに署名することも可能になります。

ダウンロードしたサーバーバイナリを検証するには：

1. `_sha256sums`（すべてのサーバーバイナリのハッシュ）と`_sha256sums.asc`（署名）をダウンロードします。

2. [openpgp.org](https://keys.openpgp.org/search?q=chat%40simplex.chat)から私たちのキーFB44AF81A45BDE327319797C85107E357D4A17FCをダウンロードします

3. `gpg --import FB44AF81A45BDE327319797C85107E357D4A17FC`でキーをインポートします。キーファイル名はフィンガープリントと同じでなければなりませんが、必要に応じて変更してください。

4. `gpg --verify _sha256sums.asc _sha256sums`を実行します。以下が表示されるはずです：

> Good signature from "SimpleX Chat <chat@simplex.chat>"

5. 使用予定のバイナリのハッシュを`sha256sum <file>`または`openssl sha256 <file>`で計算し、`_sha256sums`ファイルのハッシュと比較します。同じである必要があります。

これで、GitHubサーバーバイナリの真正性を確認できました。

## 設定

利用可能なオプションを確認するには、フラグなしで`smp-server`を実行します：

```sh
sudo su smp -c smp-server

...
利用可能なコマンド:
  init                     サーバーを初期化 - /etc/opt/simplexと
                           /var/opt/simplexディレクトリおよび設定ファイルを作成
  start                    サーバーを開始（設定:
                           /etc/opt/simplex/smp-server.ini)
  delete                   設定とログファイルを削除
```

さらにヘルプを表示するには、`sudo su smp -c "smp-server <command> -h"`を実行できます

その後、`smp-server`を設定する必要があります：

### 対話的に

以下のコマンドを実行してください：

```sh
sudo su smp -c "smp-server init"
```

検討すべき複数のオプションがあります：

- `Enable store log to restore queues and messages on server restart (Yn):`

  サーバーが再起動されたときに接続とメッセージを保存および復元するには、`y`を入力します。

  _注意_：サーバーを再起動する際はSIGINTを使用することが重要です。そうでなければ、未配信メッセージが復元されません。接続は、メッセージとは異なり、変更のたびに追記専用ログに追加されるため、サーバーがどのように再起動されても復元されます。

- `Enable logging daily statistics (yN):`

  CSV形式で統計をログに記録するには`y`を入力します。これらは`Grafana`で集計使用量チャートを表示するために使用できます。

これらの統計には、作成、確保、削除されたキューの日次カウント、送信および受信メッセージ、およびアクティブキュー（つまり、メッセージに使用されたキュー）の日次、週次、月次カウントが含まれます。この情報には、異なるキューを同じユーザーのものとして関連付けることを可能にするものは含まれていないと考えていますが、これが何らかの形で悪用される可能性があると思われる場合は、機密扱いで[お知らせください](../../SECURITY.md)。

- `Require a password to create new messaging queues?`

  `smp-server`をパスワードで保護するには、Enterを押すか任意のパスワードを入力し、パスワード保護を無効にするには`n`を入力します。

- `Enter server FQDN or IP address for certificate (127.0.0.1):`

  smp-serverが動作しているドメインまたはIPアドレスを入力してください。これはサーバー証明書に含まれ、サーバーアドレスの一部としても表示されます。

### コマンドラインオプションで

以下のコマンドを実行してください：

```sh
sudo su smp -c "smp-server init -h"

...
利用可能なオプション:
  -l,--store-log           永続化のためストアログを有効にする
  -s,--daily-stats         日次サーバー統計のログを有効にする
  -a,--sign-algorithm ALG  TLS証明書に使用される署名アルゴリズム:
                           ED25519, ED448 (デフォルト: ED448)
  --ip IP                  サーバーIPアドレス、FQDNが提供されない場合の
                           TLSオンライン証明書の Common Name として使用
                           (デフォルト: "127.0.0.1")
  -n,--fqdn FQDN           TLSオンライン証明書の Common Name として使用される
                           サーバーFQDN
  --no-password            パスワードなしでの新しいキュー作成を許可
  --password PASSWORD      新しいメッセージキューを作成するためのパスワードを設定
  -y,--yes                 コマンドラインオプションを使用した非対話的初期化
  -h,--help                このヘルプテキストを表示
```

使用例に必要なフラグを決定し、非対話的初期化のために`-y`フラグを付けて`smp-server init`を実行してください：

```sh
sudo su smp -c "smp-server init -y -<your flag> <your option>"
```

例えば、以下を実行してください：

```sh
sudo su smp -c "smp-server init -y -l --ip 192.168.1.5 --password test"
```

これにより、以下の設定で`smp-server`設定を初期化します：

- サーバーが再起動されたときの接続とメッセージの復元（`-l`フラグ）、
- IPアドレス`192.168.1.5`、
- パスワード`test`で`smp-server`を保護。

---

その後、インストールが完了し、端末出力に以下のようなものが表示されるはずです：

```sh
Certificate request self-signature ok
subject=CN = 127.0.0.1
サーバーが初期化されました。/etc/opt/simplex/smp-server.iniで設定を変更できます。
サーバーを開始するには `smp-server start` を実行してください。
----------
CA秘密鍵を安全に保存し、サーバーから削除してください。
サーバーTLS認証情報が侵害された場合、このキーを使用して新しいものに署名でき、同じサーバーIDと確立された接続を維持できます。
CA秘密鍵の場所: /etc/opt/simplex/ca.key
----------
SMP server v3.4.0
フィンガープリント: d5fcsc7hhtPpexYUbI2XPxDbyU2d3WsVmROimcL90ss=
サーバーアドレス: smp://d5fcsc7hhtPpexYUbI2XPxDbyU2d3WsVmROimcL90ss=:V8ONoJ6ICwnrZnTC_QuSHfCEYq53uLaJKQ_oIC6-ve8=@<hostnames>
```

上記のサーバーアドレスはクライアント設定で使用する必要があり、サーバーパスワードを追加した場合は、サーバーを使用してメッセージを受信することを許可したい他の人とのみ共有してください（すべての連絡先がメッセージを送信できます - パスワードは不要です）。初期化中にIPアドレスまたはホスト名を渡した場合、それらはサーバーアドレスの一部として表示されます。そうでなければ、`<hostnames>`を実際のサーバーホスト名に置き換えてください。

## 追加設定

生成されたすべての設定と各パラメーターの説明は、追加カスタマイゼーションのために設定ファイル`/etc/opt/simplex/smp-server.ini`内で利用できます。smp-serverバージョンによって、設定ファイルは以下のようになります：

```ini
[INFORMATION]
# AGPLv3ライセンスでは、サーバーのソースコードを変更した場合、
# エンドユーザーが利用できるようにすることが求められます。
# ライセンス: https://github.com/simplex-chat/simplexmq/blob/stable/LICENSE
# サーバーのソースコードが何らかの形で変更された場合は、正しいソースコードURIを含めてください。
# その他の情報フィールドが存在する場合、source_codeプロパティも必須です。

source_code: https://github.com/simplex-chat/simplexmq

# 以下のすべての情報の宣言は任意で、これらのフィールドのいずれも省略できます。

# サーバー使用条件と修正。
# 標準条件を別の文書の修正と一緒に使用することを推奨します。
# usage_conditions: https://github.com/simplex-chat/simplex-chat/blob/stable/PRIVACY.md
# condition_amendments: link

# サーバーの場所と運営者。
# server_country: ISO-3166 2文字コード
# operator: エンティティ（組織または個人名）
# operator_country: ISO-3166 2文字コード
# website:

# 管理連絡先。
# admin_simplex: SimpleXアドレス
# admin_email:
# admin_pgp:
# admin_pgp_fingerprint:

# 苦情とフィードバックの連絡先。
# complaints_simplex: SimpleXアドレス
# complaints_email:
# complaints_pgp:
# complaints_pgp_fingerprint:

# ホスティングプロバイダー。
# hosting: エンティティ（組織または個人名）
# hosting_country: ISO-3166 2文字コード

[STORE_LOG]
# サーバーは永続化のためにSTMメモリを使用します。
# これは再起動時に失われます（redisと同様）。
# このオプションは、メモリを追記専用ログに保存し、
# サーバー開始時に復元することを可能にします。
# ログは開始時に圧縮されます（削除されたオブジェクトが除去されます）。
enable: on

# 未配信メッセージは、サーバーの再起動時に任意で保存・復元され、
# 次の再起動まで.bakファイルに保持されます。
restore_messages: on
expire_messages_days: 21
expire_ntfs_hours: 24

# 日次サーバー統計をCSVファイルにログ
log_stats: on

[AUTH]
# new_queuesオプションをoffに設定すると、新しいメッセージキューの作成を完全に禁止します。
# これは、サーバーを廃止したいが、すべての接続がまだ切り替わっていない場合に有用です。
new_queues: on

# create_passwordオプションを使用して、新しいメッセージキューを作成するための基本認証を有効にします。
# パスワードは、クライアント設定のサーバーアドレスの一部として使用する必要があります：
# smp://fingerprint:password@host1,host2
# パスワードは接続する連絡先と共有されません。サーバー上でメッセージキューを作成することを
# 許可したいユーザーとのみ共有する必要があります。
# create_password: 新しいキューを作成するためのパスワード（空白なし、'@'、':'、'/'なしの印刷可能ASCII文字）

# control_port_admin_password:
# control_port_user_password:

[TRANSPORT]
# ホストは開始時にサーバーアドレスを表示するためのみ使用されます。
# 複数のサーバーポートを指定できます。
host: <domain/ip>
port: 5223,443
log_tls_errors: off

# `websockets: 443`を使用して、プレーンTLSに加えてwebsocketsサーバーを実行します。
websockets: off
# control_port: 5224

[PROXY]
# SMPプロキシクライアントのネットワーク設定。
# `host_mode`は'public'（デフォルト）または'onion'にできます。
# これは複数のホスト名を持つ宛先サーバーの優先ホスト名を定義します。
# host_mode: public
# required_host_mode: off

# 運営するリレーのドメインサフィックス（スペース区切り）を別のプロキシ統計として数える。
# own_server_domains: 

# 宛先サーバーにメッセージを転送するためのSOCKSプロキシポート。
# 受信シングルホップリクエスト用に別のSOCKSプロキシインスタンスが必要な場合があります。
# socks_proxy: localhost:9050

# `socks_mode`は、SOCKSプロキシが.onion宛先ホストのみに使用される'onion'（デフォルト）、
# またはすべての宛先ホストに使用される'always'にできます（.onionサーバーの場合に使用可能）。
# socks_mode: onion

# クライアントがプロキシコマンドを並列処理するために生成できるスレッド数を制限します。
# client_concurrency: 32

[INACTIVE_CLIENTS]
# 非アクティブクライアントのTTLと確認間隔
disconnect: off
# ttl: 21600
# check_interval: 3600

[WEB]
# サーバー情報とQRコード/リンクのための静的ミニサイトを生成するパスを設定
static_path: /var/opt/simplex/www

# このポートで埋め込みサーバーを実行
# Onionサイトは任意のポートを使用でき、隠しサービス設定に登録できます。
# ポート80で実行するにはプロセス機能の設定が必要な場合があります。
#http: 8000

# ポートと証明書およびキーファイルを提供すれば、埋め込みTLS Webサーバーも実行できます。
# onionアドレスでリレーを実行する場合は不要です。
https: 443
cert: /etc/opt/simplex/web.crt
key: /etc/opt/simplex/web.key
```

## サーバーセキュリティ

### 初期化

サーバー上で直接smp-server設定を初期化することは便利ですが、運営者には**SMPサーバーのCA秘密鍵を保護するため**にsmp-serverを完全にオフラインで初期化することを**推奨**します。

サーバーを迅速にオフラインで初期化するための手順：

1. システムにDockerをインストールします。

2. [smp-server](https://github.com/simplex-chat/simplexmq#using-docker)をローカルでデプロイします。

3. コンテナを破棄します。関連するすべての設定ファイルとキーは`$HOME/simplex/smp/config`で利用できます。

4. `CA`秘密鍵（`ca.key`）を安全な場所に移動します。詳細な説明については、次のセクション：[サーバーセキュリティ：秘密鍵](#秘密鍵)を参照してください。

5. CAキーを**除く**他のすべての設定ファイルをサーバーにコピーします：

   ```sh
   rsync -hzasP $HOME/simplex/smp/config/ <server_user>@<server_address>:/etc/opt/simplex/
   ```

### 秘密鍵

smpサーバーへの接続はTLS接続を介して行われます。TLSハンドシェイク中、クライアントはサーバーアドレスに含まれるフィンガープリントと比較して、smp-serverのCAとサーバー証明書を検証します。サーバーTLS認証情報が侵害された場合、このキーを使用して新しいものに署名でき、同じサーバーIDと確立された接続を維持できます。悪意のある行為者からsmp-serverを保護するため、運営者にはCA秘密鍵を安全な場所に移動することを**推奨**します。それは以下のようなものです：

- [永続化および暗号化されたストレージ](https://tails.net/doc/persistent_storage/create/index.en.html)付きの[Tails](https://tails.net/) live USBドライブ。
- オフラインLinuxラップトップ。
- Bitwarden。
- セキュリティ要件を満たすその他の安全なストレージ。

CAキーを保護するための手順：

1. SSHでサーバーにログインします。

2. このファイルからCAキーを安全な場所にコピーします：

   ```sh
   /etc/opt/simplex/ca.key
   ```

3. サーバーからCAキーを削除します。**CAキーを安全な場所に保存していることを確認してください。そうでなければ、[オンライン証明書のローテーション](#オンライン証明書のローテーション)を行う能力を失います**：

   ```sh
   rm /etc/opt/simplex/ca.key
   ```

### オンライン証明書のローテーション

smpサーバーの運営者には、定期的（例：3ヶ月ごと）にオンライン証明書をローテーションすることを**推奨**します。これを行うには、以下の手順に従ってください：

1. 関連するフォルダを作成：

   ```sh
   mkdir -p $HOME/simplex/smp/config
   ```

1. サーバーからローカルマシンに設定ファイルをコピー（まだの場合）：

   ```sh
   rsync -hzasP <server_user>@<server_address>:/etc/opt/simplex/ $HOME/simplex/smp/config/
   ```

2. 安全な場所からローカルマシンにCA秘密鍵を**コピー**し、`ca.key`と名前を付けます。

3. [Githubリリース](https://github.com/simplex-chat/simplexmq/releases)から最新の`smp-server`バイナリをダウンロード：

   ```sh
   curl -L 'https://github.com/simplex-chat/simplexmq/releases/latest/download/smp-server-ubuntu-20_04-x86-64' -o smp-server
   ```

4. `smp-server`バイナリを`$PATH`に配置し、実行可能にする：

   ```sh
   sudo mv smp-server /usr/local/bin/ && chmod +x /usr/local/bin/smp-server
   ```

5. smp-server設定へのパスを設定するための変数をエクスポート：

   ```sh
   export SMP_SERVER_CFG_PATH=$HOME/simplex/smp/config
   ```

6. 以下のコマンドを実行：

   ```sh
   smp-server cert
   ```

   このコマンドは以下を表示するはずです：

   ```sh
   Certificate request self-signature ok
   subject=CN = <your domain or IP>
   新しいサーバー認証情報を生成しました
   ----------
   CA秘密鍵を安全に保存し、サーバーから削除してください。
   サーバーTLS認証情報が侵害された場合、このキーを使用して新しいものに署名でき、同じサーバーIDと確立された接続を維持できます。
   CA秘密鍵の場所:
   $HOME/simplex/smp/config/ca.key
   ----------
   ```

7. 設定フォルダからCAキーを削除（バックアップがあることを確認してください！）：

   ```sh
   rm $HOME/simplex/smp/config/ca.key
   ```

8. 新しい証明書をサーバーにアップロード：

   ```sh
   rsync -hzasP $HOME/simplex/smp/config/ <server_user>@<server_address>:/etc/opt/simplex/
   ```

9. SSHでサーバーに接続し、サービスを再起動：

   ```sh
   ssh <server_user>@<server_address> "systemctl restart smp-server"
   ```

10. 完了！

## Tor: インストールと設定

### onionアドレスのためのインストール

SMP-serverは、[Tor](https://www.torproject.org)ネットワークを介して利用できるようにデプロイすることもできます。以下のコマンドを`root`ユーザーで実行してください。

1. torをインストール：

   Ubuntu/Debianベースのディストリビューションを使用していると仮定しています。そうでない場合は、[公式torドキュメント](https://community.torproject.org/onion-services/setup/install/)または使用しているディストリビューションのガイドを参照してください。

   - 公式Tor PPAリポジトリを設定：

     ```sh
     CODENAME="$(lsb_release -c | awk '{print $2}')"
     echo "deb [signed-by=/usr/share/keyrings/tor-archive-keyring.gpg] https://deb.torproject.org/torproject.org ${CODENAME} main
     deb-src [signed-by=/usr/share/keyrings/tor-archive-keyring.gpg] https://deb.torproject.org/torproject.org ${CODENAME} main" > /etc/apt/sources.list.d/tor.list
     ```

   - リポジトリキーをインポート：

     ```sh
     curl --proto '=https' --tlsv1.2 -sSf https://deb.torproject.org/torproject.org/A3C4F0F979CAA22CDBA8F512EE8CBC9E886DDD89.asc | gpg --dearmor | tee /usr/share/keyrings/tor-archive-keyring.gpg >/dev/null
     ```

   - リポジトリインデックスを更新：

     ```sh
     apt update
     ```

   - `tor`パッケージをインストール：

     ```sh
     apt install -y tor deb.torproject.org-keyring
     ```

2. torを設定：

   - ファイル設定：
  
     選択したエディタ（`nano`、`vim`、`emacs`など）でtor設定を開く：

     ```sh
     vim /etc/tor/torrc
     ```

     設定の最下部に以下の行を挿入してください。`#`で始まる行に注意してください：これらは各個別オプションに関するコメントです。

     ```sh
     # ログを有効にする（そうでなければ、torはonionアドレスをデプロイしないようです）
     Log notice file /var/log/tor/notices.log
     # シングルホップルーティングを有効にする（以下の2つのオプションは3つ目の依存関係です） - レイテンシーを減らしますが、サーバーの匿名性が低下します - SMP-serverのonionアドレスがクライアントでパブリックアドレスと一緒に使用されるので、これは問題ありません。onion専用アドレスでSMP-serverをデプロイする場合は、代わりに標準設定を維持することをお勧めします。
     SOCKSPort 0
     HiddenServiceNonAnonymousMode 1
     HiddenServiceSingleHopMode 1
     # smp-server隠しサービスホストディレクトリとポートマッピング
     HiddenServiceDir /var/lib/tor/simplex-smp/
     HiddenServicePort 5223 localhost:5223
     HiddenServicePort 443 localhost:443
     ```

   - ディレクトリを作成：

     ```sh
     mkdir /var/lib/tor/simplex-smp/ && chown debian-tor:debian-tor /var/lib/tor/simplex-smp/ && chmod 700 /var/lib/tor/simplex-smp/
     ```

3. torを開始：

   `systemd`サービスを有効にしてtorを開始します。公式の`tor`は初回起動時に少し不安定で、onionホストアドレスを作成しない場合があるため、念のため再起動しています。

   ```sh
   systemctl enable --now tor && systemctl restart tor
   ```

4. onionホストを表示：

   onionホストアドレスを表示するために以下のコマンドを実行：

   ```sh
   cat /var/lib/tor/simplex-smp/hostname
   ```

### SMP PROXYのためのSOCKSポート

`v5.8.0-beta.0`以降のSMP-serverバージョンでは、[Tor](https://www.torproject.org)ネットワークを介してのみ利用可能なsmpサーバーを、Torを使用しないクライアントがアクセスできるようにPROXYするように設定できます。以下のコマンドを`root`ユーザーで実行してください。

1. [前のセクション](#onionアドレスのためのインストール)で説明されているようにtorをインストール。

2. 新しいTorデーモンインスタンスを作成するために以下のコマンドを実行：

   ```sh
   tor-instance-create tor2
   ```

3. `tor2`設定を開き、以下の行で内容を置き換え：

   ```sh
   vim /etc/tor/instances/tor2/torrc
   ```

   ```sh
   # torをsystemdデーモンにログ
   Log notice syslog
   # socksプロキシのためにローカル9050ポートをリッスン
   SocksPort 9050
   ```

3. 起動時にサービスを有効にし、デーモンを開始：

   ```sh
   systemctl enable --now tor@tor2
   ```

   以下のコマンドで`tor2`ログを確認できます：

   ```sh
   journalctl -u tor@tor2
   ```

4. [サーバー初期化](#設定)後、以下のように`PROXY`セクションを設定：

   ```ini
   ...
   [PROXY]
   socks_proxy: 127.0.0.1:9050
   own_server_domains: <`log_stats: on`を使用する場合のドメインサフィックス>
   ...
   ```

## サーバー情報ページ

SMPサーバーは、管理者情報、サーバー情報、プロバイダー情報などを含むサーバー情報のWebページを提供するように設定する**べき**です。また、モバイル/デスクトップアプリを使用して生成された接続リンクも提供します。以下のコマンドを`root`ユーザーで実行してください。

_注意:_ この設定は`v6.1.0-beta.2`以降でサポートされています。

1. smp-server設定に以下を追加（[INFORMATION]セクションのフィールドを関連情報を含むように変更してください）：

   ```sh
   vim /etc/opt/simplex/smp-server.ini
   ```

   ```ini
   [TRANSPORT]
   # hostは開始時にサーバーアドレスを表示するためのみ使用
   host: <domain/ip>
   port: 443,5223
   websockets: off
   log_tls_errors: off
   control_port: 5224

   [WEB]
   https: 443
   static_path: /var/opt/simplex/www
   cert: /etc/opt/simplex/web.crt
   key: /etc/opt/simplex/web.key

   [INFORMATION]
   # AGPLv3ライセンスでは、サーバーのソースコードを変更した場合、
   # エンドユーザーが利用できるようにすることが求められます。
   # ライセンス: https://github.com/simplex-chat/simplexmq/blob/stable/LICENSE
   # サーバーのソースコードが何らかの形で変更された場合は、正しいソースコードURIを含めてください。
   # その他の情報フィールドが存在する場合、source_codeプロパティも必須です。

   source_code: https://github.com/simplex-chat/simplexmq

   # 以下のすべての情報の宣言は任意で、これらのフィールドのいずれも省略できます。

   # サーバー使用条件と修正。
   # 標準条件を別の文書の修正と一緒に使用することを推奨します。
   # usage_conditions: https://github.com/simplex-chat/simplex-chat/blob/stable/PRIVACY.md
   # condition_amendments: link

   # サーバーの場所と運営者。
   server_country: <YOUR_SERVER_LOCATION>
   operator: <YOUR_NAME>
   operator_country: <YOUR_LOCATION>
   website: <WEBSITE_IF_AVAILABLE>
  
   # 管理連絡先。
   #admin_simplex: SimpleXアドレス
   admin_email: <EMAIL>
   # admin_pgp:
   # admin_pgp_fingerprint:

   # 苦情とフィードバックの連絡先。
   # complaints_simplex: SimpleXアドレス
   complaints_email: <COMPLAINTS_EMAIL>
   # complaints_pgp:
   # complaints_pgp_fingerprint:

   # ホスティングプロバイダー。
   hosting: <HOSTING_PROVIDER_NAME>
   hosting_country: <HOSTING_PROVIDER_LOCATION> 
   ```

2. Webサーバーをインストールします。簡単なデプロイメントのため、Ubuntuサーバーでの[Caddy](https://caddyserver.com) Webサーバーのインストールプロセスを説明します：

   1. パッケージをインストール：

      ```sh
      sudo apt install -y debian-keyring debian-archive-keyring apt-transport-https curl
      ```

   2. リポジトリ用のcaddy gpgキーをインストール：

      ```sh
      curl -1sLf 'https://dl.cloudsmith.io/public/caddy/stable/gpg.key' | sudo gpg --dearmor -o /usr/share/keyrings/caddy-stable-archive-keyring.gpg
      ```

   3. Caddyリポジトリをインストール：

      ```sh
      curl -1sLf 'https://dl.cloudsmith.io/public/caddy/stable/debian.deb.txt' | sudo tee /etc/apt/sources.list.d/caddy-stable.list
      ```

   4. Caddyをインストール：

      ```sh
      sudo apt update && sudo apt install caddy
      ```

   [完全なCaddyインストール手順](https://caddyserver.com/docs/install)

3. Caddy設定を以下で置き換え：

   `YOUR_DOMAIN`を実際のドメイン（smp.example.com）に置き換えてください。

   ```sh
   vim /etc/caddy/Caddyfile
   ```

   ```
   http://YOUR_DOMAIN {
      redir https://YOUR_DOMAIN{uri} permanent
   }

   YOUR_DOMAIN:8443 {
      tls {
         key_type rsa4096
      }
   }
   ```

4. Caddyサービスを有効化して開始：

   ```sh
   systemctl enable --now caddy
   ```

5. smpディレクトリに証明書をコピーするスクリプトを作成：

   `YOUR_DOMAIN`を実際のドメイン（smp.example.com）に置き換えてください。

   ```sh
   vim /usr/local/bin/simplex-servers-certs
   ```

   ```sh
   #!/usr/bin/env sh
   set -eu

   user='smp'
   group="$user"

   domain='HOST'
   folder_in="/var/lib/caddy/.local/share/caddy/certificates/acme-v02.api.letsencrypt.org-directory/${domain}"
   folder_out='/etc/opt/simplex'
   key_name='web.key'
   cert_name='web.crt'

   # Caddyディレクトリからsmpサーバーディレクトリに証明書をコピー
   cp "${folder_in}/${domain}.crt" "${folder_out}/${cert_name}"
   # 正しい権限を割り当て
   chown "$user":"$group" "${folder_out}/${cert_name}"

   # Caddyディレクトリからsmpサーバーディレクトリに証明書キーをコピー
   cp "${folder_in}/${domain}.key" "${folder_out}/${key_name}"
   # 正しい権限を割り当て
   chown "$user":"$group" "${folder_out}/${key_name}"
   ```

6. スクリプトを実行可能にして実行：

   ```sh
   chmod +x /usr/local/bin/simplex-servers-certs && /usr/local/bin/simplex-servers-certs
   ```

7. 証明書がコピーされたかを確認：

   ```sh
   ls -haltr /etc/opt/simplex/web*
   ```

8. smpディレクトリに証明書を適時にコピーするcronjobを作成：

   ```sh
   sudo crontab -e
   ```

   ```sh
   # 毎週日曜日の00:20
   20 0 * * 0 /usr/local/bin/simplex-servers-certs
   ```

9. その後：

   - `v6.1.0-beta.2`以上を実行している場合は、[サーバーを再起動](#systemdコマンド)してください。
   - `v6.1.0-beta.2`未満を実行している場合は、[サーバーをアップグレード](#smpサーバーの更新)してください。

10. ブラウザーからデプロイしたWebページ（`https://smp.example.org`）にアクセスします。iniファイルで提供したsmp-server情報が表示されるはずです。

## ドキュメント

`smp-server`に必要なすべてのファイルは`/etc/opt/simplex/`フォルダーにあります。

保存されたメッセージ、接続、統計、サーバーログは`/var/opt/simplex/`フォルダーにあります。

### SMPサーバーアドレス

SMPサーバーアドレスには以下の形式があります：

```
smp://<fingerprint>[:<password>]@<public_hostname>[,<onion_hostname>]
```

- `<fingerprint>`

  `smp-server`の証明書フィンガープリント。証明書フィンガープリントは`/etc/opt/simplex/fingerprint`で確認できます。

- **オプション** `<password>`

  `smp-server`の設定されたパスワード。設定されたパスワードは`/etc/opt/simplex/smp-server.ini`の`[AUTH]`セクションの`create_password:`フィールドで確認できます。

- `<public_hostname>`、**オプション** `<onion_hostname>`

  `smp-server`の設定されたホスト名。設定されたホストは`/etc/opt/simplex/smp-server.ini`の`[TRANSPORT]`セクションの`host:`フィールドで確認できます。

### Systemdコマンド

ホスト起動時に`smp-server`を開始するには、以下を実行：

```sh
sudo systemctl enable smp-server.service

Created symlink /etc/systemd/system/multi-user.target.wants/smp-server.service → /etc/systemd/system/smp-server.service.
```

`smp-server`を開始するには、以下を実行：

```sh
sudo systemctl start smp-server.service
```

`smp-server`のステータスを確認するには、以下を実行：

```sh
sudo systemctl status smp-server.service

● smp-server.service - SMP server
     Loaded: loaded (/etc/systemd/system/smp-server.service; enabled; vendor preset: enabled)
     Active: active (running) since Sat 2022-11-23 19:23:21 UTC; 1min 48s ago
   Main PID: 30878 (smp-server)
     CGroup: /docker/5588ab759e80546b4296a7c50ffebbb1fb7b55b8401300e9201313b720989aa8/system.slice/smp-server.service
             └─30878 smp-server start

Nov 23 19:23:21 5588ab759e80 systemd[1]: Started SMP server.
Nov 23 19:23:21 5588ab759e80 smp-server[30878]: SMP server v3.4.0
Nov 23 19:23:21 5588ab759e80 smp-server[30878]: Fingerprint: d5fcsc7hhtPpexYUbI2XPxDbyU2d3WsVmROimcL90ss=
Nov 23 19:23:21 5588ab759e80 smp-server[30878]: Server address: smp://d5fcsc7hhtPpexYUbI2XPxDbyU2d3WsVmROimcL90ss=:V8ONoJ6ICwnrZnTC_QuSHfCEYq53uLaJKQ_oIC6-ve8=@<hostnames>
Nov 23 19:23:21 5588ab759e80 smp-server[30878]: Store log: /var/opt/simplex/smp-server-store.log
Nov 23 19:23:21 5588ab759e80 smp-server[30878]: Listening on port 5223 (TLS)...
Nov 23 19:23:21 5588ab759e80 smp-server[30878]: not expiring inactive clients
Nov 23 19:23:21 5588ab759e80 smp-server[30878]: creating new queues requires password
```

`smp-server`を停止するには、以下を実行：

```sh
sudo systemctl stop smp-server.service
```

`smp-server`ログの末尾を確認するには、以下を実行：

```sh
sudo journalctl -fu smp-server.service

Nov 23 19:23:21 5588ab759e80 systemd[1]: Started SMP server.
Nov 23 19:23:21 5588ab759e80 smp-server[30878]: SMP server v3.4.0
Nov 23 19:23:21 5588ab759e80 smp-server[30878]: Fingerprint: d5fcsc7hhtPpexYUbI2XPxDbyU2d3WsVmROimcL90ss=
Nov 23 19:23:21 5588ab759e80 smp-server[30878]: Server address: smp://d5fcsc7hhtPpexYUbI2XPxDbyU2d3WsVmROimcL90ss=:V8ONoJ6ICwnrZnTC_QuSHfCEYq53uLaJKQ_oIC6-ve8=@<hostnames>
Nov 23 19:23:21 5588ab759e80 smp-server[30878]: Store log: /var/opt/simplex/smp-server-store.log
Nov 23 19:23:21 5588ab759e80 smp-server[30878]: Listening on port 5223 (TLS)...
Nov 23 19:23:21 5588ab759e80 smp-server[30878]: not expiring inactive clients
Nov 23 19:23:21 5588ab759e80 smp-server[30878]: creating new queues requires password
```

### 制御ポート

設定で制御ポートを有効にすると、管理者がリアルタイムでsmp-serverに関する情報を確認できます。さらに、コンテンツモデレーションのためのキューの削除や、クライアント、ソケットなどに関するデバッグ情報の確認ができます。制御ポートを有効にするには、`admin`と`user`のパスワードの設定が必要です。

1. 各ユーザー用に2つのパスワードを生成：

   ```sh
   tr -dc A-Za-z0-9 </dev/urandom | head -c 20; echo
   ```

2. 設定ファイルを開く：

   ```sh
   vim /etc/opt/simplex/smp-server.ini
   ```

2. 制御ポートを設定し、パスワードを置き換え：

   ```ini
   [AUTH]
   control_port_admin_password: <your_randomly_generated_admin_password>
   control_port_user_password: <your_randomly_generated_user_password>

   [TRANSPORT]
   control_port: 5224
   ```

3. サーバーを再起動：

   ```sh
   systemctl restart smp-server
   ```

制御ポートにアクセスするには、以下を使用：

```sh
nc 127.0.0.1 5224
```

または：

```sh
telnet 127.0.0.1 5224
```

接続すると、制御ポートは以下を表示するはずです：

```sh
SMP server control port
サポートされているコマンドは 'help'
```

認証するには、以下を入力してEnterを押してください。`my_generated_password`を設定からの`user`または`admin`パスワードに変更してください：

```sh
auth my_generated_password
```

以下がコマンドの完全なリスト、説明、およびアクセス権限です。

| コマンド          | 説明                                                                     | `admin`権限が必要      |
| ---------------- | ------------------------------------------------------------------------------- | -------------------------- |
| `stats`          | リアルタイム統計。フィールドは[日次統計](#日次統計)で説明 | -                          |
| `stats-rts`      | GHC/Haskell統計。`+RTS -T -RTS`オプションで有効化可能               | -                          |
| `clients`        | クライアント情報。デバッグに有用。                                      | yes                        |
| `sockets`        | 一般的なソケット情報。                                                    | -                          |
| `socket-threads` | ソケットごとのスレッド情報。デバッグに有用。                             | yes                        |
| `threads`        | スレッド情報。デバッグに有用。                                      | yes                        |
| `server-info`    | 集約されたサーバー情報。                                                   | -                          |
| `delete`         | 既知のキューを削除。コンテンツモデレーションに有用。                              | -                          |
| `save`           | メモリからキュー/メッセージを保存。                                               | yes                        |
| `help`           | ヘルプメニュー。                                                                      | -                          |
| `quit`           | 制御ポートを終了。                                                          | -                          |

### 日次統計

`/etc/opt/simplex/smp-server.ini`の`[STORE_LOG]`セクションの`log_stats:`フィールドで値を`on`に設定することで、`Grafana`ダッシュボード用の`smp-server`統計を有効にできます。

ログは`/var/opt/simplex/smp-server-stats.daily.log`の`csv`ファイルとして保存されます。`csv`ファイルのフィールドは：

```sh
fromTime,qCreated,qSecured,qDeleted,msgSent,msgRecv,dayMsgQueues,weekMsgQueues,monthMsgQueues,msgSentNtf,msgRecvNtf,dayCountNtf,weekCountNtf,monthCountNtf,qCount,msgCount,msgExpired,qDeletedNew,qDeletedSecured,pRelays_pRequests,pRelays_pSuccesses,pRelays_pErrorsConnect,pRelays_pErrorsCompat,pRelays_pErrorsOther,pRelaysOwn_pRequests,pRelaysOwn_pSuccesses,pRelaysOwn_pErrorsConnect,pRelaysOwn_pErrorsCompat,pRelaysOwn_pErrorsOther,pMsgFwds_pRequests,pMsgFwds_pSuccesses,pMsgFwds_pErrorsConnect,pMsgFwds_pErrorsCompat,pMsgFwds_pErrorsOther,pMsgFwdsOwn_pRequests,pMsgFwdsOwn_pSuccesses,pMsgFwdsOwn_pErrorsConnect,pMsgFwdsOwn_pErrorsCompat,pMsgFwdsOwn_pErrorsOther,pMsgFwdsRecv,qSub,qSubAuth,qSubDuplicate,qSubProhibited,msgSentAuth,msgSentQuota,msgSentLarge,msgNtfs,msgNtfNoSub,msgNtfLost,qSubNoMsg,msgRecvGet,msgGet,msgGetNoMsg,msgGetAuth,msgGetDuplicate,msgGetProhibited,psSubDaily,psSubWeekly,psSubMonthly,qCount2,ntfCreated,ntfDeleted,ntfSub,ntfSubAuth,ntfSubDuplicate,ntfCount,qDeletedAllB,qSubAllB,qSubEnd,qSubEndB,ntfDeletedB,ntfSubB,msgNtfsB,msgNtfExpired
```

**フィールドの説明**

| フィールド番号  | フィールド名                   | フィールドの説明          |
| ------------- | ---------------------------- | -------------------------- |
| 1             | `fromTime`                   | 統計の日付         |
| メッセージキュー:                                                          |
| 2             | `qCreated`                   | 作成数                    |
| 3             | `qSecured`                   | 確立数                |
| 4             | `qDeleted`                   | 削除数                    |
| メッセージ:                                                                 |
| 5             | `msgSent`                    | 送信数                       |
| 6             | `msgRecv`                    | 受信数                   |
| 7             | `dayMsgQueues`               | 1日のアクティブキュー数     |
| 8             | `weekMsgQueues`              | 1週間のアクティブキュー数    |
| 9             | `monthMsgQueues`             | 1ヶ月のアクティブキュー数   |
| "notification"フラグ付きメッセージ                                         |
| 10            | `msgSentNtf`                 | 送信数                       |
| 11            | `msgRecvNtf`                 | 受信数                   |
| 12            | `dayCountNtf`                | 1日のアクティブキュー数     |
| 13            | `weekCountNtf`               | 1週間のアクティブキュー数    |
| 14            | `monthCountNtf`              | 1ヶ月のアクティブキュー数   |
| 追加統計:                                                    |
| 15            | `qCount`                     | 保存キュー数              |
| 16            | `msgCount`                   | 保存メッセージ数            |
| 17            | `msgExpired`                 | 期限切れメッセージ数           |
| 18            | `qDeletedNew`                | 新規削除キュー数         |
| 19            | `qDeletedSecured`            | セキュア削除キュー数     |
| すべてのリレーとの要求セッション:                                       |
| 20            | `pRelays_pRequests`          | - 要求数                 |
| 21            | `pRelays_pSuccesses`         | - 成功数                |
| 22            | `pRelays_pErrorsConnect`     | - 接続エラー数        |
| 23            | `pRelays_pErrorsCompat`      | - 互換性エラー数     |
| 24            | `pRelays_pErrorsOther`       | - その他のエラー数             |
| 独自リレーとの要求セッション:                                       |
| 25            | `pRelaysOwn_pRequests`       | - 要求数                 |
| 26            | `pRelaysOwn_pSuccesses`      | - 成功数                |
| 27            | `pRelaysOwn_pErrorsConnect`  | - 接続エラー数        |
| 28            | `pRelaysOwn_pErrorsCompat`   | - 互換性エラー数     |
| 29            | `pRelaysOwn_pErrorsOther`    | - その他のエラー数             |
| すべてのリレーへのメッセージ転送:                                           |
| 30            | `pMsgFwds_pRequests`         | - 要求数                 |
| 31            | `pMsgFwds_pSuccesses`        | - 成功数                |
| 32            | `pMsgFwds_pErrorsConnect`    | - 接続エラー数        |
| 33            | `pMsgFwds_pErrorsCompat`     | - 互換性エラー数     |
| 34            | `pMsgFwds_pErrorsOther`      | - その他のエラー数             |
| 独自リレーへのメッセージ転送:                                            |
| 35            | `pMsgFwdsOwn_pRequests`      | - 要求数                 |
| 36            | `pMsgFwdsOwn_pSuccesses`     | - 成功数                |
| 37            | `pMsgFwdsOwn_pErrorsConnect` | - 接続エラー数        |
| 38            | `pMsgFwdsOwn_pErrorsCompat`  | - 互換性エラー数     |
| 39            | `pMsgFwdsOwn_pErrorsOther`   | - その他のエラー数             |
| 受信メッセージ転送:                                                |
| 40            | `pMsgFwdsRecv`               |                            |
| メッセージキューサブスクリプションエラー:                                        |
| 41            | `qSub`                       | すべて                        |
| 42            | `qSubAuth`                   | 認証エラー      |
| 43            | `qSubDuplicate`              | 重複SUBエラー       |
| 44            | `qSubProhibited`             | 禁止SUBエラー      |
| メッセージエラー:                                                           |
| 45            | `msgSentAuth`                | 認証エラー      |
| 46            | `msgSentQuota`               | クォータエラー               |
| 47            | `msgSentLarge`               | 大メッセージエラー       |
| 48            | `msgNtfs`                    | XXXXXXXXXXXXXXXXXXXX       |
| 49            | `msgNtfNoSub`                | XXXXXXXXXXXXXXXXXXXX       |
| 50            | `msgNtfLost`                 | XXXXXXXXXXXXXXXXXXXX       |
| 51            | `qSubNoMsg`                  | 削除済み、常に0          |
| 52            | `msgRecvGet`                 | XXXXXXXXXXXXXXXXX          |
| 53            | `msgGet`                     | XXXXXXXXXXXXXXXXX          |
| 54            | `msgGetNoMsg`                | XXXXXXXXXXXXXXXXX          |
| 55            | `msgGetAuth`                 | XXXXXXXXXXXXXXXXX          |
| 56            | `msgGetDuplicate`            | XXXXXXXXXXXXXXXXX          |
| 57            | `msgGetProhibited`           | XXXXXXXXXXXXXXXXX          |
| 58            | `psSub_dayCount`             | 削除済み、常に0          |
| 59            | `psSub_weekCount`            | 削除済み、常に0          |
| 60            | `psSub_monthCount`           | 削除済み、常に0          |
| 61            | `qCount`                     | XXXXXXXXXXXXXXXXX          |
| 62            | `ntfCreated`                 | XXXXXXXXXXXXXXXXX          |
| 63            | `ntfDeleted`                 | XXXXXXXXXXXXXXXXX          |
| 64            | `ntfSub`                     | XXXXXXXXXXXXXXXXX          |
| 65            | `ntfSubAuth`                 | XXXXXXXXXXXXXXXXX          |
| 66            | `ntfSubDuplicate`            | XXXXXXXXXXXXXXXXX          |
| 67            | `ntfCount`                   | XXXXXXXXXXXXXXXXX          |
| 68            | `qDeletedAllB`               | XXXXXXXXXXXXXXXXX          |
| 69            | `qSubAllB`                   | XXXXXXXXXXXXXXXXX          |
| 70            | `qSubEnd`                    | XXXXXXXXXXXXXXXXX          |
| 71            | `qSubEndB`                   | XXXXXXXXXXXXXXXXX          |
| 72            | `ntfDeletedB`                | XXXXXXXXXXXXXXXXX          |
| 73            | `ntfSubB`                    | XXXXXXXXXXXXXXXXX          |
| 74            | `msgNtfsB`                   | XXXXXXXXXXXXXXXXX          |
| 75            | `msgNtfExpired`              | XXXXXXXXXXXXXXXXX          |

`csv`を`Grafana`にインポートするには：

1. Grafanaプラグインをインストール：[Grafana - CSV datasource](https://grafana.com/grafana/plugins/marcusolsson-csv-datasource/)

2. 以下を追加してローカルモードを許可：

   ```sh
   [plugin.marcusolsson-csv-datasource]
   allow_local_mode = true
   ```

   ... を `/etc/grafana/grafana.ini` に

3. CSVデータソースを追加：

   - サイドメニューでConfiguration（歯車アイコン）タブをクリック
   - Data Sourcesタブの右上角の Add data source をクリック
   - 検索ボックスに "CSV" と入力してCSVデータソースを見つける
   - "CSV" と表示された検索結果をクリック
   - URLに、CSVコンテンツを指すファイルを入力

4. 完了！統計を使って独自のダッシュボードを作成できるはずです。

詳細なドキュメントについては、[CSV Data Source for Grafana - Documentation](https://grafana.github.io/grafana-csv-datasource/)を参照してください

## SMPサーバーの更新

smp-serverを最新バージョンに更新するには、インストール方法を選択して手順に従ってください：

   - 手動デプロイ

     1. サーバーを停止：

        ```sh
        sudo systemctl stop smp-server
        ```

     2. バイナリを更新：

        ```sh
         curl -L https://github.com/simplex-chat/simplexmq/releases/latest/download/smp-server-ubuntu-20_04-x86-64 -o /usr/local/bin/smp-server && chmod +x /usr/local/bin/smp-server
        ```

     3. サーバーを開始：

        ```sh
        sudo systemctl start smp-server
        ```

   - [公式インストールスクリプト](https://github.com/simplex-chat/simplexmq#using-installation-script)

     1. 以下のコマンドを実行：

        ```sh
        sudo simplex-servers-update
        ```

        特定のバージョンをインストールするには、以下を実行：

        ```sh
        export VER=<version_from_github_releases> &&\
        sudo -E simplex-servers-update
        ```

     2. 完了！

   - [Dockerコンテナ](https://github.com/simplex-chat/simplexmq#using-docker)

     1. コンテナを停止して削除：

        ```sh
        docker rm $(docker stop $(docker ps -a -q --filter ancestor=simplexchat/smp-server --format="\{\{.ID\}\}"))
        ```

     2. 最新のイメージをプル：

        ```sh
        docker pull simplexchat/smp-server:latest
        ```

     3. 新しいコンテナを開始：

        ```sh
        docker run -d \
          -p 5223:5223 \
          -p 443:443 \
          -v $HOME/simplex/smp/config:/etc/opt/simplex:z \
          -v $HOME/simplex/smp/logs:/var/opt/simplex:z \
          simplexchat/smp-server:latest
        ```

   - [Linode Marketplace](https://www.linode.com/marketplace/apps/simplex-chat/simplex-chat/)

     1. 最新のイメージをプル：

        ```sh
        docker-compose --project-directory /etc/docker/compose/simplex pull
        ```

     2. コンテナを再起動：

        ```sh
        docker-compose --project-directory /etc/docker/compose/simplex up -d --remove-orphans
        ```

     3. 古いイメージを削除：

        ```sh
        docker image prune
        ```

## ビルドの再現

これらの手順に従って、サーバーバイナリをローカルで再現できます。

セキュリティ専門家または研究者の場合、リリースチェックサムに署名することでSimpleXネットワークとユーザーコミュニティを支援できます。[署名を公開](https://github.com/simplex-chat/simplexmq/releases/tag/v6.3.1)します。お気軽にご連絡ください！

ビルドを再現するには以下が必要です：

- Linuxマシン
- `x86-64`アーキテクチャ
- インストール済みの`docker`、`curl`、`git`

1. スクリプトをダウンロード：

   ```sh
   curl -LO 'https://raw.githubusercontent.com/simplex-chat/simplexmq/refs/heads/master/scripts/simplexmq-reproduce-builds.sh'
   ```

2. 実行可能にする：

   ```sh
   chmod +x simplexmq-reproduce-builds.sh
   ```

3. 必要なタグでスクリプトを実行：

   ```sh
   ./simplexmq-reproduce-builds.sh 'v6.3.1'
   ```

   スクリプトは以下の手順を実行します（スクリプトを確認してください）：

   1) dockerコンテナでリリース用のすべてのサーバーバイナリをビルドします。
   2) 同じGitHubリリースからバイナリをダウンロードし、ビルドしたバイナリと比較します。
   3) すべてが一致する場合、チェックサムを含む_sha256sumsファイルを生成します。

   これには時間がかかります。

4. コンパイル後、タグと同じ名前のフォルダ（例：`v6.3.1`）と2つのサブフォルダが表示されるはずです：

   ```sh
   ls v6.3.1
   ```

   ```sh
   from-source  prebuilt  _sha256sums
   ```

   _sha256sumsファイルには、すべてのビルドのハッシュが含まれています。GitHubリリースの同じファイルと比較できます。

## サーバーを使用するためのアプリ設定

メッセージサーバーを使用するようにアプリを設定するには、パスワードを含む完全なアドレスをコピーして、アプリに追加します。プリセットサーバーと一緒にサーバーを使用するか、プリセットサーバーなしで使用するかを選択できます。プリセットサーバーを削除または無効にできます。

サーバー設定からQRコードをスキャンして、友人とサーバーのアドレスを共有することも可能です。これにはサーバーパスワードが含まれるため、友人もサーバー経由でメッセージを受信できるようになります。

_注意_：パスワードサポートにはSMPサーバーバージョン4.0が必要です。すでにデプロイされたサーバーがある場合は、サーバーINIファイルにパスワードを追加できます。

<img src="../../../images/server_config_1.png" width="288"> &nbsp;&nbsp; <img src="../../../images/server_config_2.png" width="288"> &nbsp;&nbsp; <img src="../../../images/server_config_3.png" width="288">
