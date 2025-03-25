---
title: 托管您自己的SMP服务器
revision: 12.10.2024
---

# 托管您自己的SMP服务器

| 更新于 12.10.2024 | 语言：ZH_CN, EN, [FR](/docs/lang/fr/SERVER.md), [CZ](/docs/lang/cs/SERVER.md), [PL](/docs/lang/pl/SERVER.md) |

## 目录

- [概述](#overview)
- [快速开始](#quick-start) 使用 systemd 服务
- [安装选项](#installation-options)
   - [systemd 服务](#systemd-service) 使用 [安装脚本](#installation-script) 或 [手动部署](#manual-deployment)
   - [Docker 容器](#docker-container)
   - [Linode 市场](#linode-marketplace)
- [验证服务器二进制文件](#verifying-server-binaries)
- [配置](#configuration)
   - [交互式配置](#interactively)
   - [通过命令行选项配置](#via-command-line-options)
- [进一步配置](#further-configuration)
- [服务器安全](#server-security)
   - [初始化](#initialization)
   - [私钥](#private-keys)
   - [在线证书轮换](#online-certificate-rotation)
- [Tor：安装与配置](#tor-installation-and-configuration)
   - [为洋葱地址安装](#installation-for-onion-address)
   - [SMP代理的SOCKS端口](#socks-port-for-smp-proxy)
- [服务器信息页面](#server-information-page)
- [文档](#documentation)
   - [SMP服务器地址](#smp-server-address)
   - [Systemd命令](#systemd-commands)
   - [控制端口](#control-port)
   - [每日统计](#daily-statistics)
- [重现构建](#reproduce-builds)
- [更新您的SMP服务器](#updating-your-smp-server)
- [配置应用以使用服务器](#configuring-the-app-to-use-the-server)

## 概述

SMP服务器是SimpleX网络中用于传递消息的中继服务器。SimpleX Chat应用程序预设了服务器（对于移动应用程序，这些是smp11、smp12和smp14.simplex.im），但您可以轻松更改应用配置以使用其他服务器。

SimpleX客户端仅决定用于接收消息的服务器，每个联系人（或与组成员的组连接）分别设置，这些服务器只是临时的，因为传递地址可以更改。

要创建SMP服务器，您需要：

1. VPS或其他服务器。
2. 指向服务器的域名（例如`smp.example.com`）。
3. 基本的Linux知识。

_请注意_：当您在应用配置中更改服务器时，它仅影响新联系人的服务器使用，现有联系人不会自动迁移到新服务器，但您可以通过联系人/成员信息页面中的["更改接收地址"](../blog/20221108-simplex-chat-v4.2-security-audit-new-website.md#change-your-delivery-address-beta)按钮手动迁移——未来将实现自动化。

## 快速开始

要将SMP服务器创建为systemd服务，您需要：

- VPS或其他服务器。
- 您的服务器域名，具有指定IPv4和IPv6地址的A和AAAA记录（例如`smp1.example.com`）。
- 基本的Linux知识。

*请注意*：虽然您可以在没有域名的情况下运行SMP服务器，但在不久的将来，客户端应用程序将在邀请链接中使用服务器域名（而不是当前使用的`simplex.chat`域名）。如果服务器没有域名和服务器页面（见下文），客户端将生成无法在浏览器中打开的`simplex:`协议链接。

1. 使用[安装脚本](https://github.com/simplex-chat/simplexmq#using-installation-script)安装服务器。

2. 调整防火墙：

   ```sh
   ufw allow 80/tcp &&\
   ufw allow 443/tcp &&\
   ufw allow 5223/tcp
   ```

3. 初始化服务器：

   将`smp1.example.com`替换为您的实际服务器域名。

   ```sh
   su smp -c 'smp-server init --yes \
                           --store-log \
                           --no-password \
                           --control-port \
                           --socks-proxy \
                           --source-code \
                           --fqdn=smp1.example.com
   ```

4. 安装Tor：

   ```sh
   CODENAME="$(lsb_release -c | awk '{print $2}')"

   echo "deb [signed-by=/usr/share/keyrings/tor-archive-keyring.gpg] https://deb.torproject.org/torproject.org ${CODENAME} main
   deb-src [signed-by=/usr/share/keyrings/tor-archive-keyring.gpg] https://deb.torproject.org/torproject.org ${CODENAME} main" > /etc/apt/sources.list.d/tor.list &&\
   curl --proto '=https' --tlsv1.2 -sSf https://deb.torproject.org/torproject.org/A3C4F0F979CAA22CDBA8F512EE8CBC9E886DDD89.asc | gpg --dearmor | tee /usr/share/keyrings/tor-archive-keyring.gpg >/dev/null &&\
   apt update && apt install -y tor deb.torproject.org-keyring
   ```

5. 配置Tor：

   ```sh
   tor-instance-create tor2 &&\
   mkdir /var/lib/tor/simplex-smp/ &&\
   chown debian-tor:debian-tor /var/lib/tor/simplex-smp/ &&\
   chmod 700 /var/lib/tor/simplex-smp/
   ```

   ```sh
   vim /etc/tor/torrc
   ```

   粘贴以下内容：

   ```sh
   # 启用日志（否则，Tor似乎不会部署洋葱地址）
   Log notice file /var/log/tor/notices.log
   # 启用单跳路由（以下两个选项是第三个选项的依赖项） - 它将以较低的匿名性为代价减少延迟 - 由于SMP服务器洋葱地址与公共地址一起在客户端中使用，这没问题。如果您仅部署洋葱地址的SMP服务器，请保持标准配置。
   SOCKSPort 0
   HiddenServiceNonAnonymousMode 1
   HiddenServiceSingleHopMode 1
   # SMP服务器隐藏服务主机目录和端口映射
   HiddenServiceDir /var/lib/tor/simplex-smp/
   HiddenServicePort 5223 localhost:5223
   HiddenServicePort 443 localhost:443
   ```

   ```sh
   vim /etc/tor/instances/tor2/torrc
   ```

   粘贴以下内容：

   ```sh
   # 将Tor日志记录到systemd守护进程
   Log notice syslog
   # 监听本地9050端口以用于SOCKS代理
   SocksPort 9050
   ```

6. 启动Tor：

   ```sh
   systemctl enable tor &&\
   systemctl start tor &&\
   systemctl restart tor &&\
   systemctl enable --now tor@tor2
   ```

7. 安装Caddy：

   ```sh
   sudo apt install -y debian-keyring debian-archive-keyring apt-transport-https curl &&\
   curl -1sLf 'https://dl.cloudsmith.io/public/caddy/stable/gpg.key' | sudo gpg --dearmor -o /usr/share/keyrings/caddy-stable-archive-keyring.gpg &&\
   curl -1sLf 'https://dl.cloudsmith.io/public/caddy/stable/debian.deb.txt' | sudo tee /etc/apt/sources.list.d/caddy-stable.list &&\
   sudo apt update && sudo apt install caddy
   ```

8. 配置Caddy：

   ```sh
   vim /etc/caddy/Caddyfile
   ```

   将`smp1.example.com`替换为您的实际服务器域名。粘贴以下内容：

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

   将`smp1.example.com`替换为您的实际服务器域名。粘贴以下内容：

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

   # 从Caddy目录复制证书到SMP服务器目录
   cp "${folder_in}/${domain}.crt" "${folder_out}/${cert_name}"
   # 分配正确的权限
   chown "$user":"$group" "${folder_out}/${cert_name}"

   # 从Caddy目录复制证书密钥到SMP服务器目录
   cp "${folder_in}/${domain}.key" "${folder_out}/${key_name}"
   # 分配正确的权限
   chown "$user":"$group" "${folder_out}/${key_name}"
   ```

   ```sh
   chmod +x /usr/local/bin/simplex-servers-certs
   ```

   ```sh
   sudo crontab -e
   ```

   粘贴以下内容：

   ```sh
   # 每周日00:20
   20 0 * * 0 /usr/local/bin/simplex-servers-certs
   ```

9. 启用并启动Caddy服务：

   等待打印“good to go”。

   ```sh
   systemctl enable --now caddy &&\
   sleep 10 &&\
   /usr/local/bin/simplex-servers-certs &&\
   echo 'good to go'
   ```

10. 启用并启动SMP服务器：

    ```sh
    systemctl enable --now smp-server.service
    ```

11. 打印您的地址：

    ```sh
    smp="$(journalctl --output cat -q _SYSTEMD_INVOCATION_ID="$(systemctl show -p InvocationID --value smp-server)" | grep -m1 'Server address:' | awk '{print $NF}' | sed 's/:443.*//')"
    tor="$(cat /var/lib/tor/simplex-smp/hostname)"

    echo "$smp,$tor"
    ```

## 安装选项

您可以通过以下方式之一安装SMP服务器：

- [systemd 服务](#systemd-service)
   - 使用 [安装脚本](#installation-script) - **推荐**
   - 或 [手动部署](#manual-deployment)
- [Docker 容器](#docker-container) 从 DockerHub
- [Linode 市场](#linode-marketplace)

### systemd 服务

#### 安装脚本

此安装脚本将自动安装二进制文件、systemd服务和其他脚本，这些脚本将管理备份、更新和卸载。这是推荐的选项，因为它具有灵活性、易于更新，并且在我们的服务器上经过了实战测试。

**请注意**，目前仅支持Ubuntu发行版。

在服务器上运行以下脚本：

```sh
curl --proto '=https' --tlsv1.2 -sSf https://raw.githubusercontent.com/simplex-chat/simplexmq/stable/install.sh -o simplex-server-install.sh &&\
if echo '53fcdb4ceab324316e2c4cda7e84dbbb344f32550a65975a7895425e5a1be757 simplex-server-install.sh' | sha256sum -c; then
  chmod +x ./simplex-server-install.sh
  ./simplex-server-install.sh
  rm ./simplex-server-install.sh
else
  echo "SHA-256 checksum is incorrect!"
  rm ./simplex-server-install.sh
fi
```

输入`1`并按回车键安装`smp-server`。

#### 手动部署

手动安装是最先进的部署方式，提供了最大的灵活性。通常仅推荐给高级用户。

1. 安装二进制文件：

   - 使用预编译的二进制文件：

     ```sh
     curl -L https://github.com/simplex-chat/simplexmq/releases/latest/download/smp-server-ubuntu-20_04-x86-64 -o /usr/local/bin/smp-server && chmod +x /usr/local/bin/smp-server
     ```

   - 从源代码编译：

     请参考[从源代码构建：使用您的发行版](https://github.com/simplex-chat/simplexmq#using-your-distribution)

2. 为`smp-server`创建用户和组：

   ```sh
   sudo useradd -m smp
   ```

3. 创建必要的目录并分配权限：

   ```sh
   sudo mkdir -p /var/opt/simplex /etc/opt/simplex
   sudo chown smp:smp /var/opt/simplex /etc/opt/simplex
   ```

4. 在防火墙中允许`smp-server`端口：

   ```sh
   # 对于Ubuntu
   sudo ufw allow 5223/tcp
   sudo ufw allow 443/tcp
   sudo ufw allow 80/tcp
   # 对于Fedora
   sudo firewall-cmd --permanent --add-port=5223/tcp --add-port=443/tcp --add-port=80/tcp && \
   sudo firewall-cmd --reload
   ```

5. **可选** - 如果您使用的是带有`systemd`的发行版，请创建`/etc/systemd/system/smp-server.service`文件，内容如下：

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

   并执行`sudo systemctl daemon-reload`。

### Docker 容器

您可以使用Docker Compose部署smp-server。这是第二推荐的选项，因为它的流行和相对容易的部署。

此部署提供了两个Docker Compose文件：**自动**和**手动**。如果您不确定，请选择**自动**。

这将从[Docker Hub](https://hub.docker.com/r/simplexchat)下载镜像。

#### Docker：自动设置

此配置提供了快速简便的方式来设置您的SMP服务器：Caddy将自动管理Let's Encrypt证书并将HTTP重定向到HTTPS，而smp-server将通过443端口同时提供[服务器信息页面](#server-information-page)和SMP协议。5223端口用作备用。

**请注意**，您必须有`80`和`443`端口未被其他服务器占用。

1. 创建`smp-server`目录并切换到该目录：

  ```sh
  mkdir smp-server && cd smp-server
  ```

2. 创建`docker-compose.yml`文件，内容如下：

  您也可以从这里获取 - [docker-compose-smp-complete.yml](https://raw.githubusercontent.com/simplex-chat/simplexmq/refs/heads/stable/scripts/docker/docker-compose-smp-complete.yml)。不要忘记将其重命名为`docker-compose.yml`。

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
        ADDR: ${ADDR?"Please specify the domain."}
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
        ADDR: ${ADDR?"Please specify the domain."}
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

3. 在同一目录中，创建`.env`文件，内容如下：

  您也可以从这里获取 - [docker-compose-smp-complete.env](https://raw.githubusercontent.com/simplex-chat/simplexmq/refs/heads/stable/scripts/docker/docker-compose-smp-complete.env)。不要忘记将其重命名为`.env`。

  根据您的偏好更改变量。

  ```env
  # 强制
  ADDR=your_ip_or_addr

  # 可选
  #PASS='123123'
  ```

4. 启动您的容器：

  ```sh
  docker compose up
  ```

#### Docker：手动设置

如果您知道自己在做什么，此配置提供了裸SMP服务器设置，而无需Caddy自动管理Let's Encrypt证书以通过5223端口提供[服务器信息页面](#server-information-page)。

此配置允许您保留自己管理80和443端口的能力。缺点是SMP服务器**不能**通过443端口提供服务。

1. 创建`smp-server`目录并切换到该目录：

  ```sh
  mkdir smp-server && cd smp-server
  ```

2. 创建`docker-compose.yml`文件，内容如下：

  您也可以从这里获取 - [docker-compose-smp-manual.yml](https://raw.githubusercontent.com/simplex-chat/simplexmq/refs/heads/stable/scripts/docker/docker-compose-smp-manual.yml)。不要忘记将其重命名为`docker-compose.yml`。

  ```yaml
  name: SimpleX Chat - smp-server

  services:
    smp-server:
      image: ${SIMPLEX_IMAGE:-simplexchat/smp-server:latest}
      environment:
        WEB_MANUAL: ${WEB_MANUAL:-1}
        ADDR: ${ADDR?"Please specify the domain."}
        PASS: ${PASS:-}
      volumes:
        - ./smp_configs:/etc/opt/simplex
        - ./smp_state:/var/opt/simplex
      ports:
        - 5223:5223
      restart: unless-stopped
  ```

3. 在同一目录中，创建`.env`文件，内容如下：

  您也可以从这里获取 - [docker-compose-smp-manual.env](https://raw.githubusercontent.com/simplex-chat/simplexmq/refs/heads/stable/scripts/docker/docker-compose-smp-manual.env)。不要忘记将其重命名为`.env`。

  根据您的偏好更改变量。

  ```env
  # 强制
  ADDR=your_ip_or_addr

  # 可选
  #PASS='123123'
  WEB_MANUAL=1
  ```

4. 启动您的容器：

  ```sh
  docker compose up
  ```

### Linode 市场

您可以在创建新的Linode虚拟机时部署smp-server。请参考：[Linode Marketplace](https://www.linode.com/marketplace/apps/simplex-chat/simplex-chat/)

## 验证服务器二进制文件

从v6.3版本开始，服务器构建是[可重现的](#reproduce-builds)。

这也使我们能够签署服务器发布，确认GitHub构建的完整性。

要在下载服务器二进制文件后验证它们：

1. 下载`_sha256sums`（所有服务器二进制文件的哈希值）和`_sha256sums.asc`（签名）。

2. 从[openpgp.org](https://keys.openpgp.org/search?q=chat%40simplex.chat)下载我们的密钥FB44AF81A45BDE327319797C85107E357D4A17FC。

3. 使用`gpg --import FB44AF81A45BDE327319797C85107E357D4A17FC`导入密钥。密钥文件名应与其指纹相同，但请根据需要更改。

4. 运行`gpg --verify --trusted-key  _sha256sums.asc _sha256sums`。它应打印：

> Good signature from "SimpleX Chat <chat@simplex.chat>"

5. 使用`sha256sum <file>`或`openssl sha256 <file>`计算您计划使用的二进制文件的哈希值，并将它们与文件`_sha256sums`中的哈希值进行比较 - 它们必须相同。

就是这样 - 您现在已经验证了我们的GitHub服务器二进制文件的真实性。

## 配置

要查看可用的选项，请执行不带标志的`smp-server`：

```sh
sudo su smp -c smp-server

...
可用命令：
  init                     初始化服务器 - 创建 /etc/opt/simplex 和
                           /var/opt/simplex 目录和配置文件
  start                    启动服务器（配置：
                           /etc/opt/simplex/smp-server.ini）
  delete                   删除配置和日志文件
```

您可以通过执行`sudo su smp -c "smp-server <command> -h"`获取更多帮助。

之后，我们需要配置`smp-server`：

### 交互式配置

执行以下命令：

```sh
sudo su smp -c "smp-server init"
```

有几个选项需要考虑：

- `启用存储日志以在服务器重启时恢复队列和消息（Yn）：`

  输入`y`以启用在服务器重启时保存和恢复连接和消息。

  _请注意_：重要的是使用SIGINT重启服务器，否则未发送的消息将不会恢复。无论服务器如何重启，连接都会恢复，因为与消息不同，它们在每次更改时都会添加到追加日志中。

- `启用日志记录每日统计信息（yN）：`

  输入`y`以启用以CSV格式记录统计信息，例如，它们可以用于在`Grafana`中显示汇总使用图表。

这些统计信息包括创建、保护和删除队列的每日计数，发送和接收的消息，以及每日、每周和每月的活动队列计数（即，使用任何消息的队列）。我们相信这些信息不包括任何允许将不同队列关联为同一用户的信息，但如果您认为这可能被利用，请[告知我们](./SECURITY.md)，我们会保密处理。

- `需要密码才能创建新的消息队列？`

  按回车键或输入任意密码以密码保护`smp-server`，或输入`n`以禁用密码保护。

- `输入服务器FQDN或IP地址以获取证书（127.0.0.1）：`

  输入您的域名或运行smp-server的IP地址 - 它将包含在服务器证书中，并作为服务器地址的一部分打印。

### 通过命令行选项配置

执行以下命令：

```sh
sudo su smp -c "smp-server init -h"

...
可用选项：
  -l,--store-log           启用存储日志以实现持久性
  -s,--daily-stats         启用日志记录每日服务器统计信息
  -a,--sign-algorithm ALG  用于TLS证书的签名算法：
                           ED25519, ED448（默认：ED448）
  --ip IP                  服务器IP地址，用于TLS在线证书的通用名称
                           如果未提供FQDN（默认："127.0.0.1"）
  -n,--fqdn FQDN           用于TLS在线证书的服务器FQDN
  --no-password            允许在没有密码的情况下创建新队列
  --password PASSWORD      设置密码以创建新的消息队列
  -y,--yes                 使用命令行选项进行非交互式初始化
  -h,--help                显示此帮助文本
```

您应确定哪些标志适合您的用例，然后使用`-y`标志执行`smp-server init`进行非交互式初始化：

```sh
sudo su smp -c "smp-server init -y -<your flag> <your option>"
```

例如，运行：

```sh
sudo su smp -c "smp-server init -y -l --ip 192.168.1.5 --password test"
```

以初始化您的`smp-server`配置：

- 在服务器重启时恢复连接和消息（`-l`标志），
- IP地址`192.168.1.5`，
- 使用密码`test`保护`smp-server`。

---

之后，您的安装完成，您应该在终端输出中看到类似以下内容：

```sh
Certificate request self-signature ok
subject=CN = 127.0.0.1
Server initialized, you can modify configuration in /etc/opt/simplex/smp-server.ini.
Run `smp-server start` to start server.
----------
You should store CA private key securely and delete it from the server.
If server TLS credential is compromised this key can be used to sign a new one, keeping the same server identity and established connections.
CA private key location: /etc/opt/simplex/ca.key
----------
SMP server v3.4.0
Fingerprint: d5fcsc7hhtPpexYUbI2XPxDbyU2d3WsVmROimcL90ss=
Server address: smp://d5fcsc7hhtPpexYUbI2XPxDbyU2d3WsVmROimcL90ss=:V8ONoJ6ICwnrZnTC_QuSHfCEYq53uLaJKQ_oIC6-ve8=@<hostnames>
```

上面的服务器地址应在您的客户端配置中使用，如果您添加了服务器密码，则应仅与您希望允许使用您的服务器接收消息的其他人共享（所有联系人都可以发送消息 - 这不需要密码）。如果在初始化期间传递了IP地址或主机名，它们将作为服务器地址的一部分打印，否则请将`<hostnames>`替换为实际的服务器主机名。

## 进一步配置

所有生成的配置以及每个参数的描述都在`/etc/opt/simplex/smp-server.ini`中的配置文件中可用，以供进一步自定义。根据smp-server版本，配置文件看起来如下：

```ini
[INFORMATION]
# AGPLv3许可证要求您向服务器的最终用户提供任何源代码修改。
# 许可证：https://github.com/simplex-chat/simplexmq/blob/stable/LICENSE
# 如果服务器源代码以任何方式修改，请包含正确的源代码URI。
# 如果存在任何其他信息字段，源代码属性也必须存在。

source_code: https://github.com/simplex-chat/simplexmq

# 声明以下所有信息是可选的，可以省略任何这些字段。

# 服务器使用条件和修正。
# 建议使用标准条件，并在单独的文档中包含任何修正。
# usage_conditions: https://github.com/simplex-chat/simplex-chat/blob/stable/PRIVACY.md
# condition_amendments: link

# 服务器位置和运营商。
# server_country: ISO-3166 2-letter code
# operator: entity (organization or person name)
# operator_country: ISO-3166 2-letter code
# website:

# 管理联系人。
# admin_simplex: SimpleX address
# admin_email:
# admin_pgp:
# admin_pgp_fingerprint:

# 投诉和反馈联系人。
# complaints_simplex: SimpleX address
# complaints_email:
# complaints_pgp:
# complaints_pgp_fingerprint:

# 托管提供商。
# hosting: entity (organization or person name)
# hosting_country: ISO-3166 2-letter code

[STORE_LOG]
# 服务器使用STM内存进行持久化，
# 在重启时将丢失（例如，与redis一样）。
# 此选项启用将内存保存到追加日志，
# 并在服务器启动时恢复它。
# 日志在启动时压缩（删除的对象被移除）。
enable: on

# 未发送的消息可以选择保存并在服务器重启时恢复，
# 它们在下次重启之前保存在.bak文件中。
restore_messages: on
expire_messages_days: 21
expire_ntfs_hours: 24

# 记录每日服务器统计信息到CSV文件
log_stats: on

[AUTH]
# 将new_queues选项设置为off以完全禁止创建新的消息队列。
# 这在您想要停用服务器但尚未切换所有连接时非常有用。
new_queues: on

# 使用create_password选项启用基本身份验证以创建新的消息队列。
# 密码应作为服务器地址的一部分在客户端配置中使用：
# smp://fingerprint:password@host1,host2
# 密码不会与连接的联系人共享，您必须仅与希望允许在您的服务器上创建消息队列的用户共享。
# create_password: password to create new queues (any printable ASCII characters without whitespace, '@', ':' and '/')

# control_port_admin_password:
# control_port_user_password:

[TRANSPORT]
# host仅用于在启动时打印服务器地址。
# 您可以指定多个服务器端口。
host: <domain/ip>
port: 5223,443
log_tls_errors: off

# 使用`websockets: 443`在纯TLS之外运行websockets服务器。
websockets: off
# control_port: 5224

[PROXY]
# SMP代理客户端的网络配置。
# `host_mode`可以是'public'（默认）或'onion'。
# 它定义了具有多个主机名的目标服务器的首选主机名。
# host_mode: public
# required_host_mode: off

# 您操作的中继的域名后缀（空格分隔）以计为单独的代理统计信息。
# own_server_domains: 

# 用于将消息转发到目标服务器的SOCKS代理端口。
# 您可能需要单独的SOCKS代理实例以处理传入的单跳请求。
# socks_proxy: localhost:9050

# `socks_mode`可以是'onion'，用于仅用于.onion目标主机的SOCKS代理（默认）
# 或'always'，用于所有目标主机（可以用于.onion服务器）。
# socks_mode: onion

# 限制客户端可以生成的线程数以并行处理代理命令。
# client_concurrency: 32

[INACTIVE_CLIENTS]
# TTL和检查不活动客户端的间隔
disconnect: off
# ttl: 21600
# check_interval: 3600

[WEB]
# 设置路径以生成服务器信息和二维码/链接的静态迷你站点
static_path: /var/opt/simplex/www

# 在此端口上运行嵌入式服务器
# 洋葱站点可以使用任何端口并在隐藏服务配置中注册它。
# 在端口80上运行可能需要设置进程能力。
#http: 8000

# 如果您提供端口和证书和密钥文件，您也可以运行嵌入式TLS Web服务器。
# 不需要运行中继在洋葱地址。
https: 443
cert: /etc/opt/simplex/web.crt
key: /etc/opt/simplex/web.key
```

## 服务器安全

### 初始化

虽然在服务器上直接初始化smp-server配置很方便，但建议操作员**离线**初始化smp-server以保护您的SMP服务器CA私钥。

按照以下步骤快速离线初始化服务器：

1. 在您的系统上安装Docker。

2. 本地部署[smp-server](https://github.com/simplex-chat/simplexmq#using-docker)。

3. 销毁容器。所有相关的配置文件和密钥将位于`$HOME/simplex/smp/config`。

4. 将您的`CA`私钥（`ca.key`）移动到安全的地方。有关进一步说明，请参阅下一节：[服务器安全：私钥](#private-keys)。

5. 将所有其他配置文件**除**CA密钥外复制到服务器：

   ```sh
   rsync -hzasP $HOME/simplex/smp/config/ <server_user>@<server_address>:/etc/opt/simplex/
   ```

### 私钥

连接到smp服务器是通过TLS连接进行的。在TLS握手期间，客户端通过将其指纹与服务器地址中包含的指纹进行比较来验证smp-server CA和服务器证书。如果服务器TLS凭证被泄露，此密钥可以用于签署新的凭证，保持相同的服务器身份和已建立的连接。为了保护您的smp-server免受恶意行为者的攻击，建议操作员**将CA私钥移动到安全的地方**。这可以是：

- 带有[持久和加密存储](https://tails.net/doc/persistent_storage/create/index.en.html)的[Tails](https://tails.net/) live USB驱动器。
- 离线Linux笔记本电脑。
- Bitwarden。
- 满足您的安全要求的任何其他安全存储。

按照以下步骤保护您的CA密钥：

1. 通过SSH登录到您的服务器。

2. 将CA密钥从以下文件复制到安全的地方：

   ```sh
   /etc/opt/simplex/ca.key
   ```

3. 从服务器删除CA密钥。**请确保您已将CA密钥安全保存。否则，您将失去[轮换在线证书](#online-certificate-rotation)的能力**：

   ```sh
   rm /etc/opt/simplex/ca.key
   ```

### 在线证书轮换

建议smp服务器的操作员定期轮换在线证书（例如，每3个月）。为此，请按照以下步骤操作：

1. 创建相关文件夹：

   ```sh
   mkdir -p $HOME/simplex/smp/config
   ```

1. 将配置文件从服务器复制到本地计算机（如果尚未复制）：

   ```sh
   rsync -hzasP <server_user>@<server_address>:/etc/opt/simplex/ $HOME/simplex/smp/config/
   ```

2. **复制**您的CA私钥从安全的地方到本地计算机，并将其命名为`ca.key`。

3. 从Github发布下载最新的`smp-server`二进制文件：

   ```sh
   curl -L 'https://github.com/simplex-chat/simplexmq/releases/latest/download/smp-server-ubuntu-20_04-x86-64' -o smp-server
   ```

4. 将`smp-server`二进制文件放入您的`$PATH`并使其可执行：

   ```sh
   sudo mv smp-server /usr/local/bin/ && chmod +x /usr/local/bin/smp-server
   ```

5. 导出变量以配置您的smp-server配置路径：

   ```sh
   export SMP_SERVER_CFG_PATH=$HOME/simplex/smp/config
   ```

6. 执行以下命令：

   ```sh
   smp-server cert
   ```

   此命令应打印：

   ```sh
   Certificate request self-signature ok
   subject=CN = <your domain or IP>
   Generated new server credentials
   ----------
   You should store CA private key securely and delete it from the server.
   If server TLS credential is compromised this key can be used to sign a new one, keeping the same server identity and established connections.
   CA private key location:
   $HOME/simplex/smp/config/ca.key
   ----------
   ```

7. 从配置文件夹中删除CA密钥（确保您有备份！）：

   ```sh
   rm $HOME/simplex/smp/config/ca.key
   ```

8. 将新证书上传到服务器：

   ```sh
   rsync -hzasP $HOME/simplex/smp/config/ <server_user>@<server_address>:/etc/opt/simplex/
   ```

9. 通过SSH连接到服务器并重启服务：

   ```sh
   ssh <server_user>@<server_address> "systemctl restart smp-server"
   ```

10. 完成！

## Tor：安装与配置

### 为洋葱地址安装

SMP服务器也可以部署为通过[Tor](https://www.torproject.org)网络可用。以`root`用户身份运行以下命令。

1. 安装Tor：

   我们假设您使用的是基于Ubuntu/Debian的发行版。如果不是，请参考[官方Tor文档](https://community.torproject.org/onion-services/setup/install/)或您的发行版指南。

   - 配置官方Tor PPA存储库：

     ```sh
     CODENAME="$(lsb_release -c | awk '{print $2}')"
     echo "deb [signed-by=/usr/share/keyrings/tor-archive-keyring.gpg] https://deb.torproject.org/torproject.org ${CODENAME} main
     deb-src [signed-by=/usr/share/keyrings/tor-archive-keyring.gpg] https://deb.torproject.org/torproject.org ${CODENAME} main" > /etc/apt/sources.list.d/tor.list
     ```

   - 导入存储库密钥：

     ```sh
     curl --proto '=https' --tlsv1.2 -sSf https://deb.torproject.org/torproject.org/A3C4F0F979CAA22CDBA8F512EE8CBC9E886DDD89.asc | gpg --dearmor | tee /usr/share/keyrings/tor-archive-keyring.gpg >/dev/null
     ```

   - 更新存储库索引：

     ```sh
     apt update
     ```

   - 安装`tor`包：

     ```sh
     apt install -y tor deb.torproject.org-keyring
     ```

2. 配置Tor：

   - 文件配置：
  
     使用您选择的编辑器（`nano`，`vim`，`emacs`等）打开Tor配置：

     ```sh
     vim /etc/tor/torrc
     ```

     并将以下行插入配置底部。请注意以`#`开头的行：这是关于每个选项的注释。

     ```sh
     # 启用日志（否则，Tor似乎不会部署洋葱地址）
     Log notice file /var/log/tor/notices.log
     # 启用单跳路由（以下两个选项是第三个选项的依赖项） - 它将以较低的匿名性为代价减少延迟 - 由于SMP服务器洋葱地址与公共地址一起在客户端中使用，这没问题。如果您仅部署洋葱地址的SMP服务器，请保持标准配置。
     SOCKSPort 0
     HiddenServiceNonAnonymousMode 1
     HiddenServiceSingleHopMode 1
     # SMP服务器隐藏服务主机目录和端口映射
     HiddenServiceDir /var/lib/tor/simplex-smp/
     HiddenServicePort 5223 localhost:5223
     HiddenServicePort 443 localhost:443
     ```

   - 创建目录：

     ```sh
     mkdir /var/lib/tor/simplex-smp/ && chown debian-tor:debian-tor /var/lib/tor/simplex-smp/ && chmod 700 /var/lib/tor/simplex-smp/
     ```

3. 启动Tor：

   启用`systemd`服务并启动Tor。官方`tor`在首次启动时有点不稳定，可能不会创建洋葱主机地址，因此我们重新启动它以防万一。

   ```sh
   systemctl enable --now tor && systemctl restart tor
   ```

4. 显示洋葱主机：

   执行以下命令以显示您的洋葱主机地址：

   ```sh
   cat /var/lib/tor/simplex-smp/hostname
   ```

### SMP代理的SOCKS端口

从`smp-server`版本`v5.8.0-beta.0`开始，可以配置SMP代理以通过[Tor](https://www.torproject.org)网络访问仅通过Tor网络可用的smp服务器，以便不使用Tor的客户端可以访问。以`root`用户身份运行以下命令。

1. 按照[上一节](#installation-for-onion-address)中的说明安装Tor。

2. 执行以下命令以创建新的Tor守护进程实例：

   ```sh
   tor-instance-create tor2
   ```

3. 打开`tor2`配置并将其内容替换为以下行：

   ```sh
   vim /etc/tor/instances/tor2/torrc
   ```

   ```sh
   # 将Tor日志记录到systemd守护进程
   Log notice syslog
   # 监听本地9050端口以用于SOCKS代理
   SocksPort 9050
   ```

3. 启用服务并启动守护进程：

   ```sh
   systemctl enable --now tor@tor2
   ```

   您可以使用以下命令检查`tor2`日志：

   ```sh
   journalctl -u tor@tor2
   ```

4. 在[服务器初始化](#configuration)后，配置`PROXY`部分如下：

   ```ini
   ...
   [PROXY]
   socks_proxy: 127.0.0.1:9050
   own_server_domains: <your domain suffixes if using `log_stats: on`>
   ...
   ```

## 服务器信息页面

SMP服务器**应**配置为提供包含管理员信息、服务器信息、提供商信息等的网页。它还将提供使用移动/桌面应用程序生成的连接链接。以`root`用户身份运行以下命令。

_请注意_：此配置从`v6.1.0-beta.2`开始支持。

1. 将以下内容添加到您的smp-server配置中（请修改[INFORMATION]部分中的字段以包含相关信息）：

   ```sh
   vim /etc/opt/simplex/smp-server.ini
   ```

   ```ini
   [TRANSPORT]
   # host仅用于在启动时打印服务器地址
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
   # AGPLv3许可证要求您向服务器的最终用户提供任何源代码修改。
   # 许可证：https://github.com/simplex-chat/simplexmq/blob/stable/LICENSE
   # 如果服务器源代码以任何方式修改，请包含正确的源代码URI。
   # 如果存在任何其他信息字段，源代码属性也必须存在。

   source_code: https://github.com/simplex-chat/simplexmq

   # 声明以下所有信息是可选的，可以省略任何这些字段。

   # 服务器使用条件和修正。
   # 建议使用标准条件，并在单独的文档中包含任何修正。
   # usage_conditions: https://github.com/simplex-chat/simplex-chat/blob/stable/PRIVACY.md
   # condition_amendments: link

   # 服务器位置和运营商。
   server_country: <YOUR_SERVER_LOCATION>
   operator: <YOUR_NAME>
   operator_country: <YOUR_LOCATION>
   website: <WEBSITE_IF_AVAILABLE>
  
   # 管理联系人。
   #admin_simplex: SimpleX address
   admin_email: <EMAIL>
   # admin_pgp:
   # admin_pgp_fingerprint:

   # 投诉和反馈联系人。
   # complaints_simplex: SimpleX address
   complaints_email: <COMPLAINTS_EMAIL>
   # complaints_pgp:
   # complaints_pgp_fingerprint:

   # 托管提供商。
   hosting: <HOSTING_PROVIDER_NAME>
   hosting_country: <HOSTING_PROVIDER_LOCATION> 
   ```

2. 安装Web服务器。为了便于部署，我们将描述在Ubuntu服务器上安装[Caddy](https://caddyserver.com) Web服务器的过程：

   1. 安装软件包：

      ```sh
      sudo apt install -y debian-keyring debian-archive-keyring apt-transport-https curl
      ```

   2. 为存储库安装Caddy gpg密钥：

      ```sh
      curl -1sLf 'https://dl.cloudsmith.io/public/caddy/stable/gpg.key' | sudo gpg --dearmor -o /usr/share/keyrings/caddy-stable-archive-keyring.gpg
      ```

   3. 安装Caddy存储库：

      ```sh
      curl -1sLf 'https://dl.cloudsmith.io/public/caddy/stable/debian.deb.txt' | sudo tee /etc/apt/sources.list.d/caddy-stable.list
      ```

   4. 安装Caddy：

      ```sh
      sudo apt update && sudo apt install caddy
      ```

   [完整的Caddy安装说明](https://caddyserver.com/docs/install)

3. 将Caddy配置替换为以下内容：

   请将`YOUR_DOMAIN`替换为您的实际域名（例如smp.example.com）。

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

4. 启用并启动Caddy服务：

   ```sh
   systemctl enable --now caddy
   ```

5. 创建脚本以将证书复制到您的smp目录：

   请将`YOUR_DOMAIN`替换为您的实际域名（例如smp.example.com）。

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

   # 从Caddy目录复制证书到SMP服务器目录
   cp "${folder_in}/${domain}.crt" "${folder_out}/${cert_name}"
   # 分配正确的权限
   chown "$user":"$group" "${folder_out}/${cert_name}"

   # 从Caddy目录复制证书密钥到SMP服务器目录
   cp "${folder_in}/${domain}.key" "${folder_out}/${key_name}"
   # 分配正确的权限
   chown "$user":"$group" "${folder_out}/${key_name}"
   ```

6. 使脚本可执行并执行它：

   ```sh
   chmod +x /usr/local/bin/simplex-servers-certs && /usr/local/bin/simplex-servers-certs
   ```

7. 检查证书是否已复制：

   ```sh
   ls -haltr /etc/opt/simplex/web*
   ```

8. 创建cron作业以定期将证书复制到smp目录：

   ```sh
   sudo crontab -e
   ```

   ```sh
   # 每周日00:20
   20 0 * * 0 /usr/local/bin/simplex-servers-certs
   ```

9. 然后：

   - 如果您运行的是至少`v6.1.0-beta.2`，请[重启服务器](#systemd-commands)。
   - 如果您运行的是低于`v6.1.0-beta.2`，请[升级服务器](#updating-your-smp-server)。

10. 从浏览器访问您部署的网页（例如`https://smp.example.org`）。您应该会看到在ini文件中提供的smp-server信息。

## 文档

所有必要的`smp-server`文件都位于`/etc/opt/simplex/`文件夹中。

存储的消息、连接、统计信息和服务器日志位于`/var/opt/simplex/`文件夹中。

### SMP服务器地址

SMP服务器地址具有以下格式：

```
smp://<fingerprint>[:<password>]@<public_hostname>[,<onion_hostname>]
```

- `<fingerprint>`

  您的`smp-server`证书指纹。您可以在`/etc/opt/simplex/fingerprint`中检查您的证书指纹。

- **可选** `<password>`

  您配置的`smp-server`密码。您可以在`/etc/opt/simplex/smp-server.ini`中的`[AUTH]`部分的`create_password:`字段中检查您的配置密码。

- `<public_hostname>`，**可选** `<onion_hostname>`

  您配置的`smp-server`主机名。您可以在`/etc/opt/simplex/smp-server.ini`中的`[TRANSPORT]`部分的`host:`字段中检查您的配置主机名。

### Systemd命令

要在主机启动时启动`smp-server`，请运行：

```sh
sudo systemctl enable smp-server.service

Created symlink /etc/systemd/system/multi-user.target.wants/smp-server.service → /etc/systemd/system/smp-server.service.
```

要启动`smp-server`，请运行：

```sh
sudo systemctl start smp-server.service
```

要检查`smp-server`的状态，请运行：

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

要停止`smp-server`，请运行：

```sh
sudo systemctl stop smp-server.service
```

要检查`smp-server`日志的尾部，请运行：

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

### 控制端口

在配置中启用控制端口允许管理员实时查看smp-server的信息。此外，它允许删除队列以进行内容审核，并查看有关客户端、套接字等的调试信息。启用控制端口需要设置`admin`和`user`密码。

1. 为每个用户生成两个密码：

   ```sh
   tr -dc A-Za-z0-9 </dev/urandom | head -c 20; echo
   ```

2. 打开配置文件：

   ```sh
   vim /etc/opt/simplex/smp-server.ini
   ```

2. 配置控制端口并替换密码：

   ```ini
   [AUTH]
   control_port_admin_password: <your_randomly_generated_admin_password>
   control_port_user_password: <your_randomly_generated_user_password>

   [TRANSPORT]
   control_port: 5224
   ```

3. 重启服务器：

   ```sh
   systemctl restart smp-server
   ```

要访问控制端口，请使用：

```sh
nc 127.0.0.1 5224
```

或：

```sh
telnet 127.0.0.1 5224
```

连接后，控制端口应打印：

```sh
SMP server control port
'help' for supported commands
```

要进行身份验证，请键入以下内容并按回车键。将`my_generated_password`更改为配置中的`user`或`admin`密码：

```sh
auth my_generated_password
```

以下是命令的完整列表、其描述以及谁可以访问它们。

| 命令          | 描述                                                                     | 需要`admin`角色      |
| ---------------- | ------------------------------------------------------------------------------- | -------------------------- |
| `stats`          | 实时统计信息。字段在[每日统计](#daily-statistics)中描述 | -                          |
| `stats-rts`      | GHC/Haskell统计信息。可以使用`+RTS -T -RTS`选项启用               | -                          |
| `clients`        | 客户端信息。用于调试。                                      | yes                        |
| `sockets`        | 常规套接字信息。                                                    | -                          |
| `socket-threads` | 每个套接字的线程信息。用于调试。                             | yes                        |
| `threads`        | 线程信息。用于调试。                                      | yes                        |
| `server-info`    | 聚合的服务器信息。                                                   | -                          |
| `delete`         | 删除已知队列。用于内容审核。                              | -                          |
| `save`           | 从内存中保存队列/消息。                                               | yes                        |
| `help`           | 帮助菜单。                                                                      | -                          |
| `quit`           | 退出控制端口。                                                          | -                          |

### 每日统计

您可以通过在`/etc/opt/simplex/smp-server.ini`中的`[STORE_LOG]`部分的`log_stats:`字段中设置值`on`来启用`smp-server`统计信息以用于`Grafana`仪表板。

日志将以`csv`文件的形式存储在`/var/opt/simplex/smp-server-stats.daily.log`中。`csv`文件的字段如下：

```sh
fromTime,qCreated,qSecured,qDeleted,msgSent,msgRecv,dayMsgQueues,weekMsgQueues,monthMsgQueues,msgSentNtf,msgRecvNtf,dayCountNtf,weekCountNtf,monthCountNtf,qCount,msgCount,msgExpired,qDeletedNew,qDeletedSecured,pRelays_pRequests,pRelays_pSuccesses,pRelays_pErrorsConnect,pRelays_pErrorsCompat,pRelays_pErrorsOther,pRelaysOwn_pRequests,pRelaysOwn_pSuccesses,pRelaysOwn_pErrorsConnect,pRelaysOwn_pErrorsCompat,pRelaysOwn_pErrorsOther,pMsgFwds_pRequests,pMsgFwds_pSuccesses,pMsgFwds_pErrorsConnect,pMsgFwds_pErrorsCompat,pMsgFwds_pErrorsOther,pMsgFwdsOwn_pRequests,pMsgFwdsOwn_pSuccesses,pMsgFwdsOwn_pErrorsConnect,pMsgFwdsOwn_pErrorsCompat,pMsgFwdsOwn_pErrorsOther,pMsgFwdsRecv,qSub,qSubAuth,qSubDuplicate,qSubProhibited,msgSentAuth,msgSentQuota,msgSentLarge,msgNtfs,msgNtfNoSub,msgNtfLost,qSubNoMsg,msgRecvGet,msgGet,msgGetNoMsg,msgGetAuth,msgGetDuplicate,msgGetProhibited,psSubDaily,psSubWeekly,psSubMonthly,qCount2,ntfCreated,ntfDeleted,ntfSub,ntfSubAuth,ntfSubDuplicate,ntfCount,qDeletedAllB,qSubAllB,qSubEnd,qSubEndB,ntfDeletedB,ntfSubB,msgNtfsB,msgNtfExpired
```

**字段描述**

| 字段编号  | 字段名称                   | 字段描述          |
| ------------- | ---------------------------- | -------------------------- |
| 1             | `fromTime`                   | 统计日期         |
| 消息队列:                                                          |
| 2             | `qCreated`                   | 创建                    |
| 3             | `qSecured`                   | 建立                |
| 4             | `qDeleted`                   | 删除                    |
| 消息:                                                                 |
| 5             | `msgSent`                    | 发送                       |
| 6             | `msgRecv`                    | 接收                   |
| 7             | `dayMsgQueues`               | 一天内的活动队列     |
| 8             | `weekMsgQueues`              | 一周内的活动队列    |
| 9             | `monthMsgQueues`             | 一个月内的活动队列   |
| 带有“通知”标志的消息                                         |
| 10            | `msgSentNtf`                 | 发送                       |
| 11            | `msgRecvNtf`                 | 接收                   |
| 12            | `dayCountNtf`                | 一天内的活动队列     |
| 13            | `weekCountNtf`               | 一周内的活动队列    |
| 14            | `monthCountNtf`              | 一个月内的活动队列   |
| 其他统计信息:                                                    |
| 15            | `qCount`                     | 存储的队列              |
| 16            | `msgCount`                   | 存储的消息            |
| 17            | `msgExpired`                 | 过期的消息           |
| 18            | `qDeletedNew`                | 新删除的队列         |
| 19            | `qDeletedSecured`            | 已建立的删除队列     |
| 请求与所有中继的会话:                                       |
| 20            | `pRelays_pRequests`          | - 请求                 |
| 21            | `pRelays_pSuccesses`         | - 成功                |
| 22            | `pRelays_pErrorsConnect`     | - 连接错误        |
| 23            | `pRelays_pErrorsCompat`      | - 兼容性错误     |
| 24            | `pRelays_pErrorsOther`       | - 其他错误             |
| 请求与自己的中继的会话:                                       |
| 25            | `pRelaysOwn_pRequests`       | - 请求                 |
| 26            | `pRelaysOwn_pSuccesses`      | - 成功                |
| 27            | `pRelaysOwn_pErrorsConnect`  | - 连接错误        |
| 28            | `pRelaysOwn_pErrorsCompat`   | - 兼容性错误     |
| 29            | `pRelaysOwn_pErrorsOther`    | - 其他错误             |
| 消息转发到所有中继:                                           |
| 30            | `pMsgFwds_pRequests`         | - 请求                 |
| 31            | `pMsgFwds_pSuccesses`        | - 成功                |
| 32            | `pMsgFwds_pErrorsConnect`    | - 连接错误        |
| 33            | `pMsgFwds_pErrorsCompat`     | - 兼容性错误     |
| 34            | `pMsgFwds_pErrorsOther`      | - 其他错误             |
| 消息转发到自己的中继:                                            |
| 35            | `pMsgFwdsOwn_pRequests`      | - 请求                 |
| 36            | `pMsgFwdsOwn_pSuccesses`     | - 成功                |
| 37            | `pMsgFwdsOwn_pErrorsConnect` | - 连接错误        |
| 38            | `pMsgFwdsOwn_pErrorsCompat`  | - 兼容性错误     |
| 39            | `pMsgFwdsOwn_pErrorsOther`   | - 其他错误             |
| 接收的消息转发:                                                |
| 40            | `pMsgFwdsRecv`               |                            |
| 消息队列订阅错误:                                        |
| 41            | `qSub`                       | 全部                        |
| 42            | `qSubAuth`                   | 身份验证错误      |
| 43            | `qSubDuplicate`              | 重复订阅错误       |
| 44            | `qSubProhibited`             | 禁止订阅错误      |
| 消息错误:                                                           |
| 45            | `msgSentAuth`                | 身份验证错误      |
| 46            | `msgSentQuota`               | 配额错误               |
| 47            | `msgSentLarge`               | 大消息错误       |
| 48            | `msgNtfs`                    | XXXXXXXXXXXXXXXXXXXX       |
| 49            | `msgNtfNoSub`                | XXXXXXXXXXXXXXXXXXXX       |
| 50            | `msgNtfLost`                 | XXXXXXXXXXXXXXXXXXXX       |
| 51            | `qSubNoMsg`                  | 已删除，总是0          |
| 52            | `msgRecvGet`                 | XXXXXXXXXXXXXXXXX          |
| 53            | `msgGet`                     | XXXXXXXXXXXXXXXXX          |
| 54            | `msgGetNoMsg`                | XXXXXXXXXXXXXXXXX          |
| 55            | `msgGetAuth`                 | XXXXXXXXXXXXXXXXX          |
| 56            | `msgGetDuplicate`            | XXXXXXXXXXXXXXXXX          |
| 57            | `msgGetProhibited`           | XXXXXXXXXXXXXXXXX          |
| 58            | `psSub_dayCount`             | 已删除，总是0          |
| 59            | `psSub_weekCount`            | 已删除，总是0          |
| 60            | `psSub_monthCount`           | 已删除，总是0          |
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

要将`csv`导入`Grafana`，应：

1. 安装Grafana插件：[Grafana - CSV数据源](https://grafana.com/grafana/plugins/marcusolsson-csv-datasource/)

2. 通过在以下位置附加以下内容来允许本地模式：

   ```sh
   [plugin.marcusolsson-csv-datasource]
   allow_local_mode = true
   ```

   ...到`/etc/grafana/grafana.ini`

3. 添加CSV数据源：

   - 在侧边菜单中，单击配置选项卡（齿轮图标）
   - 单击数据源选项卡右上角的添加数据源
   - 在搜索框中输入“CSV”以找到CSV数据源
   - 单击显示“CSV”的搜索结果
   - 在URL中，输入指向CSV内容的文件

4. 完成！您应该能够创建自己的统计信息仪表板。

有关更多文档，请参阅：[CSV数据源的Grafana - 文档](https://grafana.github.io/grafana-csv-datasource/)

## 更新您的SMP服务器

要将您的smp-server更新到最新版本，请选择您的安装方法并按照以下步骤操作：

   - 手动部署

     1. 停止服务器：

        ```sh
        sudo systemctl stop smp-server
        ```

     2. 更新二进制文件：

        ```sh
         curl -L https://github.com/simplex-chat/simplexmq/releases/latest/download/smp-server-ubuntu-20_04-x86-64 -o /usr/local/bin/smp-server && chmod +x /usr/local/bin/smp-server
        ```

     3. 启动服务器：

        ```sh
        sudo systemctl start smp-server
        ```

   - [官方安装脚本](https://github.com/simplex-chat/simplexmq#using-installation-script)

     1. 执行以下命令：

        ```sh
        sudo simplex-servers-update
        ```

        要安装特定版本，请运行：

        ```sh
        export VER=<version_from_github_releases> &&\
        sudo -E simplex-servers-update
        ```

     2. 完成！

   - [Docker容器](https://github.com/simplex-chat/simplexmq#using-docker)

     1. 停止并删除容器：

        ```sh
        docker rm $(docker stop $(docker ps -a -q --filter ancestor=simplexchat/smp-server --format="\{\{.ID\}\}"))
        ```

     2. 拉取最新镜像：

        ```sh
        docker pull simplexchat/smp-server:latest
        ```

     3. 启动新容器：

        ```sh
        docker run -d \
          -p 5223:5223 \
          -p 443:443 \
          -v $HOME/simplex/smp/config:/etc/opt/simplex:z \
          -v $HOME/simplex/smp/logs:/var/opt/simplex:z \
          simplexchat/smp-server:latest
        ```

   - [Linode Marketplace](https://www.linode.com/marketplace/apps/simplex-chat/simplex-chat/)

     1. 拉取最新镜像：

        ```sh
        docker-compose --project-directory /etc/docker/compose/simplex pull
        ```

     2. 重启容器：

        ```sh
        docker-compose --project-directory /etc/docker/compose/simplex up -d --remove-orphans
        ```

     3. 删除过时的镜像：

        ```sh
        docker image prune
        ```

## 重现构建

您可以按照以下说明在本地重现服务器二进制文件。

您必须具备：

- Linux机器
- `x86-64`架构
- 安装了`docker`，`curl`和`git`

1. 下载脚本：

   ```sh
   curl -LO 'https://raw.githubusercontent.com/simplex-chat/simplexmq/refs/heads/master/scripts/reproduce-builds.sh'
   ```

2. 使其可执行：

   ```sh
   chmod +x reproduce-builds.sh
   ```

3. 使用所需标签执行脚本：

   ```sh
   ./reproduce-builds.sh 'v6.3.0'
   ```

   这将需要一段时间。

4. 编译后，您应该会看到以下文件夹：

   ```sh
   ls out*
   ```

   ```sh
   out-20.04:
   ntf-server  smp-server  xftp  xftp-server

   out-20.04-github:
   ntf-server  smp-server  xftp  xftp-server

   out-22.04:
   ntf-server  smp-server  xftp  xftp-server

   out-22.04-github:
   ntf-server  smp-server  xftp  xftp-server

   out-24.04:
   ntf-server  smp-server  xftp  xftp-server

   out-24.04-github:
   ntf-server  smp-server  xftp  xftp-server
   ```

5. 将github发布的哈希值与本地构建的二进制文件进行比较：

   ```sh
   sha256sum out*-github/*
   ```

   ```sh
   sha256sum out*[0-9]/*
   ```

   您可以安全地删除克隆的存储库：

   ```sh
   cd ../ && rm -rf simplexmq
   ```

## 配置应用以使用服务器

要配置应用以使用您的消息服务器，请复制其完整地址（包括密码），并将其添加到应用中。您可以选择与预设服务器一起使用您的服务器或不使用它们 - 您可以删除或禁用它们。

您还可以通过让朋友扫描服务器设置中的二维码来共享您的服务器地址 - 它将包含服务器密码，因此他们也可以通过您的服务器接收消息。

_请注意_：您需要SMP服务器版本4.0才能支持密码。如果您已经部署了服务器，可以通过将密码添加到服务器INI文件中来添加密码。

<img src="./server_config_1.png" width="288"> &nbsp;&nbsp; <img src="./server_config_2.png" width="288"> &nbsp;&nbsp; <img src="./server_config_3.png" width="288">
