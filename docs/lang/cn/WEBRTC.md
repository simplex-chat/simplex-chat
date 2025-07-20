---
title: 在 SimpleX 中使用定制 WebRTC ICE 服务器
revision: 31.01.2023
---

| 更新日期: 31.01.2023 | 语言: ZH_CN, EN, [FR](/docs/lang/fr/WEBRTC.md), [CZ](/docs/lang/cs/WEBRTC.md), [波兰文](/docs/lang/pl/WEBRTC.md) |

# 在 SimpleX 中使用定制 WebRTC ICE 服务器
## 部署 STUN/TURN 服务器

For this guide, we'll be using the most featureful and battle-tested STUN/TURN server implementation 在本指南中，我们将使用特性最完善且经过实战测试的 STUN/TURN 服务器 – [`coturn`](https://github.com/coturn/coturn) 以及 [`Ubuntu 20.04 LTS`](https://ubuntu.com/download/server) Linux 发行版

0. 获取 `stun.$YOUR_DOMAIN` 和 `turn.$YOUR_DOMAIN` 的证书.

   我们在此使用 [Let's Encrypt](https://letsencrypt.org/getting-started/).

1. 从主仓库安装 `coturn` 发行包

```sh
apt update && apt install coturn`
```

2. 取消 `/etc/default/coturn` 中的 `TURNSERVER_ENABLED=1` 注释：

```sh
sed -i '/TURN/s/^#//g' /etc/default/coturn
```

3. 在 `/etc/turnserver.conf`中配置 `conturn`：

   另外，请参阅每个选项的注释。

```sh
# Also listen to 443 port for tls
alt-tls-listening-port=443
# Use fingerprints in the TURN messages
fingerprint
# Use long-term credentials mechanism
lt-cred-mech
# Your credentials
user=$YOUR_LOGIN:$YOUR_PASSWORD
# Your server domain
server-name=$YOUR_DOMAIN
# The default realm to be used for the users when no explicit origin/realm relationship was found
realm=$YOUR_DOMAIN
# Path to your certificates. Make sure they're readable by cotun process user/group
cert=/var/lib/turn/cert.pem
pkey=/var/lib/turn/key.pem
# Use 2066 bits predefined DH TLS key
dh2066
# Log to journalctl
syslog
# User/group which will be running coturn service
proc-user=turnserver
proc-group=turnserver
# Disable weak encryption
no-tlsv1
no-tlsv1_1
no-tlsv1_2
```

4. 运行并启用 `coturn` 服务：

```sh
systemctl enable coturn && systemctl start coturn
```

5. 可选, 如果你在使用 `ufw` 作为防火墙，请放行以下端口：

- **3478** – "透明的" TURN/STUN;
- **5349** – TURN/STUN over TLS;
- **443** – TURN/STUN over TLS, 可绕过防火墙;
- **49152:65535** – 正常情况下 Coturn 将用于 TURN 的端口范围

```sh
# For Ubuntu
sudo ufw allow 3478 && \
sudo ufw allow 443 && \
sudo ufw allow 5349 && \
sudo ufw allow 49152:65535/tcp && \
sudo ufw allow 49152:65535/udp

# For Fedora
sudo firewall-cmd --permanent --add-port=443/tcp && \
sudo firewall-cmd --permanent --add-port=443/udp && \
sudo firewall-cmd --permanent --add-port=5349/tcp && \
sudo firewall-cmd --permanent --add-port=5349/udp && \
sudo firewall-cmd --permanent --add-port=49152:65535/tcp && \
sudo firewall-cmd --permanent --add-port=49152:65535/udp && \
sudo firewall-cmd --reload
```

## 配置移动设备

为了让移动设备使用你的服务器，请这么做：

1. 打开 `设置 / 网络和服务器 / WebRTC ICE 服务器` and switch toggle `配置 ICE servers`.

2. 在输入框中输入你所有的服务器地址，一行一个。例如，你的服务器都运行在 5349 端口上：

```
stun:stun.example.com:5349
turn:username:password@turn.example.com:5349
```

完事 - 你现在可以通过自己的服务器打语音或视频通话，不必与我们的服务器交换数据（E2E加密聊天中与联系人的密钥交换除外）。

## 疑难解答：

- **确定服务器是否可用**:

  在你的终端运行这个命令：

  ```sh
  ping <your_ip_or_domain>
  ```

  如果数据包正在传输，服务器一切OK。

- **确定端口是否开放**:

  在你的终端运行这个命令：

  ```sh
  nc -zvw10 <your_ip_or_domain> 443 5349
  ```

  你应该看见：

  ```
  Connection to <your_ip_or_domain> 443 port [tcp/https] succeeded!
  Connection to <your_ip_or_domain> 5349 port [tcp/*] succeeded!
  ```

- **测试 STUN/TURN 连通性**:

  1. 前往 [IceTest](https://icetest.info/).

  2. 在 **Build up ICE Server List** 选项中，添加这个：

     <img src="../../images/stun_1.png">

     - `STUN: stun:<your_ip_or_domain>:<port>` 然后点击 `Add STUN`
     - `TURN: turn:<your_ip_or_domain>:<port>`, `Username: <your_login>`, `Credential: <your_pass>` 然后点击 `Add TURN`

     在 `<port>` 中通常应该是 443 或 5349.

  3. 你应该看到你的服务器在 **ICE server list** 中。 如果万事大吉，点击 `Start test`：

     <img src="../../images/stun_2.png">

  4. 在 **Results** 中, 你应该看到类似这样的内容：

     <img src="../../images/stun_3.png">

     如果结果显示 `srflx` 和 `relay` 候选词，则一切正常！

