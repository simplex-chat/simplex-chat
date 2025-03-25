---
title: 终端 CLI
revision: 31.01.2023
---

| 更新于 31.01.2023 | 语言: EN, [FR](/docs/lang/fr/CLI.md), [CZ](/docs/lang/cs/CLI.md), [PL](/docs/lang/pl/CLI.md) |

# 适用于 Linux/MacOS/Windows 的 SimpleX Chat 终端（控制台）应用程序

## 目录

- [终端聊天功能](#终端聊天功能)
- [安装](#🚀-安装)
  - [下载聊天客户端](#下载聊天客户端)
    - [Linux 和 MacOS](#linux-和-macos)
    - [Windows](#windows)
  - [从源代码构建](#从源代码构建)
    - [使用 Docker](#使用-docker)
    - [在任何操作系统中使用 Haskell](#在任何操作系统中使用-haskell)
- [使用](#使用)
  - [运行聊天客户端](#运行聊天客户端)
  - [通过 Tor 访问消息服务器](#通过-tor-访问消息服务器-测试版)
  - [如何使用 SimpleX 聊天](#如何使用-simplex-聊天)
  - [群组](#群组)
  - [发送文件](#发送文件)
  - [用户联系地址](#用户联系地址)
  - [访问聊天记录](#访问聊天记录)

## 终端聊天功能

- 在同一个终端窗口中与多个人进行一对一聊天。
- 群组消息。
- 向联系人和群组发送文件。
- 用户联系地址 - 通过多次使用的联系链接建立连接。
- 消息保存在本地 SQLite 数据库中。
- 自动填充收件人姓名 - 一旦建立连接，只需输入消息即可回复发送者。
- 提供预配置的演示 SMP 服务器 - 或者您可以[部署自己的服务器](https://github.com/simplex-chat/simplexmq#using-smp-server-and-smp-agent)。
- 没有全局身份或任何服务器可见的名称，确保您的联系人和对话的完全隐私。
- 两层 E2E 加密（双棘轮用于双工连接，使用 X3DH 密钥协议和临时 Curve448 密钥，以及 NaCl crypto_box 用于 SMP 队列，使用 Curve25519 密钥）和带外传递接收者密钥（参见[如何使用 SimpleX 聊天](#如何使用-simplex-聊天)）。
- 消息完整性验证（通过包含前一条消息的摘要）。
- SMP 服务器通过自动生成的 Ed448 密钥对每个命令/消息进行身份验证。
- TLS 1.3 传输加密。
- 从 SMP 服务器到接收者的消息的额外加密，以减少流量关联。

密钥交换中涉及的公钥不作为身份使用，它们是为每个联系人随机生成的。

有关技术细节，请参见[使用的加密原语](https://github.com/simplex-chat/simplexmq/blob/master/protocol/overview-tjr.md#encryption-primitives-used)。

<a name="🚀-安装"></a>

## 🚀 安装

### 下载聊天客户端

#### Linux 和 MacOS

要**安装**或**更新**`simplex-chat`，您应该运行安装脚本。为此，请使用以下 cURL 或 Wget 命令：

```sh
curl -o- https://raw.githubusercontent.com/simplex-chat/simplex-chat/stable/install.sh | bash
```

```sh
wget -qO- https://raw.githubusercontent.com/simplex-chat/simplex-chat/stable/install.sh | bash
```

下载聊天客户端后，您可以在终端中使用 `simplex-chat` 命令运行它。

或者，您可以从[最新稳定版本](https://github.com/simplex-chat/simplex-chat/releases)手动下载适用于您的系统的聊天二进制文件，并按如下所示使其可执行。

```sh
chmod +x <binary>
mv <binary> ~/.local/bin/simplex-chat
```

（或 `PATH` 上的任何其他首选位置）。

在 MacOS 上，您还需要[允许 Gatekeeper 运行它](https://support.apple.com/en-us/HT202491)。

#### Windows

```sh
move <binary> %APPDATA%/local/bin/simplex-chat.exe
```

### 从源代码构建

> **请注意：** 要构建应用程序，请使用[稳定分支](https://github.com/simplex-chat/simplex-chat/tree/stable)的源代码。

#### 使用 Docker

在 Linux 上，您可以使用[带有自定义输出的 docker build](https://docs.docker.com/engine/reference/commandline/build/#custom-build-outputs)构建聊天可执行文件：

```shell
git clone git@github.com:simplex-chat/simplex-chat.git
cd simplex-chat
git checkout stable
DOCKER_BUILDKIT=1 docker build --output ~/.local/bin .
```

> **请注意：** 如果遇到 `` 版本 `GLIBC_2.28' 未找到 `` 错误，请使用 `haskell:8.10.7-stretch` 基础镜像重新构建（在本地[Dockerfile](/Dockerfile)中更改它）。

#### 在任何操作系统中

1. 安装 [Haskell GHCup](https://www.haskell.org/ghcup/)、GHC 9.6.3 和 cabal 3.10.1.0：

```shell
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

您可以使用 `ghcup tui` 检查或添加 GHC 和 cabal 版本。

2. 克隆源代码：

```shell
git clone git@github.com:simplex-chat/simplex-chat.git
cd simplex-chat
git checkout stable
# 或者构建特定版本：
# git checkout v5.3.0-beta.8
```

`master` 是开发分支，可能包含不稳定的代码。

3. 准备系统：

在 Linux 上：

```shell
apt-get update && apt-get install -y build-essential libgmp3-dev zlib1g-dev
cp scripts/cabal.project.local.linux cabal.project.local
```

在 Mac 上：

```
brew install openssl@3.0
cp scripts/cabal.project.local.mac cabal.project.local
```

您可能需要修改 cabal.project.local 以指向实际的 openssl 位置。

4. 构建应用程序：

```shell
cabal update
cabal install simplex-chat
```

## 使用

### 运行聊天客户端

要启动聊天客户端，请从终端运行 `simplex-chat`。

默认情况下，应用程序数据目录会在主目录中创建（`~/.simplex`，或在 Windows 上为 `%APPDATA%/simplex`），并在其中初始化两个 SQLite 数据库文件 `simplex_v1_chat.db` 和 `simplex_v1_agent.db`。

要为数据库文件指定不同的文件路径前缀，请使用 `-d` 命令行选项：

```shell
$ simplex-chat -d alice
```

例如，运行上述命令将在当前目录中创建 `alice_v1_chat.db` 和 `alice_v1_agent.db` 数据库文件。

三个默认的 SMP 服务器托管在 Linode 上 - 它们在应用程序中[预配置](https://github.com/simplex-chat/simplex-chat/blob/stable/src/Simplex/Chat/Options.hs#L42)。

如果您部署了自己的 SMP 服务器，您可以通过 `-s` 选项配置客户端：

```shell
$ simplex-chat -s smp://LcJUMfVhwD8yxjAiSaDzzGF3-kLG4Uh0Fl_ZIjrRwjI=@smp.example.com
```

服务器地址前面的 Base64url 编码字符串是服务器的离线证书指纹，客户端在 TLS 握手期间会验证该指纹。

您仍然可以使用默认或任何其他服务器与他人交谈 - 它只会影响您发起连接时消息队列的位置（回复队列可以在另一台服务器上，由另一方的客户端设置）。

运行 `simplex-chat -h` 查看所有可用选项。

### 通过 Tor 访问消息服务器

安装 Tor 并将其作为 SOCKS5 代理在端口 9050 上运行，例如在 Mac 上，您可以：

```
brew install tor
brew services start tor
```

使用 `-x` 选项通过 Tor 访问服务器：

```
simplex-chat -x
```

您还可以使用选项 `--socks-proxy=ipv4:port` 或 `--socks-proxy=:port` 配置 SOCKS5 代理的主机和端口，例如如果您在其他主机或端口上运行它。

### 如何使用 SimpleX 聊天

启动聊天后，系统会提示您指定“显示名称”和可选的“全名”以创建本地聊天配置文件。您的显示名称是您的联系人用来称呼您的别名 - 它不是唯一的，也不作为全局身份。如果您的某些联系人选择了相同的显示名称，聊天客户端会在其本地显示名称后添加一个数字后缀。

下图显示了如何连接和消息联系人：

<div align="center">
  <img align="center" src="../images/how-to-use-simplex.svg">
</div>

设置本地配置文件后，输入 `/c`（表示 `/connect`）以创建新连接并生成邀请。通过任何其他渠道将此邀请发送给您的联系人。

您可以通过多次输入 `/connect` 并将这些邀请发送给相应的联系人来创建多个邀请。

邀请只能使用一次，即使被拦截，攻击者也无法通过此队列向您发送消息，一旦您的联系人确认连接已建立。请参阅代理协议以了解[邀请格式](https://github.com/simplex-chat/simplexmq/blob/master/protocol/agent-protocol.md#connection-request)的解释。

收到邀请的联系人应输入 `/c <invitation>` 接受连接。这将建立连接，并通知双方。

然后他们会使用 `@<name> <message>` 命令发送消息。您也可以直接开始输入消息，将其发送给最后一个联系人的联系人。

在聊天中使用 `/help` 查看可用命令列表。

### 群组

要创建群组，请使用 `/g <group>`，然后使用 `/a <group> <name>` 将联系人添加到其中。然后，您可以通过输入 `#<group> <message>` 向群组发送消息。使用 `/help groups` 查看其他命令。

![simplex-chat](../images/groups.gif)

> **请注意**：群组不会存储在任何服务器上，它们作为应用程序数据库中的成员列表进行维护，消息将发送给这些成员。

### 发送文件

您可以使用 `/f @<contact> <file_path>` 向联系人发送文件 - 接收者必须接受文件，然后才能发送。使用 `/help files` 查看其他命令。

![simplex-chat](../images/files.gif)

您可以使用 `/f #<group> <file_path>` 向群组发送文件。

### 用户联系地址

作为一次性邀请链接的替代方案，您可以使用 `/ad`（表示 `/address`）创建长期地址。创建的地址可以通过任何渠道共享，并由其他用户用作链接，通过 `/c <user_contact_address>` 进行联系请求。

您可以使用 `/ac <name>` 和 `/rc <name>` 命令接受或拒绝传入请求。

用户地址是“长期”的，因为它是一个多次使用的连接链接 - 它可以使用，直到用户删除它，在这种情况下，所有已建立的连接仍然保持活动状态（与电子邮件的工作方式不同，更改地址会导致人们无法向您发送消息）。

使用 `/help address` 查看其他命令。

![simplex-chat](../images/user-addresses.gif)
