# The World's Most Secure Messaging

**Ultimate security**: SimpleX network uses the most secure end-to-end encryption, with continuous post-quantum key exchange to protect all messages and metadata.

**Unique privacy**: SimpleX network has no user profile IDs, not even random numbers or keys. It provides better privacy of your contacts, protecting who you talk with from network servers.

**No spam**: nobody can contact you unless you share 1-time link or long-term address.

**Data ownership**: only your device stores your profiles, contacts and messages. You can securely move your data to another device. Servers store encrypted messages only while your device is offline.

**Secure decentralization**: you control which servers to connect to. For security 4 different servers are used in each chat — they can't observe which IP addresses talk to each other.

#### How to connect to others

- Tap new chat button in the corner, then create 1-time link.
- Share the link with your contact via any other messenger or email - it is secure.
- Ask your contact to use the link in the app - click it after the app is installed or paste into the search field in the app.

## 为什么 SimpleX 是独特的

### 您有完整的隐私

SimpleX 保护您的个人资料、联系人和元数据的隐私，不让 SimpleX 网络服务器和任何观察者看到它们。

与任何其他现有的消息传递网络不同，SimpleX 没有分配给用户的标识符—— **甚至随机数也没有**。

### 您可以免受垃圾消息和平台滥用的侵害

因为您在 SimpleX 网络上没有标识符或固定地址，所以除非您以二维码或链接的形式分享一次性或临时用户地址，没有人可以联系您。

### 您控制您的数据

SimpleX 以**便携式加密数据库格式**将所有用户数据存储在客户端设备上—— 它可以转移到另一个设备。

端到端加密的消息在被收到前会暂时保存在 SimpleX 中继服务器上，传送完成后它们会被永久删除。

### 您拥有 SimpleX 网络

SimpleX 网络是完全去中心化的，并且独立于任何加密货币或除互联网以外的任何其他网络。

您可以**搭配自己的服务器来使用 SimpleX**  或使用我们提供的服务器 — 并仍然连接到任何用户。

# 您的身份、个人资料、联系人和元数据的完整隐私

与其他消息网络不同，SimpleX **没有分配给用户的标识符**。 它不依赖电话号码、基于域的地址（如电子邮件或 XMPP）、用户名、公钥甚至随机数来识别其用户—— SimpleX 服务器运营方不知道有多少人使用其服务器。

为了传递消息，SimpleX 使用单向消息队列的[成对匿名地址](https://csrc.nist.gov/glossary/term/Pairwise_Pseudonymous_Identifier)，通常通过不同的服务器将接收和发送的消息分开。

这种设计保护了您正在与之通信的人的隐私，将其隐藏在 SimpleX 网络的服务器和任何观察者之外。 要对服务器隐藏您的 IP 地址，您可以**通过 Tor 连接到 SimpleX 服务器**。

# 防止垃圾消息和平台滥用的最佳保护

因为您在 SimpleX 网络上没有标识符，所以除非您以二维码或链接的形式分享一次性或临时用户地址，没有人可以联系您。

即使使用可选的用户地址，当它被用于发送垃圾邮件联系请求，您可以更改或完全删除它而不会丢失任何连接。

# 您数据的所有权、控制权和安全性

SimpleX Chat 使用**便携式加密数据库格式**仅将所有用户数据存储在客户端设备上，该格式可以导出并传输到任何支持的设备。

端到端加密的消息在被收到前会暂时保存在 SimpleX 中继服务器上，传送完成后它们会被永久删除。

与联合网络服务器（电子邮件、XMPP 或 Matrix）不同，SimpleX 服务器不存储用户帐户，它们仅中继消息，保护双方的隐私。

发送和接收的服务器流量之间没有共同的标识符或密文—— 如果有人在观察它，他们也无法轻易确定谁与谁通信，即使 TLS 受到威胁。

# 完全去中心化 —— 用户拥有 SimpleX 网络

您可以**将 SimpleX 与您自己的服务器一起使用**，并且仍然可以与使用应用中预配置服务器的人们进行通信。

SimpleX 网络使用[开放协议](https://github.com/simplex-chat/simplexmq/blob/stable/protocol/overview-tjr.md)并提供[用于创建聊天机器人的 SDK](https://github.com/simplex-chat/simplex-chat/tree/stable/packages/simplex-chat-client/typescript)， 允许用户实现通过 SimpleX Chat 应用程序与之交互的服务—我们真的很期待看到您会依托SimpleX构建哪些服务。

如果您正在考虑为在SimpleX 网络上开发，例如，为 SimpleX 应用程序用户开发聊天机器人，或将 SimpleX 聊天库集成到您的移动应用，请 [联系我们](https://simplex.chat/contact#/?v=1&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2FK1rslx-m5bpXVIdMZg9NLUZ_8JBm8xTt%23MCowBQYDK2VuAyEALDeVe-sG8mRY22LsXlPgiwTNs9dbiLrNuA7f3ZMAJ2w%3D)获取建议和支持。

## 功能

端到端加密的文字消息，支持markdown和编辑

端到端加密的
图片、视频和文件

端到端加密的去中心化的秘密群组 —只有用户知道它们的存在

端到端加密的语音消息

支持消息自动销毁

端到端加密的音视频通话

便携的加密应用存储— 可将您的个人资料移至另一台设备

隐身模式 —
SimpleX Chat 独有

## 是什么让 SimpleX 能够保密

### 临时匿名成对标识符

SimpleX 为每个用户的联系人或群组成员均使用临时匿名成对地址和凭据。

它让消息能在没有用户标识符的情况下传递，并提供比替代方案更好的元数据隐私。

### 带外密钥交换

许多通信平台容易受到服务器或网络提供商的中间人攻击。

为防止这种情况，当您将通讯地址作为链接或二维码共享时，SimpleX 应用程序会在带外传递一次性密钥。

### 双层端到端加密

双棘轮协议——具有完美前向保密和入侵恢复功能的 OTR(不留记录即时通讯) 消息传递。

每个队列中的网络与密码学库加密盒(NaCL cryptobox)可防止 TLS 受到威胁时消息队列之间的流量关联。

### 消息完整性验证

为了保证消息完整性，消息按顺序编号并会包含前一条消息的哈希值。

如果添加、删除或更改任何消息，收件人都会收到警告。

### 服务器提供的另一层加密

为来信附加服务器加密层，以防止在 TLS 受破坏时接收和发送的服务器流量之间发生关联。

### 通过消息混合减少相关性

SimpleX 服务器会充当低延迟混合节点，打乱传入和传出消息的顺序。

### 安全认证的TLS传输

客户端与服务器之间的连接只使用强加密的 TLS 1.2/1.3。

服务器指纹和通道绑定可防止中间人与重放攻击。

禁用连接恢复以防止会话攻击。

### 可选择通过 Tor 访问

为了保护您的 IP 地址，您可以通过 Tor 或其他传输覆盖网络访问服务器。

要通过 Tor 使用 SimpleX，请安装 [Orbot](https://guardianproject.info/apps/org.torproject.android/) 应用并启用 SOCKS5 代理（在 [iOS](https://apps.apple.com/us/app/orbot/id1609461599?platform=iphone)上需要使用 VPN 模式）。

### 单向消息队列

每个消息队列都有不同的发送和接收地址，单向地传递消息。

与传统的消息代理相比，它减少了攻击媒介和可用的元数据。

### 多层级的内容填充

SimpleX 为每个加密层进行内容填充来对抗长度扩展攻击。

它使不同大小的消息在服务器和网络监视者看来是一样的。

## SimpleX 网络

SimpleX Chat 通过结合 P2P 和联邦网络的优势使其保密性无与伦比。

### 与 P2P 网络不同

所有消息都通过服务器发送，既能更好地保护元数据隐私和可靠地传递异步消息，同时也能避免许多 .

# 与 P2P 通讯协议的比较

[P2P](https://en.wikipedia.org/wiki/Peer-to-peer) 消息传递协议和应用程序存在各种问题，使得它们不如 SimpleX 可靠，分析起来更复杂，并且 容易受到多种类型的攻击。

1. P2P 网络依赖于[分布式散列表(DHT)](https://en.wikipedia.org/wiki/Distributed_hash_table) 的某些变体来路由消息。 DHT 在设计上必须平衡可达性和延迟。 SimpleX 比 P2P 具有更好的可达性和更低的延迟，因为消息可以通过通讯双方选择的多个服务器并行地冗余传递。若是在 P2P 网络中，消息则需要使用算法选择，并依次通过 *O(log N)* 个节点。
2. 与大多数 P2P 网络不同，SimpleX 在设计上没有任何类型的全局用户标识符，甚至临时的也没有。SimpleX 仅使用临时的成对标识符，提供更好的匿名性和元数据保护。
3. P2P 并未解决[中间人攻击(MITM Attack)](https://en.wikipedia.org/wiki/Man-in-the-middle_attack) 问题。大多数现有的 P2P 实现没有使用带外通讯来进行初始密钥的交换，而 SimpleX 使用带外通讯，或者在某些情况下，使用预先存在的安全和可信连接来进行初始密钥交换。
4. P2P 实现（如[BitTorrent](https://en.wikipedia.org/wiki/BitTorrent)）可能会被某些互联网提供商阻止。 SimpleX 与传输协议无关 — 它可以在标准网络协议上工作，例如 WebSockets。
5. 所有已知的 P2P 网络都可能受到[Sybil 攻击](https://en.wikipedia.org/wiki/Sybil_attack)，因为每个节点都是可发现的，并且网络作为一个整体运行。 已知的缓解措施不是需要一个中心化的组件就是需要昂贵的[工作量证明](https://en.wikipedia.org/wiki/Proof_of_work)。而 SimpleX 网络没有服务器可发现性，它是碎片化的并且作为多个隔离的子网运行，这样全网络范围的攻击便无从实现。
6. P2P 网络可能受到 [分布式反射拒绝服务攻击](https://www.usenix.org/conference/woot15/workshop-program/presentation/p2p-file-sharing-hell-exploiting-bittorrent) 。客户端有能力重新广播和放大流量，从而导致整个网络范围内的服务中断。 SimpleX 客户端仅中继来自已知连接的流量，因此不能被攻击者用来放大整个网络的流量。

---

### 不同于联邦网络

SimpleX 中继服务器不存储用户配置文件、联系人和传递的消息，不相互连接，并且没有服务器目录。

---

### SimpleX 网络

服务器提供单向队列来连接用户，但是他们看不到网络连接图图谱— 只有用户可以。

## SimpleX 简述

1. 用户会体验到什么

2. 背后的运作原理

3. 服务器能看到什么

1. 用户会体验到什么

您可以创建联系人和群组，并进行双向对话，就像是任何其他即时通讯软件一样。

它是如何利用单向消息队列并不利用用户识别符工作的？

2. 背后的运作原理

对于每个连接，您都会使用两个单独的消息队列，通过不同的服务器发送和接收消息。

服务器只单向传输消息，无法掌握用户的对话或连接的全貌。

3. 服务器能看到什么

服务器对每个队列都有单独的匿名凭证，并且不知道这些凭证属于哪些用户。

用户可以通过使用 Tor 访问服务器以进一步提高元数据隐私，防止通过 IP 地址关联实际身份。

## 与其他协议的比较

|  |  | Signal、其他大平台 | XMPP、Matrix | P2P协议 |
| --- | --- | --- | --- | --- |
| 需要全局身份 | 否 - 私密 | 是 [1] | 是 [2] | 是 [3] |
| 中间人攻击的可能性 | 不可能 - 安全 [4] | 是 [5] | 是 | 是 |
| 对 DNS 的依赖 | 不依赖 - 有韧性 | 是 | 是 | 否 |
| 单一或中心化网络 | 不依赖 - 去中心化的 | 是 | 不依赖 - 联邦式网络 [6] | 是 [7] |
| 中央组件或其他全网攻击 | 不依赖 - 有韧性 | 是 | 是 [2] | 是 [8] |

---

1. 通常基于电话号码，在某些情况下基于用户名
2. 基于 DNS 的地址
3. 公钥或其他一些全球唯一的 ID
4. SimpleX 中继无法破坏 e2e 加密。 验证安全代码以减轻对带外通道的攻击
5. 如果运营商的服务器受到威胁。 验证 Signal 和其他一些应用程序中的安全代码以缓解该问题
6. 不保护用户的元数据隐私
7. P2P 是分布式的，但并非联邦式的 — 它们作为单个网络运行
8. P2P 网络要么拥有中央权威，要么整个网络可能被攻陷 — [参见此处](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/SIMPLEX.md#comparison-with-p2p-messaging-protocols)

## 不同即时通讯软件端到端加密安全性的比较

|  | Session | Briar | Element | Cwtch | Signal | SimpleX |
| --- | --- | --- | --- | --- | --- | --- |
| 消息填充 | ✗ | ✔︎[1] | ✗ | ✔︎ | ✔︎[1] | ✔︎ |
| 否认（可否认性） | ✗ | ✗ | ✗ | ✔︎[2] | ✔︎[3] | ✔︎ |
| 前向加密 | ✗ | ✔︎ | ✔︎ | ✔︎ | ✔︎ | ✔︎ |
| 事后安全 | ✗ | ✗ | ✗ | ✔︎ | ✗[4] | ✔︎ |
| 双因素密钥交换 | ✔︎ | ✔︎[5] | ✔︎[5] | ✔︎ | ✔︎[5] | ✔︎ |
| 后量子混合加密 | ✗ | ✗ | ✗ | ✗ | ✔︎[6] | ✔︎ |

---

1. Briar 将消息大小向上取整至 1024 字节，Signal 消息大小向上取整至 160 字节
2. 可否认性不包括客户端-服务器连接。
3. 使用加密签名似乎会损害可否认性（否认能力），但这一点需要澄清。
4. 多设备部署会降低双棘轮攻击后的安全性 — [参见此处](https://eprint.iacr.org/2021/626.pdf).
5. 双因素密钥交换可通过安全码验证进行，并非强制要求。
6. 后量子密钥协商是“稀疏的”——它只保护了部分棘轮步骤。
