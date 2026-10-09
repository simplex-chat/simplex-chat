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

## SimpleX為何與眾不同

### 您擁有完整的私隱權

SimpleX 保護您的個人資料、聯絡人和元資料的隱私，使其對 SimpleX 網路伺服器和任何觀察者隱藏。

與任何其他現有的訊息網路不同，SimpleX 不指定使用者的標識符—**甚至沒有隨機數**。

### SimpleX保護您免受垃圾訊息、濫用之害

由於您在 SimpleX 網路上沒有任何標識符或固定地址，因此除非您分享一次性或臨時地址 (如 二維碼或連結)，任何人都無法與您聯絡。

### 您掌控您的數據

SimpleX 以 **可攜式加密數據庫格式** 儲存用戶端裝置上的所有使用者資料；這些資料可傳輸至其他設備。

端對端加密的訊息暫時保留在 SimpleX 中繼伺服器上，收到訊息後會被永久刪除。

### 您擁有SimpleX網路

SimpleX 網路是完全去中心化的，獨立於任何加密貨幣或任何其他網路（除Internet）。

您可以**使用您自己的伺服器**運行SimpleX，或使用我們提供的伺服器並仍連線到任何使用者。

# 您的身份、個人資料、聯絡人與詮釋資料完全隱密

與其他通訊網路不同，SimpleX **不為使用者指定標識符**。它不依賴電話號碼、網域地址 (如電子郵件或 XMPP)、用戶名、公鑰或甚至隨機數來識別使用者；SimpleX 伺服器操作員不知道有多少人使用他們的伺服器。

To deliver messages SimpleX uses [pairwise anonymous addresses](https://csrc.nist.gov/glossary/term/Pairwise_Pseudonymous_Identifier) of unidirectional message queues, separate for received and sent messages, usually via different servers.

This design protects the privacy of who you are communicating with, hiding it from SimpleX network servers and from any observers. To hide your IP address from the servers, you can **connect to SimpleX servers via Tor**.

# 防止垃圾郵件和濫用的最佳保護

Because you have no identifier on the SimpleX network, nobody can contact you unless you share a one-time or temporary user address, as a QR code or a link.

Even with the optional user address, while it can be used to send spam contact requests, you can change or completely delete it without losing any of your connections.

# 您的數據的擁有權、控制與安全

SimpleX Chat stores all user data only on client devices using a **portable encrypted database format** that can be exported and transferred to any supported device.

The end-to-end encrypted messages are held temporarily on SimpleX relay servers until received, then they are permanently deleted.

Unlike federated networks servers (email, XMPP or Matrix), SimpleX servers don't store user accounts, they only relay messages, protecting the privacy of both parties.

傳送與接收的伺服器流量之間沒有共同的標識符或密文—如果有人觀察，即使 TLS 遭到破壞，也無法輕易確定誰與誰通訊。

# 完全去中心化—用戶擁有 SimpleX 網路

您可以**使用您自己的伺服器**運行SimpleX，並仍與使用應用程式中預設伺服器的人進行通訊。

SimpleX網路使用[開源協議](https://github.com/simplex-chat/simplexmq/blob/stable/protocol/overview-tjr.md)並提供[開發包以編寫聊天機器人](https://github.com/simplex-chat/simplex-chat/tree/stable/packages/simplex-chat-client/typescript)，允許使用者透過SimpleX Chat應用程式與服務互動—我們非常期待看到您的SimpleX服務之作！

如您正考慮針對 SimpleX 網路進行開發，例如針對 SimpleX 使用者的聊天機器人，或將 SimpleX 聊天函式庫整合至您的手機應用程式，請[與我們聯絡](https://simplex.chat/contact#/?v=1&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2FK1rslx-m5bpXVIdMZg9NLUZ_8JBm8xTt%23MCowBQYDK2VuAyEALDeVe-sG8mRY22LsXlPgiwTNs9dbiLrNuA7f3ZMAJ2w%3D)以獲取支持與建議。

## 特徵

支持Markdown與編輯的、端到端加密的訊息

端到端加密的圖像、視訊、檔案

端到端加密的、去中心化的群組—存在只有用戶自己知道

端到端加密的語音訊息

自刪除訊息

端到端加密的語音、視訊通話

可攜、加密存儲—將設定檔移至另一設備

隱身模式—SimpleX獨有

## What makes SimpleX private

### 臨時匿名標識符對

對用戶與群組成員，SimpleX使用臨時、匿名的地址對與憑證對。

SimpleX使不用用戶設定檔傳輸訊息成為可能，與其他軟體相比提供更強的詮釋資料私隱性。

### 頻帶外密鑰交換

許多通訊網路容易受到伺服器或網路供應商的中間人攻擊。

To prevent it SimpleX apps pass one-time keys out-of-band, when you share an address as a link or a QR code.

### 雙層端到端加密

雙棘輪協定—帶有前向保密、入侵恢復特質的不留記錄即時通訊協定。

如TLS安全受威脅，每個隊列中的NaCL cryptobox可防止關聯訊息隊列間的通訊。

### 訊息完整性驗證

To guarantee integrity the messages are sequentially numbered and include the hash of the previous message.

If any message is added, removed or changed the recipient will be alerted.

### 伺服器附加加密層

傳送至收件者的伺服器加密附加層，以防止 TLS 遭到攻擊時，接收和傳送的伺服器流量之間的關聯。

### 訊息混雜以降低關聯性

SimpleX servers act as low latency mix nodes — the incoming and outgoing messages have different order.

### 經安全鑑權的TLS 傳送

Only TLS 1.2/1.3 with strong algorithms is used for client-server connections.

Server fingerprint and channel binding prevent MITM and replay attacks.

連線恢復被停用，以防止會話攻擊。

### 選擇性 經由 Tor 訪問

為保護您的 IP 位址，您可透過 Tor 或其他傳輸覆蓋網路訪問伺服器。

To use SimpleX via Tor please install [Orbot app](https://guardianproject.info/apps/org.torproject.android/) and enable SOCKS5 proxy (or VPN [on iOS](https://apps.apple.com/us/app/orbot/id1609461599?platform=iphone)).

### 單向訊息隊列

Each message queue passes messages in one direction, with the different send and receive addresses.

與傳統的訊息代理相比，它可減小攻擊媒介及可見的詮釋資料量。

### 多層內容填充

SimpleX 為每個加密層使用內容填充，以挫敗通過監控訊息長度的攻擊。

它讓不同長度的訊息在伺服器和網路觀察者看來相同。

## SimpleX Network

SinpleX Chat 結合點對點和互聯網路的優點，提供最佳的隱私性。

### Unlike P2P networks

所有訊息都經由伺服器傳送，既能提供更好的元資料隱私和可靠的異步訊息傳送，又能避免許多 .

# 與點對點訊息傳輸協定的比較

[點對點](https://en.wikipedia.org/wiki/Peer-to-peer)通訊協定和應用程式有多種問題，使得它們不如 SimpleX 可靠、分析起來更複雜，而且易受幾種類型的攻擊。

1. 點對點網路依賴 [DHT](https://en.wikipedia.org/wiki/Distributed_hash_table) 的某些變體來路由訊息。DHT 設計必須平衡傳送保證和延遲。與點對點相比，SimpleX 具有更好的傳送保證和更低的延遲，因為訊息可以使用收件者選擇的伺服器，經由多個伺服器並行冗餘地傳送。在點對點網路中，訊息依次經由*O(log N)*個由演算法選擇的節點。
2. SimpleX 的設計與大多數點對點網路不同，沒有任何類型的全局使用者標識符，即使是臨時標識符。SimpleX 只使用臨時的標識符對，提供更好的匿名性和元資料保護。
3. P2P does not solve [MITM attack](https://en.wikipedia.org/wiki/Man-in-the-middle_attack) problem, and most existing implementations do not use out-of-band messages for the initial key exchange. SimpleX uses out-of-band messages or, in some cases, pre-existing secure and trusted connections for the initial key exchange.
4. P2P implementations can be blocked by some Internet providers (like [BitTorrent](https://en.wikipedia.org/wiki/BitTorrent)). SimpleX is transport agnostic — it can work over standard web protocols, e.g. WebSockets.
5. All known P2P networks may be vulnerable to [Sybil attack](https://en.wikipedia.org/wiki/Sybil_attack), because each node is discoverable, and the network operates as a whole. Known measures to mitigate it require either a centralized component or expensive [proof of work](https://en.wikipedia.org/wiki/Proof_of_work). SimpleX network has no server discoverability, it is fragmented and operates as multiple isolated sub-networks, making network-wide attacks impossible.
6. P2P networks may be vulnerable to [DRDoS attack](https://www.usenix.org/conference/woot15/workshop-program/presentation/p2p-file-sharing-hell-exploiting-bittorrent), when the clients can rebroadcast and amplify traffic, resulting in network-wide denial of service. SimpleX clients only relay traffic from known connections and cannot be used by an attacker to amplify the traffic in the whole network.

---

### 與互聯網路不同

SimpleX 中繼伺服器不儲存使用者個人資料、聯絡人和傳送的訊息，也不連線彼此，也沒有伺服器目錄。

---

### SimpleX網路

伺服器提供單向佇列以連接使用者，但伺服器無法看到網路連線圖 —只有使用者能看到。

## SimpleX 解釋

1. 用戶體驗

2. 它是如何工作的

3. 伺服器可以看到什麼

1. 用戶體驗

你可以建立聯絡人和群組，並進行雙向對話，就像在任何其他即時通訊軟件中一樣。

它如何在沒有使用者個人檔案識別符的情況下使用單向佇列？

2. 它是如何工作的

對於每個連接，您可以使用兩個單獨的消息佇列通過不同的伺服器發送和接收消息。

伺服器僅單向傳遞消息，無法全面瞭解使用者的對話記錄或連接。

3. 伺服器可以看到什麼

伺服器對每個佇列都有單獨的匿名憑證，並且不知道它們屬於哪些使用者。

用戶可以通過使用 Tor 訪問伺服器來進一步提高元數據隱私，防止按 IP 位址進行序列化。

## 與其他協議之比較

|  |  | Signal，大平台 | XMPP、Matrix | 點對點協議 |
| --- | --- | --- | --- | --- |
| 需要全局身份 | No - private | Yes [1] | Yes [2] | Yes [3] |
| 中間人攻擊之可能 | No - secure [4] | Yes [5] | Yes | Yes |
| 對DNS的依賴 | No - resilient | Yes | Yes | No |
| 單一或集中式網路 | No - decentralized | Yes | No - federated [6] | Yes [7] |
| Central component or other network-wide attack | No - resilient | Yes | Yes [2] | Yes [8] |

---

1. 通常基於電話號碼，有時用戶名
2. 基於DNS的位址
3. 公鑰或其他某種全局獨一的標識符
4. SimpleX中繼不可能威脅端到端加密。由其他方式驗證安全碼以杜絕攻擊之可能
5. 如果運營商的伺服器受到攻擊，用其他應用程式（如 Signal）驗證 SimpleX 安全碼，以減輕風險
6. 不保護使用者的元數據隱私
7. While P2P are distributed, they are not federated — they operate as a single network
8. 點對點網路必須有中央權威，否則整個網路都可能受到攻擊 — [見此](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/SIMPLEX.md#comparison-with-p2p-messaging-protocols)

## Comparison of end-to-end encryption security in different messengers

|  | Session | Briar | Element | Cwtch | Signal | SimpleX |
| --- | --- | --- | --- | --- | --- | --- |
| Message padding | ✗ | ✔︎[1] | ✗ | ✔︎ | ✔︎[1] | ✔︎ |
| Repudiation (deniability) | ✗ | ✗ | ✗ | ✔︎[2] | ✔︎[3] | ✔︎ |
| Forward secrecy | ✗ | ✔︎ | ✔︎ | ✔︎ | ✔︎ | ✔︎ |
| Post-compromise security | ✗ | ✗ | ✗ | ✔︎ | ✗[4] | ✔︎ |
| 2-factor key exchange | ✔︎ | ✔︎[5] | ✔︎[5] | ✔︎ | ✔︎[5] | ✔︎ |
| Post-quantum hybrid crypto | ✗ | ✗ | ✗ | ✗ | ✔︎[6] | ✔︎ |

---

1. Briar pads messages to the size rounded up to 1024 bytes, Signal - to 160 bytes
2. Repudiation does not include client-server connection.
3. It appears that the usage of cryptographic signatures compromises repudiation (deniability), but it needs to be clarified.
4. Multi-device implementation compromises post-compromise security of Double Ratchet — [見此](https://eprint.iacr.org/2021/626.pdf).
5. 2-factor key exchange is optional via security code verification.
6. Post-quantum key agreement is "sparse" — it protects only some of the ratchet steps.
