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

## なぜSimpleXが唯一無二なのか

### プライバシーが完全に守られます

SimpleXは、SimpleXネットワークのサーバやその他の観察者から隠すことで、あなたのプロフィール、連絡先やメタデータのプライバシーを守ります。

その他の既存のメッセージネットワークと異なり、SimpleXはユーザへ識別子を割り当てません — **ランダムな番号さえありません**。

### スパムや悪用から 保護されています

あなたは識別子や固定されたアドレスをSimpleXネットワーク上で持たないため、あなたがQRコードやリンクといった一度のみ使用可能もしくは一時的なユーザアドレスを共有しない限り、誰もあなたへ連絡することができません。

### データを管理するのはあなたです

SimpleXはクライアント端末上の全てのユーザデータを **ポータブルで暗号化されたデータベースフォーマット**で保管します—別の端末へ移行することができます。

エンドツーエンドで暗号化されたメッセージは、SimpleXのリレーサーバ上で受信されるまで一時的に保管され、その後永久的に削除されます。

### SimpleX ネットワークを所有

SimpleXネットワークは、インターネット以外のいかなる暗号通貨やネットワークから独立しており、完全に分散化されています。

あなたは私たちの提供するサーバや **自分自身のサーバでSimpleXを使う** ことができます — そして別のユーザとつながることができます。

# ID、プロフィール、連絡先、メタデータの完全なプライバシー

他のメッセージングネットワークとは異なり、SimpleX には**ユーザーに割り当てられる識別子がありません**。 ユーザーを識別するために、電話番号、ドメインベースのアドレス (電子メールや XMPP など)、ユーザー名、公開キー、さらには乱数にも依存しません。 — サーバオペレータはどれだけの人が利用しているかも知ることはありません。

メッセージを配信するために、SimpleX は一方向メッセージ キューの[ペアワイズ匿名アドレス](https://csrc.nist.gov/glossary/term/Pairwise_Pseudonymous_Identifier)を使用し、受信メッセージと送信メッセージに分けて、通常は異なるサーバーを経由します。

この設計により、通信相手のプライバシーが保護され、SimpleX ネットワークサーバや監視者からプライバシーが隠されます。 IP アドレスをサーバから隠すには、**Tor 経由で SimpleX サーバーに接続**します。

# スパムと悪用からの最高の保護

SimpleXネットワークには識別子がないため、ワンタイムまたは一時的なユーザー アドレスを QR コードまたはリンクとして共有しない限り、誰もあなたに連絡することはできません。

オプションのユーザー アドレスを使用しても、スパムの連絡先リクエストの送信に使用される可能性がありますが、接続を失うことなく変更または完全に削除できます。

# データの所有権、管理、セキュリティ

SimpleX Chat は、サポートされているデバイスにエクスポートして転送できる**ポータブル暗号化データベース形式**を使用して、すべてのユーザー データをクライアント デバイスにのみ保存します。

エンドツーエンドで暗号化されたメッセージは、SimpleXのリレーサーバーで受信するまで一時的に保持され、その後永久に削除されます。

電子メール、XMPP、Matrixなどの連携ネットワークサーバーとは異なり、SimpleXサーバーはユーザーアカウントを保存せず、メッセージの中継のみを行い、双方のプライバシーを保護します。

送受信されるサーバー トラフィックの間に共通の識別子や暗号文はありません。 — 誰かがそれを観察している場合、たとえ TLS が侵害されたとしても、誰が誰と通信しているのかを簡単に判断することはできません。

# 完全に分散化されています — ユーザーは SimpleX ネットワークを所有します

あなたが、**自分自身のサーバでSimpleXを使っても**、アプリで予め設定されたサーバを使う方々と連絡を取ることができます。

SimpleXネットワークは、SimpleX Chatアプリを介してユーザが交流するサービスを実装させつつ[オープンプロトコル](https://github.com/simplex-chat/simplexmq/blob/stable/protocol/overview-tjr.md)を使い、[チャットボットを作成するためにSDK](https://github.com/simplex-chat/simplex-chat/tree/stable/packages/simplex-chat-client/typescript)を提供します—私たちはあなた達がどのようなSimpleXのサービスを築くか本当に楽しみです。

例えば、SimpleXアプリユーザへのチャットボットやSimpleX Chatライブラリーの携帯アプリへの統合など、SimpleXネットワークに関する開発を検討してくださっているようでしたら、どのようなアドバイスや支援のことでも[ご連絡ください](https://simplex.chat/contact#/?v=1&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2FK1rslx-m5bpXVIdMZg9NLUZ_8JBm8xTt%23MCowBQYDK2VuAyEALDeVe-sG8mRY22LsXlPgiwTNs9dbiLrNuA7f3ZMAJ2w%3D) 。

## 特徴

マークダウンと編集を使用可能なE2E 暗号化メッセージ

E2E暗号化された
画像とファイル

分散型シークレットグループ —
ユーザーのみがその存在を知っています

E2E暗号化された音声メッセージ

消えるメッセージ

E2E暗号化された
音声通話とビデオ通話

ポータブルな暗号化データベース — プロファイルを別のデバイスに移動する

シークレット モード —
SimpleX Chat に固有の

## SimpleX をプライベートにするもの

### 一時的な匿名のペア識別子

SimpleX は、ユーザー連絡先またはグループ メンバーごとに、一時的な匿名のペアごとのアドレスと資格情報を使用します。

ユーザー プロファイル識別子なしでメッセージが配信されるため、他の方法よりも優れたメタデータ プライバシーが提供されます。

### 帯域外の 鍵交換

多くの通信ネットワークは、サーバーやネットワーク プロバイダーによるMITM 攻撃に対して脆弱です。

これを防ぐために、SimpleX アプリは、アドレスをリンクまたは QR コードとして共有するときに、ワンタイム キーを帯域外で渡します。

### 2レイヤーの エンドツーエンド暗号化

ダブルラチェットプロトコル —
完全な前方秘匿性と侵入回復機能を備えたOTRメッセージング。

各キューのNaCL cryptoboxは、TLSが侵害された場合にメッセージキュー間のトラフィック相関を防止します。

### メッセージの整合性 検証

整合性を保証するために、メッセージには連続した番号が付けられ、前のメッセージのハッシュが含まれます。

メッセージが追加、削除、または変更されると、受信者に警告が表示されます。

### 追加レイヤーの サーバー暗号化

TLSが侵害された場合、受信したサーバー・トラフィックと送信したサーバー・トラフィックの相関を防ぐため、受信者に配信するサーバー暗号化レイヤーを追加します。

### メッセージのミキシング 相関性を減らす

SimpleX サーバーは、低遅延の混合ノードとして機能します — 受信メッセージと送信メッセージの順序が異なります。

### セキュアな認証付き TLSトランスポート

クライアント/サーバー接続には、強力なアルゴリズムを備えた TLS 1.2/1.3 のみが使用されます。

サーバーのフィンガープリントとチャネル バインディングにより、MITM 攻撃やリプレイ攻撃を防止します。

セッション攻撃を防ぐために、接続の再開は無効になっています。

### オプション Tor経由のアクセス

IP アドレスを保護するために、Tor またはその他のトランスポート オーバーレイ ネットワーク経由でサーバーにアクセスできます。

Tor経由でSimpleXを使用するには、[Orbotアプリ](https://guardianproject.info/apps/org.torproject.android/)をインストールし、SOCKS5プロキシを有効にしてください（iOSの場合は[VPN](https://apps.apple.com/us/app/orbot/id1609461599?platform=iphone)）。

### 単方向 メッセージキュー

各メッセージ キューは、異なる送信アドレスと受信アドレスを使用してメッセージを一方向に渡します。

従来のメッセージ ブローカーと比較して、攻撃ベクトルが減少し、利用可能なメタデータが減少します。

### 何レイヤーもの コンテンツパディング

SimpleX は、各暗号化レイヤーにコンテンツ パディングを使用して、メッセージ サイズ攻撃を阻止します。

これにより、異なるサイズのメッセージがサーバーやネットワーク オブザーバーには同じように見えます。

## SimpleX ネットワーク

SimpleX Chat は、P2P とフェデレーション ネットワークの利点を組み合わせて最高のプライバシーを提供します。

### P2Pネットワークとは異なります

すべてのメッセージはサーバー経由で送信され、メタデータのプライバシーが向上し、信頼性の高い非同期メッセージ配信が提供されると同時に、多くが回避されます .

# P2Pメッセージングプロトコルとの比較

[P2P](https://en.wikipedia.org/wiki/Peer-to-peer) メッセージング プロトコルとアプリには、SimpleX よりも信頼性が低く、分析がより複雑になるさまざまな問題があり、また、いくつかの種類の攻撃に対して脆弱です。

1. P2P ネットワークは、メッセージをルーティングするために [DHT](https://en.wikipedia.org/wiki/Distributed_hash_table) の一部の変種に依存します。 DHT の設計では、配信保証と遅延のバランスを取る必要があります。 SimpleX は、受信者が選択したサーバーを使用して、メッセージを複数のサーバーを介して並行して冗長的に渡すことができるため、P2P よりも優れた配信保証と低い遅延の両方を備えています。 P2P ネットワークでは、メッセージはアルゴリズムによって選択されたノードを使用して、*O(log N)* 個のノードを順番に通過します。
2. SimpleX 設計は、ほとんどの P2P ネットワークとは異なり、一時的であってもいかなる種類のグローバル ユーザー識別子も持たず、一時的なペアごとの識別子のみを使用するため、より優れた匿名性とメタデータ保護が提供されます。
3. P2P は [MITM 攻撃](https://en.wikipedia.org/wiki/Man-in-the-middle_attack) 問題を解決せず、既存の実装のほとんどは最初の鍵交換に帯域外メッセージを使用していません 。 SimpleX は、最初のキー交換に帯域外メッセージを使用するか、場合によっては既存の安全で信頼できる接続を使用します。
4. P2P の実装は、一部のインターネット プロバイダー ([BitTorrent](https://en.wikipedia.org/wiki/BitTorrent) など) によってブロックされる場合があります。 SimpleX はトランスポートに依存しません — WebSocketのような標準的な Web プロトコル上で動作します。
5. すべての既知の P2P ネットワークは、各ノードが検出可能であり、ネットワーク全体が動作するため、[Sybil 攻撃](https://en.wikipedia.org/wiki/Sybil_attack)に対して脆弱である可能性があります。 この問題を軽減する既知の対策には、一元化されたコンポーネントか、高価な[作業証明](https://en.wikipedia.org/wiki/Proof_of_work)が必要です。 SimpleX ネットワークにはサーバーの検出機能がなく、断片化されており、複数の分離されたサブネットワークとして動作するため、ネットワーク全体への攻撃は不可能です。
6. P2P ネットワークは、[DRDoS 攻撃](https://www.usenix.org/conference/woot15/workshop-program/presentation/p2p-file-sharing-hell-exploiting-bittorrent)に対して脆弱になる可能性があります。 クライアントがトラフィックを再ブロードキャストして増幅する可能性があり、その結果、ネットワーク全体のサービス拒否が発生する可能性があります。 SimpleX クライアントは既知の接続からのトラフィックのみを中継するため、攻撃者がネットワーク全体のトラフィックを増幅するために使用することはできません。

---

### 連合型ネットワークとは異なります

SimpleX リレー サーバーは、ユーザー プロファイル、連絡先、配信されたメッセージを保存せず、相互に接続せず、サーバー ディレクトリもありません。

---

### SimpleX ネットワーク

サーバーはユーザーを接続するための一方向キューを提供しますが、ネットワーク接続グラフは表示されません— ユーザーだけがそうします。

## SimpleXの説明

1. ユーザーが経験すること

2. 仕組み

3. サーバーが認識するもの

1. ユーザーが経験すること

他のメッセンジャーと同様に、連絡先やグループを作成し、双方向の会話を行うことができます。

ユーザー プロファイル識別子なしで単方向キューをどのように処理できるのでしょうか?

2. 仕組み

接続ごとに 2 つの個別のメッセージング キューを使用して、異なるサーバー経由でメッセージを送受信します。

サーバーは、ユーザーの会話や接続の全体を把握することなく、メッセージを一方向に送信するだけです。

3. サーバーが認識するもの

サーバーはキューごとに個別の匿名認証情報を持っており、どのユーザーに属しているかはわかりません。

ユーザーは、Tor を使用してサーバーにアクセスし、IP アドレスによる相関を防ぐことで、メタデータのプライバシーをさらに向上させることができます。

## 他のプロトコルとの比較

|  |  | Signal、大きなプラットフォーム | XMPP、Matrix | P2Pプロトコル |
| --- | --- | --- | --- | --- |
| グローバル ID が必要 | いいえ - プライベート | はい [1] | はい [2] | はい [3] |
| MITMの可能性 | いいえ - 安全 [4] | はい [5] | はい | はい |
| DNS への依存 | いいえ - 弾力性 | はい | はい | いいえ |
| 単一または集中型ネットワーク | いいえ - 分散型 | はい | いいえ - 連合型 [6] | はい [7] |
| 中央コンポーネントまたはその他のネットワーク全体の攻撃 | いいえ - 弾力性 | はい | はい [2] | はい [8] |

---

1. 通常は電話番号に基づいていますが、場合によってはユーザー名に基づいています
2. DNSベースのアドレス
3. 公開キーまたはその他のグローバルに一意な ID
4. SimpleX リレーは e2e 暗号化を侵害できません。 セキュリティ コードを検証して帯域外チャネルへの攻撃を軽減します
5. オペレーターのサーバーが侵害された場合。 Signal およびその他の一部のアプリでセキュリティ コードを検証して緩和する
6. ユーザーのメタデータのプライバシーを保護しない
7. P2Pは分散されていますが、フェデレーションされていません — 単一のネットワークとして動作します
8. P2Pネットワークには中央当局が存在するか、ネットワーク全体が侵害される可能性がある — [こちらを見る](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/SIMPLEX.md#comparison-with-p2p-messaging-protocols)

## Comparison of end-to-end encryption security in different messengers

|  | Session | Briar | Element | Cwtch | Signal | SimpleX |
| --- | --- | --- | --- | --- | --- | --- |
| Message padding | ✗ | ✔︎[1] | ✗ | ✔︎ | ✔︎[1] | ✔︎ |
| Repudiation (deniability) | ✗ | ✗ | ✗ | ✔︎[2] | ✔︎[3] | ✔︎ |
| 前方秘匿性 | ✗ | ✔︎ | ✔︎ | ✔︎ | ✔︎ | ✔︎ |
| Post-compromise security | ✗ | ✗ | ✗ | ✔︎ | ✗[4] | ✔︎ |
| 2ファクタ鍵交換 | ✔︎ | ✔︎[5] | ✔︎[5] | ✔︎ | ✔︎[5] | ✔︎ |
| ポスト量子ハイブリッド暗号 | ✗ | ✗ | ✗ | ✗ | ✔︎[6] | ✔︎ |

---

1. Briar pads messages to the size rounded up to 1024 bytes, Signal - to 160 bytes
2. 否認可能性の対象には、クライアントとサーバー間の接続は含まれません。
3. It appears that the usage of cryptographic signatures compromises repudiation (deniability), but it needs to be clarified.
4. Multi-device implementation compromises post-compromise security of Double Ratchet — [こちらを見る](https://eprint.iacr.org/2021/626.pdf).
5. 2-factor key exchange is optional via security code verification.
6. Post-quantum key agreement is "sparse" — it protects only some of the ratchet steps.
