---
title: SimpleX platformu
revision: 07.02.2023
---

| Son Güncelleme 07.02.2023 | Diller: TR, [EN](/docs/SIMPLEX.md), [FR](/docs/lang/fr/SIMPLEX.md), [CZ](/docs/lang/cs/SIMPLEX.md), [PL](/docs/lang/pl/SIMPLEX.md) |
# SimpleX platformu - motivasyon ve karşılaştırma

## Problemler

Mevcut sohbet platformları ve protokolleri aşağıdaki sorunlardan bazılarına veya bazen tümüne sahiptir:

- Kullanıcı profilinin ve kişilerin gizliliğinin olmaması (meta veri gizliliği).
- [E2EE][1] uçtan uca şifreleme uygulamalarının sağlayıcı aracılığıyla MITM saldırılarına karşı korunması yok (veya yalnızca isteğe bağlı koruma).
- İstenmeyen mesajlar (spam ve kötüye kullanım).
- Veri sahipliği ve koruma eksikliği.
- Teknik olmayan kullanıcılar için merkezi olmayan tüm protokollerin kullanım karmaşıklığı.

İletişimin az sayıda merkezi platformda yoğunlaşması, bu sorunların çözümünü oldukça zorlaştırmaktadır.

## Önerilen çözüm

Proposed stack of protocols solves these problems by making both messages and contacts stored only on client devices, reducing the role of the servers to simple message relays that only require authorization of messages sent to the queues, but do NOT require user authentication - not only the messages but also the metadata is protected because users do not have any identifiers assigned to them - unlike with any other platforms.

Önerilen protokol yığını, hem mesajları hem de kişileri yalnızca istemci cihazlarda saklayarak, sunucuların rolünü yalnızca kuyruklara gönderilen mesajların yetkilendirilmesini gerektiren, ancak kullanıcı kimlik doğrulaması gerektirmeyen basit mesaj aktarıcılarına indirgeyerek bu sorunları çözmektedir. Diğer platformlardan farklı olarak yalnızca mesajlar değil, aynı zamanda meta veriler de korunmaktadır, çünkü kullanıcıların kendilerine atanmış herhangi bir tanımlayıcısı yoktur.

Platform hedefleri ve teknik tasarım hakkında daha fazla bilgi için [SimpleX whitepaper](https://github.com/simplex-chat/simplexmq/blob/master/protocol/overview-tjr.md) adresine bakın.

## Neden SimpleX kullanmalısınız?

## SimpleX'in gizlilik ve güvenliğe benzersiz yaklaşımı

Herkes iletişiminin gizliliğine ve güvenliğine önem vermelidir - sıradan konuşmalar bile sizi tehlikeye atabilir.

### Kimliğiniz, profiliniz, kişileriniz ve meta verileriniz için tam gizlilik

**Mevcut diğer mesajlaşma platformlarından farklı olarak, SimpleX'in kullanıcılara atanmış hiçbir tanımlayıcısı yoktur** - kullanıcılarını tanımlamak için telefon numaraları (Signal veya WhatsApp gibi), alan tabanlı adresler (e-posta, XMPP veya Matrix gibi), kullanıcı adları (Telegram gibi), genel anahtarlar veya hatta rastgele sayılar (diğer tüm mesajlaşma programları gibi) kullanmaz - SimpleX'i kaç kişinin kullandığını bile bilmiyoruz.

SimpleX, mesajları iletmek için diğer tüm platformların kullandığı kullanıcı tanımlayıcıları yerine tek yönlü (simpleks) mesaj kuyruklarının adreslerini kullanır. SimpleX'i kullanmak, sahip olduğunuz her kişi için farklı bir e-posta adresine veya telefon numarasına sahip olmak gibidir, ancak tüm bu adresleri yönetme zahmeti yoktur. Yakın gelecekte SimpleX uygulamaları, kullanıcılara daha da iyi gizlilik sağlamak için konuşmaları bir sunucudan diğerine taşıyarak mesaj kuyruklarını otomatik olarak değiştirecektir.

Bu yaklaşım, SimpleX platform sunucularından ve herhangi bir gözlemciden gizleyerek kiminle iletişim kurduğunuzun gizliliğini korur. Ağ erişiminizi SimpleX sunucularına Tor gibi bir yer paylaşım ağı üzerinden bağlanacak şekilde yapılandırarak gizliliğinizi daha da artırabilirsiniz.

### Spam ve kötüye kullanıma karşı en iyi koruma

SimpleX platformunda tanımlayıcınız olmadığından, tek seferlik bir davet bağlantısı veya isteğe bağlı geçici bir kullanıcı adresi paylaşmadığınız sürece sizinle iletişime geçilemez. İsteğe bağlı kullanıcı adresleri spam iletişim istekleri göndermek için kullanılabilse de, hiçbir bağlantınızı kaybetmeden değiştirebilir veya tamamen silebilirsiniz.

### Verilerinizin tam sahipliği, kontrolü ve güvenliği

SimpleX tüm kullanıcı verilerini istemci cihazlarda saklar, mesajlar yalnızca alınana kadar geçici olarak SimpleX aktarıcı sunucularında tutulur.

Desteklenen tüm cihazlarda, taşınabilir veritabanı formatı kullanıyoruz ki yakında sohbet veritabanını mobil uygulamadan dışa aktarma özelliğini ekleyeceğiz, böylece veritabanları başka bir cihazda da kullanılabilir.

Federe ağların (e-posta, XMPP veya Matrix) sunucularının aksine, SimpleX sunucuları kullanıcı hesaplarını saklamaz, sadece mesajları alıcılara ileterek her iki tarafın da gizliliğini korur. Teslim edilen mesajlar için ek şifreleme katmanı sayesinde, sunucunun gönderilen ve alınan trafiği arasında ortak tanımlayıcılar veya şifrelenmiş mesajlar yoktur. Dolayısıyla herhangi biri sunucu trafiğini gözlemliyorsa, kimin kiminle iletişim kurduğunu kolayca belirleyemez (bilinen trafik korelasyon saldırıları için [SimpleX whitepaper](https://github.com/simplex-chat/simplexmq/blob/master/protocol/overview-tjr.md) adresine bakın).

### Kullanıcıların kendi SimpleX ağı

SimpleX'i kendi sunucularınızla kullanabilir ve uygulamalarda önceden yapılandırılmış sunucuları veya diğer SimpleX sunucularını kullanan kişilerle iletişim kurmaya devam edebilirsiniz.

SimpleX platformu açık bir protokol kullanır ve sohbet botları oluşturmak için SDK sağlar, kullanıcıların SimpleX Sohbet uygulamaları aracılığıyla etkileşime girebilecekleri hizmetlerin uygulanmasına olanak tanır - SimpleX hizmetlerinin neler oluşturulabileceğini görmek için gerçekten sabırsızlanıyoruz.

SimpleX uygulama kullanıcılarına yönelik sohbet botu hizmetleri veya SimpleX Sohbet kütüphanesini mobil uygulamalarınıza entegre etmek için SimpleX platformuyla geliştirme yapmayı düşünüyorsanız, her türlü tavsiye ve destek için lütfen iletişime geçin.

## Diğer protokollerle karşılaştırma

|                                                |      SimpleX chat     | Signal, big platforms |   XMPP, Matrix   |  P2P protocols   |
| :--------------------------------------------- | :-------------------: | :-------------------: | :--------------: | :--------------: |
| Kullanıcı tanımlayıcıları gerektirir           |    Hayır = gizli      |    Evet<sup>1</sup>   | Evet<sup>2</sup> | Evet<sup>3</sup> |
| MITM Olasılığı                                 |    Hayır = güvenli    |    Evet<sup>4</sup>   |       Evet       |       Evet       |
| DNS'e Bağımlılık                               |   Hayır = dirençli    |          Evet         |       Evet       |       Hayır      |
| Tek operatör veya şebeke                       | Hayır = merkeziyetsiz |          Evet         |       Hayır      | Evet<sup>5</sup> |
| Merkezi veya ağ bazlı diğer saldırılar         |   Hayır = dirençli    |          Evet         | Evet<sup>2</sup> | Evet<sup>6</sup> |

1. Genellikle bir telefon numarasına, bazı durumlarda da kullanıcı adlarına dayanır.
2. DNS tabanlı.
3. Açık anahtar veya küresel olarak benzersiz başka bir kimlik.
4. Operatörün sunucuları tehlikeye girerse.
5. P2P ağları ve kripto para tabanlı ağlar dağıtılmış olsalar da merkezi değildirler - tek bir kullanıcı adresi ad alanına sahip tek bir ağ olarak çalışırlar.
6. P2P ağları ya merkezi bir otoriteye sahiptir ya da tüm ağ tehlikeye atılabilir - bir sonraki bölüme bakınız.

## [P2P][9] mesajlaşma protokolleri ile karşılaştırma

Gizlilik ve merkezileşme sorununu çözmeyi amaçlayan çeşitli P2P sohbet/mesajlaşma protokolleri ve uygulamaları vardır, ancak bunların önerilen tasarımdan daha az güvenilir, uygulanması ve analiz edilmesi daha karmaşık ve saldırılara karşı daha savunmasız hale getiren kendi sorunları vardır.

1. [P2P][9] ağları, mesajları/talepleri ağ üzerinden yönlendirmek için [DHT][10]'nin bazı varyantlarını kullanır. DHT uygulamaları güvenilirlik, teslimat garantisi ve gecikme süresini dengelemek zorunda olan karmaşık tasarımlara sahiptir. Önerilen tasarım hem daha iyi teslimat garantisine hem de daha düşük gecikme süresine sahiptir (P2P ağlarında mesaj, algoritma tarafından seçilen düğümler kullanılarak sırayla `O(log N)` düğümden geçirilirken, mesaj alıcı tarafından seçilen sunucular kullanılarak her seferinde bir düğümden paralel olarak birden çok kez geçirilir).

2. Önerilen tasarım, çoğu P2P ağının aksine, geçici bile olsa herhangi bir küresel kullanıcı tanımlayıcısına sahip değildir.

3. P2P'nin kendisi [MITM saldırısı][2] sorununu çözmez ve mevcut çözümlerin çoğu ilk anahtar değişimi için bant dışı mesajlar kullanmaz. Önerilen tasarım, ilk anahtar değişimi için bant dışı mesajları veya bazı durumlarda önceden var olan güvenli ve güvenilir bağlantıları kullanır.

4. P2P uygulamaları bazı İnternet sağlayıcıları tarafından engellenebilir ([BitTorrent][11] gibi). Önerilen tasarım taşıma agnostiktir - standart web protokolleri üzerinden çalışabilir ve sunucular web siteleriyle aynı etki alanlarında konuşlandırılabilir.

5. Bilinen tüm P2P ağlarının [Sybil saldırısına][12] karşı savunmasız olması muhtemeldir, çünkü her düğüm keşfedilebilir ve ağ bir bütün olarak çalışır. Sybil saldırısı olasılığını azaltmak için bilinen önlemler ya merkezi bir bileşen gerektirir ya da pahalıdır [proof of work][13]. Önerilen tasarımda ise sunucu keşfedilebilirliği yoktur - sunucular birbirine bağlı değildir, birbirleri ve tüm istemciler tarafından bilinmezler.SimpleX ağı parçalara ayrılmıştır ve birden fazla izole bağlantı olarak çalışır. Bu, SimpleX ağına ağ çapında saldırıları imkansız hale getirir - bazı sunucular tehlikeye girse bile, ağın diğer bölümleri normal şekilde çalışabilir ve etkilenen istemciler, kişileri veya mesajları kaybetmeden diğer sunucuları kullanmaya geçebilir.

6. P2P ağlarının [DRDoS saldırısına][15] karşı [savunmasız][14] olması muhtemeldir. Önerilen tasarımda istemciler yalnızca bilinen güvenilir bağlantılardan gelen trafiği aktarır ve tüm ağdaki trafiği yansıtmak ve güçlendirmek için kullanılamaz.

[1]: https://en.wikipedia.org/wiki/End-to-end_encryption
[2]: https://en.wikipedia.org/wiki/Man-in-the-middle_attack
[9]: https://en.wikipedia.org/wiki/Peer-to-peer
[10]: https://en.wikipedia.org/wiki/Distributed_hash_table
[11]: https://en.wikipedia.org/wiki/BitTorrent
[12]: https://en.wikipedia.org/wiki/Sybil_attack
[13]: https://en.wikipedia.org/wiki/Proof_of_work
[14]: https://www.usenix.org/conference/woot15/workshop-program/presentation/p2p-file-sharing-hell-exploiting-bittorrent
[15]: https://en.wikipedia.org/wiki/Denial-of-service_attack#Reflected_attack