---
title: Terminal (CLI) Uygulaması
revision: 31.01.2023
---

| Son Güncelleme 31.01.2023 | Diller: TR, [EN](/docs/CLI.md), [FR](/docs/lang/fr/CLI.md), [CZ](/docs/lang/cs/CLI.md), [PL](/docs/lang/pl/CLI.md) |

# Linux/MacOS/Windows için SimpleX Chat terminal (konsol) uygulaması

## İçindekiler tablosu

- [Terminal sohbet özellikleri](#terminal-sohbet-özellikleri)
- [🚀 Kurulum](#-kurulum)
   - [Sohbet istemcisini indirin](#sohbet-istemcisini-indirin)
      + [Linux ve MacOS](#linux-ve-macos)
      + [Windows](#windows)
   - [Kaynaktan derlemek](#kaynaktan-derlemek)
      + [Docker Kullanımı](#docker-kullanımı)
      + [Herhangi bir işletim sisteminde](#herhangi-bir-işletim-sisteminde)
- [Kullanımı](#kullanımı)
   - [Sohbet istemcisini çalıştırmak](#sohbet-istemcisini-çalıştırmak)
   - [Tor üzerinden mesajlaşma sunucularına erişim](#tor-üzerinden-mesajlaşma-sunucularına-erişim)
   - [SimpleX chat nasıl kullanılır](#simplex-chat-nasıl-kullanılır)
   - [Gruplar](#gruplar)
   - [Dosya göndermek](#dosya-göndermek)
   - [Kullanıcı iletişim adresleri](#kullanıcı-iletişim-adresleri)

## Terminal sohbet özellikleri

- Aynı terminal penceresinde birden fazla kişiyle 1'e 1 sohbet.
- Grup mesajlaşması.
- Kişilere ve gruplara dosya gönderme.
- Kullanıcı iletişim adresleri - çok kullanımlı bağlantılar aracılığıyla bağlantılar kurun.
- Mesajlar yerel bir SQLite veritabanında tutulur.
- Otomatik doldurulan alıcı adı - bağlantı kurulduktan sonra gönderene yanıt vermek için mesajlarınızı yazmanız yeterlidir.
- Uygulamada önceden yapılandırılmış demo SMP sunucuları mevcut veya [kendi sunucunuzu kurabilirsiniz](https://github.com/simplex-chat/simplexmq#using-smp-server-and-smp-agent).
- Sunucu(lar) tarafından görülebilen genel kimlik veya herhangi bir isim yoktur, kişilerinizin ve konuşmalarınızın tam gizliliğini sağlanır.
- Two layers of E2E encryption (double-ratchet for duplex connections, using X3DH key agreement with ephemeral Curve448 keys, and NaCl crypto_box for SMP queues, using Curve25519 keys) and out-of-band passing of recipient keys (see [How to use SimpleX chat](#how-to-use-simplex-chat)).
İki katmanlı E2E uçtan uca şifreleme (Çift yönlü bağlantılar için çift-Ratchet ve kısa ömürlü Curve448 anahtarları. SMP kuyrukları için ise X3DH anahtar anlaşması, Curve25519 anahtarları ve NaCl crypto_box) ve alıcı anahtarlarının bant dışı aktarımı (bknz. [SimpleX sohbet nasıl kullanılır](#how-to-use-simplex-chat)).
- Mesaj bütünlüğü doğrulaması (önceki mesajların özetlerinin dahil edilmesi yoluyla).
- Otomatik olarak oluşturulan Ed448 anahtarları ile SMP sunucuları tarafından her komutun/mesajın kimlik doğrulaması.
- TLS 1.3 aktarım şifrelemesi.
- Trafik korelasyonunu azaltmak için SMP sunucusundan alıcıya giden mesajların ek şifrelenmesi.

Anahtar değişiminde yer alan açık anahtarlar kimlik için kullanılmaz, bunlar her kişi için rastgele üretilir.

Teknik ayrıntılar için [Kullanılan Şifreleme İlkeleri](https://github.com/simplex-chat/simplexmq/blob/master/protocol/overview-tjr.md#encryption-primitives-used) adresine bakın.

<a name="kurulum"></a>

## 🚀 Kurulum

### Sohbet istemcisini indirin

#### Linux ve MacOS

Simplex-chat'i **kurmak** veya **güncellemek** için kurulum betiğini çalıştırmalısınız. Bunu yapmak için aşağıdaki cURL veya Wget komutunu kullanın:

```sh
curl -o- https://raw.githubusercontent.com/simplex-chat/simplex-chat/stable/install.sh | bash
```

```sh
wget -qO- https://raw.githubusercontent.com/simplex-chat/simplex-chat/stable/install.sh | bash
```

Sohbet istemcisi indirildikten sonra, terminalinizde `simplex-chat` komutu ile çalıştırabilirsiniz.

Alternatif olarak, sisteminiz için binary dosyasını [en son kararlı sürüm](https://github.com/simplex-chat/simplex-chat/releases) adresinden manuel olarak indirebilir ve aşağıda gösterildiği gibi çalıştırılabilir hale getirebilirsiniz.

```sh
chmod +x <binary>
mv <binary> ~/.local/bin/simplex-chat
```

(veya `PATH` üzerinde tercih edilen başka bir konum).

MacOS'ta ayrıca [Gatekeeper'ın çalıştırmasına izin vermeniz](https://support.apple.com/en-us/HT202491) gerekir.

#### Windows

```sh
move <binary> %APPDATA%/local/bin/simplex-chat.exe
```

### Kaynaktan derlemek

> **Lütfen dikkat:** uygulamayı oluşturmak için [kararlı dal/stable](https://github.com/simplex-chat/simplex-chat/tree/stable) kaynak kodunu kullanın.

#### Docker Kullanımı

Linux'ta, [özel output ile docker derlemesi](https://docs.docker.com/engine/reference/commandline/build/#custom-build-outputs) başlığını kullanarak sohbet yürütülebilir dosyasını oluşturabilirsiniz:

```shell
git clone git@github.com:simplex-chat/simplex-chat.git
cd simplex-chat
git checkout stable
DOCKER_BUILDKIT=1 docker build --output ~/.local/bin .
```

> **Lütfen dikkat:** ``GLIBC_2.28' sürümü bulunamadı`` hatasıyla karşılaşırsanız, `haskell:8.10.7-stretch` base imajıyla yeniden oluşturun (ve yerel [Dockerfile](/Dockerfile)'ınızda değiştirin).

#### Herhangi bir işletim sisteminde

1. [Haskell GHCup](https://www.haskell.org/ghcup/), GHC 9.6.3 ve cabal 3.10.1.0'ı yükleyin:

```shell
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

GHC ve cabal sürümlerini kontrol etmek veya eklemek için `ghcup tui` komutunu kullanabilirsiniz.

2. Kaynak kodunu klonlayın:

```shell
git clone git@github.com:simplex-chat/simplex-chat.git
cd simplex-chat
git checkout stable
# veya belirli bir sürümü kullanmak için:
# git checkout v5.3.0-beta.8
```

`master` bir geliştirme dalıdır, kararsız kod içerebilir.

3. Sisteminizi hazırlayın:

Linux'ta:

```shell
apt-get update && apt-get install -y build-essential libgmp3-dev zlib1g-dev
cp scripts/cabal.project.local.linux cabal.project.local
```

Mac'te:

```
brew install openssl@1.1
cp scripts/cabal.project.local.mac cabal.project.local
```

Openssl'in gerçek konumuna işaret etmek için cabal.project.local dosyasını değiştirmeniz gerekebilir.

4. Uygulamayı oluşturun:

```shell
cabal update
cabal install simplex-chat
```

## Kullanımı

### Sohbet istemcisini çalıştırmak

Sohbet istemcisini başlatmak için terminalden `simplex-chat` komutunu çalıştırın.

Varsayılan olarak, uygulama verileri ana dizinde (`~/.simplex` veya Windows'ta `%APPDATA%/simplex`) oluşturulur ve içinde iki adet SQLite veritabanı dosyası (`simplex_v1_chat.db` ve `simplex_v1_agent.db`) içerir.

Veritabanı dosyaları için farklı bir dosya yolu öneki belirtmek için `-d` komut satırı seçeneğini aşağıdaki gibi kullanabilirsiniz:

```shell
$ simplex-chat -d alice
```

Örneğin yukarıdaki çalıştırma, geçerli dizinde `alice_v1_chat.db` ve `alice_v1_agent.db` veritabanı dosyalarını oluşturacaktır.

Linode'da üç varsayılan SMP sunucusu barındırılmaktadır - bunlar [uygulamada önceden yapılandırılmıştır](https://github.com/simplex-chat/simplex-chat/blob/stable/src/Simplex/Chat/Options.hs#L42).

Eğer kendi SMP sunucunuzu/sunucularınızı konuşlandırdıysanız, istemciyi `-s` seçeneği ile yapılandırabilirsiniz:

```shell
$ simplex-chat -s smp://LcJUMfVhwD8yxjAiSaDzzGF3-kLG4Uh0Fl_ZIjrRwjI=@smp.example.com
```

Sunucu adresinden önce gelen Base64url kodlu dize, TLS el sıkışması sırasında istemci tarafından doğrulanan sunucunun çevrimdışı sertifika parmak izidir.

Varsayılan veya başka bir sunucuyu kullanan kişilerle hala konuşabilirsiniz - bu sadece bağlantıyı başlattığınızda mesaj kuyruğunun konumunu etkiler (ve yanıt kuyruğu, diğer tarafın istemcisi tarafından ayarlandığı gibi başka bir sunucuda olabilir).

Mevcut tüm seçenekleri görmek için `simplex-chat -h` komutunu çalıştırın.

### Tor üzerinden mesajlaşma sunucularına erişim

Tor'u yükleyin ve 9050 numaralı bağlantı noktasında SOCKS5 proxy olarak çalıştırın, örneğin Mac'te bunu şu şekilde yapabilirsiniz:

```
brew install tor
brew services start tor
```

Sunuculara Tor üzerinden erişmek için `-x` seçeneğini kullanın:

```
simplex-chat -x
```

SOCKS5 proxy'nizin hostname ve bağlantı noktasını (örneğin başka bir ana bilgisayarda veya bağlantı noktasında çalıştırıyorsanız) yapılandırmak için `--socks-proxy=ipv4:port` veya `--socks-proxy=:port` seçeneğini de kullanabilirsiniz.

### SimpleX chat nasıl kullanılır

Sohbeti başlattıktan sonra, yerel bir sohbet profili oluşturmak için "görünen adınızı" ve isteğe bağlı bir "tam adınızı" belirtmeniz istenecektir. Görünen adınız, kişilerinizin size başvurması için bir takma addır - benzersiz değildir ve küresel bir kimlik olarak hizmet etmez. Kişilerinizden bazıları aynı görünen adı seçerse, sohbet istemcisi yerel görünen adlarına sayısal bir son ek ekler.

Aşağıdaki şemada bir kişiye nasıl bağlanılacağı ve mesaj gönderileceği gösterilmektedir:

<div align="center">
  <img align="center" src="../images/how-to-use-simplex.svg">
</div>

Yerel profilinizi ayarladıktan sonra, yeni bir bağlantı ve bir davetiye oluşturmak için `/c` (`/connect` için) girin. Bu daveti başka bir kanal aracılığıyla bağlantınıza gönderin.

Birden fazla kez `/connect` yazarak ve bu davetiyeleri bağlanmak istediğiniz ilgili kişilere göndererek birden fazla davetiye oluşturabilirsiniz.

Davet sadece bir kez kullanılabilir ve bu ele geçirilse bile saldırgan, bağlantınızın kurulduğunu onayladıktan sonra bu kuyruk üzerinden size mesaj göndermek için kullanamayacaktır. [Davet formatı](https://github.com/simplex-chat/simplexmq/blob/master/protocol/agent-protocol.md#connection-request) açıklaması için protokol açıklamasına bakın.

Daveti alan kişi bağlantıyı kabul etmek için `/c <davet>` girmelidir. Bu şekilde bağlantı kurulur ve her iki taraf da bilgilendirilir.

Daha sonra mesaj göndermek için `@<isim> <mesaj>` komutlarını kullanırlar. Ayrıca en son kişiye göndermek için bir mesaj yazmaya başlayabilirsiniz.

Kullanılabilir komutların listesini görmek için sohbette `/help` komutunu kullanabilirsiniz.

### Gruplar

Bir grup oluşturmak için `/g <grup>` kullanın, ardından `/a <grup> <isim>` ile kişileri gruba ekleyin. Daha sonra `#<group> <mesaj>` girerek gruba mesaj gönderebilirsiniz. Diğer komutlar için `/help groups` komutunu kullanın.

![simplex-chat](../images/groups.gif)

> **Lütfen dikkat**: gruplar herhangi bir sunucuda saklanmaz, uygulama veritabanında sadece mesajların gönderileceği üyelerin bir listesi olarak tutulur.

### Dosya göndermek

Kişinize `/f @<kişi> <dosya_konumu>` ile bir dosya gönderebilirsiniz - alıcının gönderilmeden önce dosyayı kabul etmesi gerekecektir. Diğer komutlar için `/help files` kullanın.

![simplex-chat](../images/files.gif)

Dosyaları bir gruba `/f #<grup> <dosya_konumu>` ile gönderebilirsiniz.

### Kullanıcı iletişim adresleri

Tek seferlik davet bağlantılarına alternatif olarak, `/ad` (`/address` için) ile uzun süreli bir adres oluşturabilirsiniz. Oluşturulan adres daha sonra herhangi bir kanal aracılığıyla paylaşılabilir ve diğer kullanıcılar tarafından `/c <kullanıcı_iletişim_adresi>` ile bir iletişim isteği yapmak için bir bağlantı olarak kullanılabilir.

Gelen istekleri `/ac <isim>` ve `/rc <isim>` komutları ile kabul edebilir veya reddedebilirsiniz.

Kullanıcı adresi, çok kullanımlı bir bağlantı bağlantısı olması anlamında "uzun vadelidir" - kullanıcı tarafından silinene kadar kullanılabilir, bu durumda kurulan tüm bağlantılar aktif kalmaya devam eder (e-posta ile çalışmasının aksine, adresin değiştirilmesi insanların size mesaj gönderememesine neden olur).

Diğer komutlar için `/help address` komutunu kullanın.

![simplex-chat](../images/user-addresses.gif)