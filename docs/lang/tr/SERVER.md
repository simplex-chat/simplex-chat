---
title: Kendi SMP Sunucunuzu Barındırma
revision: 03.06.2024
---

| Son Güncellenme 28.05.2024 | Diller: TR, [EN](/docs/SERVER.md), [FR](/docs/lang/fr/SERVER.md), [CZ](/docs/lang/cs/SERVER.md), [PL](/docs/lang/pl/SERVER.md) |

### İçindekiler Tablosu

- [Kendi SMP Sunucunuzu Barındırma](#kendi-smp-sunucunuzu-barındırma)
   - [Genel Bakış](#genel-bakış)
   - [Kurulum](#kurulum)
   - [Konfigürasyon](#konfigürasyon)
      - [Etkileşimli](#etkileşimli)
      - [Komut satırı seçenekleri aracılığıyla](#komut-satırı-seçenekleri-aracılığıyla)
   - [Daha fazla yapılandırma](#daha-fazla-yapılandırma)
   - [Sunucu güvenliği](#sunucu-güvenliği)
      - [Başlatma](#başlatma)
      - [Özel anahtarlar](#özel-anahtarlar)
      - [Çevrimiçi sertifika rotasyonu](#çevrimiçi-sertifika-rotasyonu)
   - [Tor: kurulum ve yapılandırma](#tor-kurulum-ve-yapılandırma)
      - [Onion adresi için kurulum](#onion-adresi-için-kurulum)
      - [SMP PROXY için SOCKS bağlantı noktası](#smp-proxy-için-socks-bağlantı-noktası)
   - [Sunucu bilgi sayfası](#sunucu-bilgi-sayfası)
   - [Dokümantasyon](#dokümantasyon)
      - [SMP sunucu adresi](#smp-sunucu-adresi)
      - [Systemd komutları](#systemd-komutları)
      - [İzleme](#i̇zleme)
   - [SMP sunucunuzu güncelleme](#smp-sunucunuzu-güncelleme)
   - [Uygulamayı sunucuyu kullanacak şekilde yapılandırma](#uygulamayı-sunucuyu-kullanacak-şekilde-yapılandırma)

   
# Kendi SMP Sunucunuzu Barındırma

## Genel Bakış

SMP sunucusu, SimpleX ağında mesajları iletmek için kullanılan röle sunucusudur. SimpleX Chat uygulamalarının önceden ayarlanmış sunucuları vardır (mobil uygulamalar için bunlar smp11, smp12 ve smp14.simplex.im'dir), ancak diğer sunucuları kullanmak için uygulama yapılandırmasını kolayca değiştirebilirsiniz.

SimpleX istemcileri, mesajları almak için hangi sunucunun kullanılacağını yalnızca her bir kişi (veya bir grup üyesi ile grup bağlantısı) için ayrı ayrı belirler ve teslimat adresi değişebileceğinden bu sunucular yalnızca geçicidir.

_Lütfen dikkat_: Uygulama yapılandırmasında sunucuları değiştirdiğinizde, bu yalnızca yeni kişiler için hangi sunucuların kullanılacağını etkiler, mevcut kişiler otomatik olarak yeni sunuculara taşınmaz, ancak kişi/üye bilgileri sayfalarındaki ["Alıcı adresini değiştir"](../blog/20221108-simplex-chat-v4.2-security-audit-new-website.md#change-your-delivery-address-beta) düğmesini kullanarak bunları manuel olarak taşıyabilirsiniz - gelecekte otomatik hale getirilecektir.

## Kurulum

1. İlk olarak `smp-server`ı yükleyin:

   - Manuel dağıtım (aşağıya bakın)

   - Yarı-otomatik dağıtım:
     - [Kurulum komut dosyası](https://github.com/simplex-chat/simplexmq#using-installation-script)
     - [Docker konteyner](https://github.com/simplex-chat/simplexmq#using-docker)
     - [Linode Marketyeri](https://www.linode.com/marketplace/apps/simplex-chat/simplex-chat/)

Manuel kurulum bazı ön işlemler gerektirir:

1. Binary dosyayı yükleyin:

   - Önceden derlenmiş binary dosyaları kullanma:

     ```sh
     curl -L https://github.com/simplex-chat/simplexmq/releases/latest/download/smp-server-ubuntu-20_04-x86-64 -o /usr/local/bin/smp-server && chmod +x /usr/local/bin/smp-server
     ```

   - Kaynaktan derleme:

     Lütfen [Kaynaktan derleme: Dağıtımınızı kullanma](https://github.com/simplex-chat/simplexmq#using-your-distribution) bölümüne bakın

2. `smp-server` için kullanıcı ve grup oluşturun:

   ```sh
   sudo useradd -m smp
   ```

3. Gerekli dizinleri oluşturun ve izinleri atayın:

   ```sh
   sudo mkdir -p /var/opt/simplex /etc/opt/simplex
   sudo chown smp:smp /var/opt/simplex /etc/opt/simplex
   ```

4. Güvenlik duvarında `smp-server` bağlantı noktasına izin verin:

   ```sh
   # Ubuntu için
   sudo ufw allow 5223/tcp
   # Fedora için
   sudo firewall-cmd --permanent --add-port=5223/tcp && \
   sudo firewall-cmd --reload
   ```

5. **Opsiyonel** - Eğer `systemd` ile dağıtım kullanıyorsanız, aşağıdaki içeriğe sahip `/etc/systemd/system/smp-server.service` dosyasını oluşturun:

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

   [Install]
   WantedBy=multi-user.target
   ```

   Ve `sudo systemctl daemon-reload` komutunu çalıştırın.

## Konfigürasyon

Hangi seçeneklerin mevcut olduğunu görmek için `smp-server` komutunu bayraklar olmadan çalıştırın:

```sh
sudo su smp -c smp-server

...
Mevcut komutlar:
  init                     Sunucuyu canlandır - etc/opt/simplex ve
                           /var/opt/simplex dizinlerini ve yapılandırma dosyalarını oluşturur
  start                    Sunucuyu başlat (yapılandırma: 
                           /etc/opt/simplex/smp-server.ini)
  delete                   Yapılandırma ve günlük dosyalarını silme
```

Daha fazla yardımı `sudo su smp -c "smp-server <komut> -h"` komutunu çalıştırarak alabilirsiniz.

Bundan sonra `smp-server`ı yapılandırmamız gerekiyor:

### Etkileşimli

Aşağıdaki komutu çalıştırın:

```sh
sudo su smp -c "smp-server init"
```

Dikkate alınması gereken birkaç seçenek vardır:

<!---
I purposely didn't directly translate the English entries in this section,
because the interface still doesn't support Turkish and users will see the original English text.
Instead, I use 'TR: Long translated TEXT'
-->

- `Enable store log to restore queues and messages on server restart (Yn):`
   TR: _Sunucu yeniden başlatıldığında kuyrukları ve mesajları geri yüklemek için depolama günlüğünü etkinleştir (EVEThayır)_

  Sunucu yeniden başlatıldığında bağlantıları ve mesajları kaydetmeyi ve geri yüklemeyi etkinleştirmek için `y` girin.

  _Lütfen dikkat_: sunucuyu yeniden başlatmak için SIGINT kullanmak önemlidir, aksi takdirde teslim edilmemiş mesajlar geri yüklenmeyecektir. Bağlantılar, sunucunun nasıl yeniden başlatıldığına bakılmaksızın geri yüklenecektir, çünkü mesajların aksine, her değişiklikte yalnızca ekleme günlüğüne eklenirler.

- `Enable logging daily statistics (yN):`
   TR: _Günlük istatistiklerin günlüğe kaydedilmesini etkinleştir (EVEThayır)_

  CSV formatında günlük istatistiklerini etkinleştirmek için `y` girin, örneğin `Grafana`da toplu kullanım grafiklerini göstermek için kullanılabilirler.

Bu istatistikler, oluşturulan, güvence altına alınan ve silinen kuyrukların, gönderilen ve alınan mesajların günlük sayılarını ve ayrıca aktif kuyrukların (yani, herhangi bir mesaj için kullanılan kuyruklar) günlük, haftalık ve aylık sayılarını içerir. Bu bilgilerin, farklı kuyrukların aynı kullanıcılara ait olduğunu ilişkilendirmeye izin verecek herhangi bir şey içermediğine inanıyoruz, ancak bunun herhangi bir şekilde istismar edilebileceğini düşünüyorsanız, lütfen gizlilik içinde [bize bildirin](./SECURITY.md).

- `Require a password to create new messaging queues?`
   TR: _Yeni mesajlaşma kuyrukları oluşturmak için parola mı gerekiyor?_

  `Enter`a basın veya `smp-server`ı parola ile korumak için rastgele parolanızı girin veya parola korumasını devre dışı bırakmak için `n` tuşuna basın.

- `Enter server FQDN or IP address for certificate (127.0.0.1):`
   TR: _Sertifika için sunucu FQDN'sini veya IP adresini girin (127.0.0.1):_

  Smp-sunucunuzun üzerinde çalıştığı domain veya ip adresinizi girin - sunucu sertifikalarına dahil edilecek ve ayrıca sunucu adresinin bir parçası olarak yazdırılacaktır.

### Komut satırı seçenekleri aracılığıyla

Aşağıdaki komutu çalıştırın:

```sh
sudo su smp -c "smp-server init -h"

...
Mevcut seçenekler:
  -l,--store-log           Kalıcılık için mağaza günlüğünü etkinleştirin
  -s,--daily-stats         Günlük sunucu istatistiklerinin günlüğe kaydedilmesini etkinleştirme
  -a,--sign-algorithm ALG  TLS sertifikaları için kullanılan imza algoritması:
                           ED25519, ED448 (varsayılan: ED448)
  --ip IP                  Sunucu IP adresi,FQDN sağlanmamışsa
                           TLS çevrimiçi sertifikası için Ortak Ad olarak kullanılır
                           (varsayılan: "127.0.0.1")
  -n,--fqdn FQDN           TLS çevrimiçi sertifikası için Ortak Ad olarak kullanılan
                           sunucu FQDN'si
  --no-password            Parola olmadan yeni kuyruklar oluşturmaya izin ver
  --password PASSWORD      Yeni mesajlaşma kuyrukları oluşturmak için şifre belirleme
  -y,--yes                 Komut satırı seçeneklerini kullanarak
                           etkileşimli olmayan başlatma
  -h,--help                Bu yardım metnini göster
```

Kullanım durumunuz için hangi bayrakların gerekli olduğunu belirlemeli ve ardından interaktif olmayan başlatma için `smp-server init` komutunu `-y` bayrağı ile çalıştırmalısınız:

```sh
sudo su smp -c "smp-server init -y -<bayraklarınız> <sizin seçenekleriniz>"
```

Örnek çalıştırma:

```sh
sudo su smp -c "smp-server init -y -l --ip 192.168.1.5 --password test"
```

`smp-server` ile yapılandırmanızı başlatmak için:

- sunucu yeniden başlatıldığında bağlantıları ve mesajları geri yükleme (`-l` bayrağı),
- IP adresi `192.168.1.5`,
- `smp-server`ı `test` parolası ile koruyun.

---

Bundan sonra, kurulumunuz tamamlanmıştır ve teminal çıktınızda buna benzer bir şey görmelisiniz:

<!--- For the same reason, it was left as it was, not translated. -->

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

Yukarıdaki sunucu adresi istemci yapılandırmanızda kullanılmalıdır ve sunucu şifresi eklediyseniz, bu şifre yalnızca sunucunuzu kullanarak mesajları almasına izin vermek istediğiniz diğer kişilerle paylaşılmalıdır (tüm kişileriniz mesaj gönderebilecektir - şifre gerektirmez). Başlatma sırasında IP adresini veya ana bilgisayar adlarını geçtiyseniz, bunlar sunucu adresinin bir parçası olarak yazdırılacaktır, aksi takdirde `<hostnames>` yerine gerçek sunucu ana bilgisayar adlarını yazın.

## Daha fazla yapılandırma

Oluşturulan tüm yapılandırma, her parametre için bir açıklama ile birlikte, daha fazla özelleştirme için `/etc/opt/simplex/smp-server.ini` yapılandırma dosyası içinde mevcuttur. Smp-server sürümüne bağlı olarak, yapılandırma dosyası aşağıdaki gibi görünür:

```ini
[INFORMATION]
# AGPLv3 lisansı, herhangi bir kaynak kodu değişikliğini
# sunucunun son kullanıcılarına sunmanızı gerektirir.
# LİSANS: https://github.com/simplex-chat/simplexmq/blob/stable/LICENSE
# Sunucu kaynak kodunun herhangi bir şekilde değiştirilmesi durumunda doğru kaynak kodu URI'sini ekleyin.
# Diğer bilgi alanları mevcutsa, kaynak kodu özelliği de mevcut OLMALIDIR.

source_code: https://github.com/simplex-chat/simplexmq

# Aşağıdaki tüm bilgilerin bildirilmesi isteğe bağlıdır, bu alanlardan herhangi biri atlanabilir.

# Sunucu kullanım koşulları ve değişiklikleri.
# Herhangi bir değişiklikle birlikte standart koşulların ayrı bir belgede kullanılması tavsiye edilir.
# usage_conditions: https://github.com/simplex-chat/simplex-chat/blob/stable/PRIVACY.md
# condition_amendments: link

# Sunucu ve operatörün konumu.
server_country: <SUNUCU_KONUMUNUZ>
operator: <ADINIZ>
operator_country: <KONUMUNUZ>
website: <EĞER_VARSA_WEBSITE>

# İdari irtibatlar.
#admin_simplex: SimpleX address
admin_email: <EPOSTA>
# admin_pgp:
# admin_pgp_fingerprint:

# Şikayetler ve geri bildirimler için iletişim bilgileri.
# complaints_simplex: SimpleX address
complaints_email: <ŞİKAYET_EPOSTASI>
# complaints_pgp:
# complaints_pgp_fingerprint:

# Barındırma sağlayıcısı.
hosting: <BARINDIRMA_SAĞLAYICISI_ADI>
hosting_country: <BARINDIRMA_SAĞLAYICISI_KONUMU>

[STORE_LOG]
# Sunucu kalıcılık için STM belleği kullanır,
# yeniden başlatıldığında kaybolacaktır (örneğin, redis'te olduğu gibi).
# Bu seçenek yalnızca günlük eklemek için bellek tasarrufu sağlar,
# ve sunucu başlatıldığında geri yüklenir.
# Günlük başlangıçta sıkıştırılır (silinen nesneler kaldırılır).
enable: on

# Teslim edilmeyen mesajlar isteğe bağlı olarak kaydedilir ve sunucu yeniden başlatıldığında geri yüklenir,
# bir sonraki yeniden başlatmaya kadar .bak dosyasında korunur.
restore_messages: on
expire_messages_days: 21

# Günlük sunucu istatistiklerini CSV dosyasına kaydetme
log_stats: on

[AUTH]
# Yeni mesajlaşma kuyrukları oluşturmayı tamamen yasaklamak için new_queues seçeneğini kapalı olarak ayarlayın.
# Bu, sunucuyu hizmet dışı bırakmak istediğinizde yararlı olabilir, ancak henüz tüm bağlantılar değiştirilmemiştir.
new_queues: on

# Yeni mesajlaşma kuyrukları oluşturmak üzere temel kimlik doğrulamasını etkinleştirmek için create_password seçeneğini kullanın.
# Parola, istemci yapılandırmasında sunucu adresinin bir parçası olarak kullanılmalıdır:
# smp://fingerprint:password@host1,host2
# Şifre bağlanan kişilerle paylaşılmayacaktır, sadece siz sunucunuzda
# mesajlaşma kuyrukları oluşturmasına izin vermek istediğiniz kullanıcılarla paylaşmalısınız.
# create_password: yeni kuyruklar oluşturmak için şifre (boşluksuz yazdırılabilir herhangi bir ASCII karakteri, '@', ':' ve '/')

[TRANSPORT]
# host yalnızca başlangıçta sunucu adresini yazdırmak için kullanılır
host: <sunucunuzun doman veya ip adresi>
port: 5223
log_tls_errors: off
websockets: off
# control_port: 5224

[PROXY]
# SMP proxy istemcisi için ağ yapılandırması.
# 'host_mode' 'public' (varsayılan) veya 'onion' olabilir.
# Birden fazla ana bilgisayar adına sahip hedef sunucular için tercih edilen ana bilgisayar adını tanımlar.
# host_mode: public
# required_host_mode: off

# Ayrı proxy istatistikleri olarak saymak için çalıştırdığınız aktarıcıların etki alanı sonekleri (boşluk bırakılarak ayrılmış).
# own_server_domains: <alan adı son ekleriniz>

# İletileri hedef sunuculara iletmek için SOCKS proxy bağlantı noktası.
# Gelen tek atlamalı istekler için ayrı bir SOCKS proxy örneğine ihtiyacınız olabilir.
# socks_proxy: localhost:9050

# SOCKS proxy'nin yalnızca .onion hedef ana bilgisayarları için kullanılması için `socks_mode' 'onion' olabilir (varsayılan)
# veya tüm hedef ana bilgisayarlar için kullanılmak üzere 'always' (bir .onion sunucusu ise kullanılabilir).
# socks_mode: onion

# Bir istemcinin proxy komutlarını paralel olarak işlemek için oluşturabileceği iş parçacığı sayısını sınırlayın.
# client_concurrency: 32

[INACTIVE_CLIENTS]
# Etkin olmayan istemcileri kontrol etmek için TTL ve aralık
disconnect: off
# ttl: 43200
# check_interval: 3600

[WEB]
# Sunucu bilgileri ve qr kodları/bağlantıları için statik mini site oluşturma yolunu ayarlayın
static_path: /var/opt/simplex/www

# Bu bağlantı noktasında gömülü bir sunucu çalıştırın
# Onion siteleri herhangi bir portu kullanabilir ve bunu gizli hizmet yapılandırmasına kaydedebilir.
# 80 numaralı bağlantı noktasında çalıştırmak işlem yeteneklerinin ayarlanmasını gerektirebilir.
# http: 8000

# Port, sertifika ve anahtar dosyaları sağlarsanız gömülü bir TLS web sunucusu da çalıştırabilirsiniz.
# Onion adresinde röle çalıştırmak için gerekli değildir.
# https: 443
# cert: /etc/opt/simplex/web.cert
# key: /etc/opt/simplex/web.key
```

## Sunucu güvenliği

### Başlatma

smp-server yapılandırmasını doğrudan sunucu üzerinde başlatmak uygun olsa da, operatörlerin SMP sunucusu CA özel anahtarınızı korumak için smp-server'ı tamamen çevrimdışı olarak başlatmaları **ÖNERİLİR**.

Sunucuyu çevrimdışı olarak hızlı bir şekilde başlatmak için aşağıdaki adımları izleyin:

1. Docker'ı sisteminize yükleyin.

2. [smp-server](https://github.com/simplex-chat/simplexmq#using-docker)'ı yerel olarak dağıtın.

3. Konteyneri yok edin. İlgili tüm yapılandırma dosyaları ve anahtarları `$HOME/simplex/smp/config` adresinde bulunacaktır.

4. `CA` özel anahtarınızı (`ca.key`) güvenli bir yere taşıyın. Daha fazla açıklama için bir sonraki bölüme bakın: [Sunucu güvenliği: Özel anahtarlar](#özel-anahtarlar).

5. **CA anahtarı hariç** diğer tüm yapılandırma dosyalarını sunucuya kopyalayın:

   ```sh
   rsync -hzasP $HOME/simplex/smp/config/ <sunucu_kullanıcısı>@<sunucu_adresi>:/etc/opt/simplex/
   ```

### Özel anahtarlar

Smp sunucusuna bağlantı bir TLS bağlantısı üzerinden gerçekleşir. TLS el sıkışması sırasında istemci, smp-sunucu CA ve sunucu sertifikalarını, parmak izini sunucu adresinde bulunanla karşılaştırarak doğrular. Sunucu TLS kimlik bilgisi tehlikeye girerse, bu anahtar aynı sunucu kimliğini ve kurulan bağlantıları koruyarak yenisini imzalamak için kullanılabilir. smp-sunucunuzu kötü aktörlerden korumak için, operatörlerin CA özel anahtarını güvenli bir yere taşımaları **TAVSİYE EDİLİR**. Bu şu olabilir:

- [Tails](https://tails.net/) [kalıcı ve şifrelenmiş depolama](https://tails.net/doc/persistent_storage/create/index.en.html) ile canlı usb sürücü.
- Çevrimdışı Linux dizüstü bilgisayar.
- Bitwarden.
- Güvenlik gereksinimlerinizi karşılayan diğer tüm güvenli depolar.

CA anahtarlarınızı güvence altına almak için adımları izleyin:

1. SSH aracılığıyla sunucunuza giriş yapın.

2. CA anahtarını bu dosyadan güvenli bir yere kopyalayın:

   ```sh
   /etc/opt/simplex/ca.key
   ```

3. CA anahtarını sunucudan silin. **Lütfen CA anahtarınızı güvenli bir yere kaydettiğinizden emin olun**. Aksi takdirde, [çevrimiçi sertifikayı döndürme](#çevrimiçi-sertifika-rotasyonu) özelliğini kaybedersiniz:

   ```sh
   rm /etc/opt/simplex/ca.key
   ```

### Çevrimiçi sertifika rotasyonu

smp sunucularının operatörlerinin çevrimiçi sertifikayı düzenli olarak (örneğin her 3 ayda bir) rotasyona tabi tutmaları **TAVSİYE EDİLİR**. Bunu yapmak için aşağıdaki adımları izleyin:

1. İlgili klasörler oluşturun:

   ```sh
   mkdir -p $HOME/simplex/smp/config
   ```

1. Yapılandırma dosyalarını sunucudan yerel makineye kopyalayın (henüz yapmadıysanız):

   ```sh
   rsync -hzasP <sunucu_kullanıcısı>@<sunucu_adresi>:/etc/opt/simplex/ $HOME/simplex/smp/config/
   ```

2. CA özel anahtarınızı güvenli bir yerden yerel makineye **Kopyalayın** ve `ca.key` olarak adlandırın.

3. En son `smp-server` ikili dosyasını [Github sürümlerinden](https://github.com/simplex-chat/simplexmq/releases) indirin:

   ```sh
   curl -L 'https://github.com/simplex-chat/simplexmq/releases/latest/download/smp-server-ubuntu-20_04-x86-64' -o smp-server
   ```

4. `smp-server` binary dosyasını `$PATH` dosyanıza ekleyin ve çalıştırılabilir hale getirin:

   ```sh
   sudo mv smp-server /usr/local/bin/ && chmod +x /usr/local/bin/smp-server
   ```

5. smp-server yapılandırmasına giden yolunuzu yapılandırmak için bir değişkeni dışa aktarın:

   ```sh
   export SMP_SERVER_CFG_PATH=$HOME/simplex/smp/config
   ```

6. Aşağıdaki komutu çalıştırın:

   ```sh
   smp-server cert
   ```

   Bu komut çıktısı yazdırılmalıdır:

   <!--- For the same reason, it was left as it was, not translated. -->
   ```sh
   Certificate request self-signature ok
   subject=CN = <domain veya IP adresiniz>
   Generated new server credentials
   ----------
   You should store CA private key securely and delete it from the server.
   If server TLS credential is compromised this key can be used to sign a new one, keeping the same server identity and established connections.
   CA private key location:
   $HOME/simplex/smp/config/ca.key
   ----------
   ```

7. CA anahtarını yapılandırma klasöründen kaldırın (bir yedeğiniz olduğundan emin olun!):

   ```sh
   rm $HOME/simplex/smp/config/ca.key
   ```

8. Sunucuya yeni sertifikalar yükleyin:

   ```sh
   rsync -hzasP $HOME/simplex/smp/config/ <sunucu_kullanıcısı>@<sunucu_adresi>:/etc/opt/simplex/
   ```

9. SSH üzerinden sunucuya bağlanın ve hizmeti yeniden başlatın:

   ```sh
   ssh <sunucu_kullanıcısı>@<sunucu_adresi> "systemctl restart smp-server"
   ```

10. Tamamdır!

## Tor: kurulum ve yapılandırma

### Onion adresi için kurulum

SMP-sunucusu [Tor](https://www.torproject.org) ağı üzerinden erişilebilir olacak şekilde de konuşlandırılabilir. Aşağıdaki komutları `root` kullanıcısı olarak çalıştırın.

1. Tor'u yükleyin:

   Ubuntu/Debian tabanlı dağıtımlar kullandığınızı varsayıyoruz. Eğer kullanmıyorsanız, lütfen [resmi̇ tor belgeleri̇](https://community.torproject.org/onion-services/setup/install/) adresine veya dağıtım kılavuzunuza bakın.

   - Resmi Tor PPA deposunu yapılandırın:

     ```sh
     CODENAME="$(lsb_release -c | awk '{print $2}')"
     echo "deb [signed-by=/usr/share/keyrings/tor-archive-keyring.gpg] https://deb.torproject.org/torproject.org ${CODENAME} main
     deb-src [signed-by=/usr/share/keyrings/tor-archive-keyring.gpg] https://deb.torproject.org/torproject.org ${CODENAME} main" > /etc/apt/sources.list.d/tor.list
     ```

   - Depo anahtarını içe aktarın:

     ```sh
     curl --proto '=https' --tlsv1.2 -sSf https://deb.torproject.org/torproject.org/A3C4F0F979CAA22CDBA8F512EE8CBC9E886DDD89.asc | gpg --dearmor | tee /usr/share/keyrings/tor-archive-keyring.gpg >/dev/null
     ```

   - Depo dizinini güncelleyin:

     ```sh
     apt update
     ```

   - `tor` paketini yükleyin:

     ```sh
     apt install -y tor deb.torproject.org-keyring
     ```

2. Tor'u yapılandırın:

   - Dosya yapılandırması:
  
     Tor yapılandırmasını tercih ettiğiniz editör ile açın (`nano`, `vim`, `emacs`, vb.):

     ```sh
     vim /etc/tor/torrc
     ```

     Ve aşağıdaki satırları yapılandırmanın altına ekleyin. Lütfen `#` ile başlayan satırlara dikkat edin: bu, her bir seçenekle ilgili yorumlardır.

     ```sh
     # Günlüğü etkinleştir (aksi takdirde, tor onion adresini dağıtmıyor gibi görünüyor)
     Log notice file /var/log/tor/notices.log
     # Tek atlamalı yönlendirmeyi etkinleştirin (aşağıdaki 2 seçenek üçüncünün bağımlılıklarıdır) - Sunucunun daha düşük anonimliği pahasına gecikmeyi azaltacaktır - SMP-sunucu onion adresi istemcilerde genel adresle birlikte kullanıldığından, bu tamamdır. SMP-sunucuyu yalnızca onion adresiyle konuşlandırırsanız, bunun yerine standart yapılandırmayı korumak isteyebilirsiniz.
     SOCKSPort 0
     HiddenServiceNonAnonymousMode 1
     HiddenServiceSingleHopMode 1
     # smp-server gizli hizmet ana bilgisayar dizini ve bağlantı noktası eşlemeleri
     HiddenServiceDir /var/lib/tor/simplex-smp/
     HiddenServicePort 5223 localhost:5223
     ```

   - Dizinler oluşturun:

     ```sh
     mkdir /var/lib/tor/simplex-smp/ && chown debian-tor:debian-tor /var/lib/tor/simplex-smp/ && chmod 700 /var/lib/tor/simplex-smp/
     ```

3. Tor'u başlat:

   `systemd` hizmetini etkinleştirin ve tor'u başlatın. Resmi `tor` ilk başlangıçta biraz inatçıdır ve onion host adresi oluşturmayabilir, bu yüzden her ihtimale karşı yeniden başlatıyoruz.

   ```sh
   systemctl enable --now tor && systemctl restart tor
   ```

4. Onion ana bilgisayarını görüntüleyin:

   Onion ana bilgisayar adresinizi görüntülemek için aşağıdaki komutu çalıştırın:

   ```sh
   cat /var/lib/tor/simplex-smp/hostname
   ```

### SMP PROXY için SOCKS bağlantı noktası

`v5.8.0-beta.0`dan başlayan SMP-sunucu sürümleri, Tor kullanmayan istemcilerin erişebilmesi için yalnızca [Tor](https://www.torproject.org) ağı üzerinden kullanılabilen smp sunucularını PROXY edecek şekilde yapılandırılabilir. Aşağıdaki komutları `root` kullanıcısı olarak çalıştırın.

1. Tor'u [önceki bölümde](#onion-adresi-için-kurulum) açıklandığı gibi yükleyin 

2. Yeni bir Tor daemon örneği oluşturmak için aşağıdaki komutu çalıştırın:

   ```sh
   tor-instance-create tor2
   ```

3. `Tor2` yapılandırmasını açın ve içeriğini aşağıdaki satırlarla değiştirin:

   ```sh
   vim /etc/tor/instances/tor2/torrc
   ```

   ```sh
   # Systemd daemon için tor günlüğü
   Log notice syslog
   # Socks proxy için yerel 9050 bağlantı noktasını dinleyin
   SocksPort 9050
   ```

3. Başlangıçta hizmeti etkinleştirin ve daemon'u başlatın:

   ```sh
   systemctl enable --now tor@tor2
   ```

   Aşağıdaki komut ile `tor2` loglarını kontrol edebilirsiniz:

   ```sh
   journalctl -u tor@tor2
   ```

4. [Sunucu başlatma](#konfigürasyon) işleminden sonra `PROXY` bölümünü aşağıdaki gibi yapılandırın:

   ```ini
   ...
   [PROXY]
   socks_proxy: 127.0.0.1:9050
   own_server_domains: <`log_stats: on` olarak kullanıyorsanız alan adı son ekleriniz>
   ...
   ```

## Sunucu bilgi sayfası

`v5.8.0`dan başlayan SMP-sunucu sürümleri, yönetici bilgisi, sunucu bilgisi, sağlayıcı bilgisi vb. içerebilen sunucu bilgilerini içeren Web sayfası sunmak üzere yapılandırılabilir. Aşağıdaki komutları `root` kullanıcısı olarak çalıştırın.

1. Aşağıdakileri smp-server yapılandırmanıza ekleyin (lütfen [BİLGİ] bölümündeki alanları ilgili bilgileri içerecek şekilde değiştirin):

   ```sh
   vim /etc/opt/simplex/smp-server.ini
   ```

   ```ini
   [WEB]
   static_path: /var/opt/simplex/www

   [INFORMATION]
   # AGPLv3 lisansı, herhangi bir kaynak kodu değişikliğini
   # sunucunun son kullanıcılarına sunmanızı gerektirir.
   # LİSANS: https://github.com/simplex-chat/simplexmq/blob/stable/LICENSE
   # Sunucu kaynak kodunun herhangi bir şekilde değiştirilmesi durumunda doğru kaynak kodu URI'sini ekleyin.
   # Diğer bilgi alanları mevcutsa, kaynak kodu özelliği de mevcut OLMALIDIR.

   source_code: https://github.com/simplex-chat/simplexmq

   # Aşağıdaki tüm bilgilerin bildirilmesi isteğe bağlıdır, bu alanlardan herhangi biri atlanabilir.

   # Sunucu kullanım koşulları ve değişiklikleri.
   # Herhangi bir değişiklikle birlikte standart koşulların ayrı bir belgede kullanılması tavsiye edilir.
   # usage_conditions: https://github.com/simplex-chat/simplex-chat/blob/stable/PRIVACY.md
   # condition_amendments: link

   # Sunucu ve operatörün konumu.
   server_country: <SUNUCU_KONUMUNUZ>
   operator: <ADINIZ>
   operator_country: <KONUMUNUZ>
   website: <EĞER_VARSA_WEBSITE>

   # İdari irtibatlar.
   #admin_simplex: SimpleX address
   admin_email: <EPOSTA>
   # admin_pgp:
   # admin_pgp_fingerprint:

   # Şikayetler ve geri bildirimler için iletişim bilgileri.
   # complaints_simplex: SimpleX address
   complaints_email: <ŞİKAYET_EPOSTASI>
   # complaints_pgp:
   # complaints_pgp_fingerprint:

   # Barındırma sağlayıcısı.
   hosting: <BARINDIRMA_SAĞLAYICISI_ADI>
   hosting_country: <BARINDIRMA_SAĞLAYICISI_KONUMU>
   ```

2. Web sunucusunu yükleyin. Kolay kurulum için Ubuntu sunucu üzerinde [Caddy](https://caddyserver.com) web sunucusunun kurulum sürecini anlatacağız:

   1. Paketleri yükleyin:

      ```sh
      sudo apt install -y debian-keyring debian-archive-keyring apt-transport-https curl
      ```

   2. Depo için caddy gpg anahtarını yükleyin:

      ```sh
      curl -1sLf 'https://dl.cloudsmith.io/public/caddy/stable/gpg.key' | sudo gpg --dearmor -o /usr/share/keyrings/caddy-stable-archive-keyring.gpg
      ```

   3. Caddy deposunu yükleyin:

      ```sh
      curl -1sLf 'https://dl.cloudsmith.io/public/caddy/stable/debian.deb.txt' | sudo tee /etc/apt/sources.list.d/caddy-stable.list
      ```

   4. Caddy'yi yükleyin:

      ```sh
      sudo apt update && sudo apt install caddy
      ```

   [Caddy kurulum talimatları](https://caddyserver.com/docs/install)

3. Caddy yapılandırmasını aşağıdaki ile değiştirin (`<DOMAIN>` yerini değiştirmeyi unutmayın):

   ```sh
   vim /etc/caddy/Caddyfile
   ```

   ```caddy
   <DOMAIN> {
     root * /var/opt/simplex/www
     file_server
   }
   ```

4. Caddy hizmetini etkinleştirin ve başlatın:

   ```sh
   systemctl enable --now caddy
   ```

5. Smp-sunucunuzu en son sürüme yükseltin - [Smp sunucunuzu güncelleme](#smp-sunucunuzu-güncelleme) kılavuzu

6. Tarayıcınızdan dağıttığınız web sayfasına erişin. ini dosyanızda verdiğiniz smp-server bilgilerini görmelisiniz.

## Dokümantasyon

`smp-server` için gerekli tüm dosyalar `/etc/opt/simplex/` klasöründe bulunmaktadır.

Saklanan mesajlar, bağlantılar, istatistikler ve sunucu günlüğü `/var/opt/simplex/` klasöründe bulunur.

### SMP sunucu adresi

SMP sunucu adresi aşağıdaki formata sahiptir:

```
smp://<fingerprint>[:<password>]@<public_hostname>[,<onion_hostname>]
```

- `<fingerprint>`

  Sertifikanızın `smp-server` parmak izi. Sertifika parmak izinizi `/etc/opt/simplex/fingerprint` dosyasından kontrol edebilirsiniz.

- **isteğe bağlı** `<password>`

  Yapılandırılmış `smp-server` parolanız. Yapılandırılmış parolanızı `/etc/opt/simplex/smp-server.ini` dosyasında, `[AUTH]` bölümü altında `create_password:` alanında kontrol edebilirsiniz.

- `<public_hostname>`, **isteğe bağlı** `<onion_hostname>`

  Yapılandırılmış `smp-server` ana bilgisayar ad(lar)ınız. Yapılandırılmış ana bilgisayarlarınızı `/etc/opt/simplex/smp-server.ini` dosyasında, `[TRANSPORT]` bölümü altında `host:` alanında kontrol edebilirsiniz.

### Systemd komutları

Ana bilgisayar açılışında `smp-server`ı başlatmak için çalıştırın:

```sh
sudo systemctl enable smp-server.service

Created symlink /etc/systemd/system/multi-user.target.wants/smp-server.service → /etc/systemd/system/smp-server.service.
```

`smp-server`ı başlatmak için çalıştırın:

```sh
sudo systemctl start smp-server.service
```

`smp-server`ın durumunu kontrol etmek için çalıştırın:

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

`smp-server`ı durdurmak için çalıştırın:

```sh
sudo systemctl stop smp-server.service
```

`smp-server` günlük kuyruğunu kontrol etmek için çalıştırın:

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

### İzleme

`log_stats:` alanındaki `[STORE_LOG]` bölümü altında `/etc/opt/simplex/smp-server.ini` dosyasında `on` değerini ayarlayarak `Grafana` panosu için `smp-server` istatistiklerini etkinleştirebilirsiniz.

Günlükler `/var/opt/simplex/smp-server-stats.daily.log` içinde `csv` dosyası olarak saklanacaktır. `csv` dosyası için alanlar şunlardır:

```sh
fromTime,qCreated,qSecured,qDeleted,msgSent,msgRecv,dayMsgQueues,weekMsgQueues,monthMsgQueues,msgSentNtf,msgRecvNtf,dayCountNtf,weekCountNtf,monthCountNtf,qCount,msgCount,msgExpired,qDeletedNew,qDeletedSecured,pRelays_pRequests,pRelays_pSuccesses,pRelays_pErrorsConnect,pRelays_pErrorsCompat,pRelays_pErrorsOther,pRelaysOwn_pRequests,pRelaysOwn_pSuccesses,pRelaysOwn_pErrorsConnect,pRelaysOwn_pErrorsCompat,pRelaysOwn_pErrorsOther,pMsgFwds_pRequests,pMsgFwds_pSuccesses,pMsgFwds_pErrorsConnect,pMsgFwds_pErrorsCompat,pMsgFwds_pErrorsOther,pMsgFwdsOwn_pRequests,pMsgFwdsOwn_pSuccesses,pMsgFwdsOwn_pErrorsConnect,pMsgFwdsOwn_pErrorsCompat,pMsgFwdsOwn_pErrorsOther,pMsgFwdsRecv,qSub,qSubAuth,qSubDuplicate,qSubProhibited,msgSentAuth,msgSentQuota,msgSentLarge
```

| Alan numarası | Alan adı                     | Alan Açıklaması            |
| ------------- | ---------------------------- | -------------------------- |
| 1             | `fromTime`                   | İstatistik tarihi          |
| Mesajlaşma kuyruğu:                                                       |
| 2             | `qCreated`                   | Oluşturuldu                |
| 3             | `qSecured`                   | Bağlantı kuruldu           |
| 4             | `qDeleted`                   | Silindi                    |
| Mesajlar:                                                                 |
| 5             | `msgSent`                    | Gönderilen                 |
| 6             | `msgRecv`                    | Alınan                     |
| 7             | `dayMsgQueues`               | Günlük aktif kuyruklar     |
| 8             | `weekMsgQueues`              | Haftalık aktif kuyruklar   |
| 9             | `monthMsgQueues`             | Aylık aktif kuyruklar      |
| "Bildirim" işaretli mesajlar                                              |
| 10            | `msgSentNtf`                 | Gönderilen                 |
| 11            | `msgRecvNtf`                 | Alınan                     |
| 12            | `dayCountNtf`                | Günlük aktif kuyruklar     |
| 13            | `weekCountNtf`               | Haftalık aktif kuyruklar   |
| 14            | `monthCountNtf`              | Aylık aktif kuyruklar      |
| Ek istatistikler:                                                         |
| 15            | `qCount`                     | Depolanmış kuyruklar       |
| 16            | `msgCount`                   | Depolanmış mesajlar        |
| 17            | `msgExpired`                 | Süresi dolmuş mesajlar     |
| 18            | `qDeletedNew`                | Yeni silinen kuyruklar     |
| 19            | `qDeletedSecured`            | Güvenli silinmiş kuyruklar |
| Tüm rölelerlerde talep edilen oturumlar:                                  |
| 20            | `pRelays_pRequests`          | - istekler                 |
| 21            | `pRelays_pSuccesses`         | - başarılı                 |
| 22            | `pRelays_pErrorsConnect`     | - bağlantı hataları        |
| 23            | `pRelays_pErrorsCompat`      | - uyumluluk hataları       |
| 24            | `pRelays_pErrorsOther`       | - diğer hatalar            |
| Kendi rölelerinizle talep edilen oturumlar:                               |
| 25            | `pRelaysOwn_pRequests`       | - istekler                 |
| 26            | `pRelaysOwn_pSuccesses`      | - başarılı                 |
| 27            | `pRelaysOwn_pErrorsConnect`  | - bağlantı hataları        |
| 28            | `pRelaysOwn_pErrorsCompat`   | - uyumluluk hataları       |
| 29            | `pRelaysOwn_pErrorsOther`    | - diğer hatalar            |
| Tüm rölelerle iletilmiş mesjalar:                                         |
| 30            | `pMsgFwds_pRequests`         | - istekler                 |
| 31            | `pMsgFwds_pSuccesses`        | - başarılı                 |
| 32            | `pMsgFwds_pErrorsConnect`    | - bağlantı hataları        |
| 33            | `pMsgFwds_pErrorsCompat`     | - uyumluluk hataları       |
| 34            | `pMsgFwds_pErrorsOther`      | - diğer hatalar            |
| Kendi rölelerinizle iletilen mesajlar:                                    |
| 35            | `pMsgFwdsOwn_pRequests`      | - istekler                 |
| 36            | `pMsgFwdsOwn_pSuccesses`     | - başarılı                 |
| 37            | `pMsgFwdsOwn_pErrorsConnect` | - bağlantı hataları        |
| 38            | `pMsgFwdsOwn_pErrorsCompat`  | - uyumluluk hataları       |
| 39            | `pMsgFwdsOwn_pErrorsOther`   | - diğer hatalar            |
| Alınan iletilmiş mesajlar:                                                |
| 40            | `pMsgFwdsRecv`               |                            |
| Mesaj kuyruğu abonelik hataları:                                          |
| 41            | `qSub`                       | Tümü                       |
| 42            | `qSubAuth`                   | Kimlik doğrulama hataları  |
| 43            | `qSubDuplicate`              | Yinelenen SUB hataları     |
| 44            | `qSubProhibited`             | Yasaklanmış SUB hataları   |
| Mesaj hataları:                                                           |
| 45            | `msgSentAuth`                | Kimlik doğrulama hataları  |
| 46            | `msgSentQuota`               | Kota hataları              |
| 47            | `msgSentLarge`               | Büyük mesaj hataları       |

`Csv`yi `Grafana`ya aktarmak için bir kişi yapmalıdır:

1. Grafana eklentisini yükleyin: [Grafana - CSV veri kaynağı](https://grafana.com/grafana/plugins/marcusolsson-csv-datasource/)

2. Aşağıdakileri ekleyerek yerel moda izin verin:

  ```sh
  [plugin.marcusolsson-csv-datasource]
  allow_local_mode = true
  ```

  ... `/etc/grafana/grafana.ini` için

3. Bir CSV veri kaynağı ekleyin:

  - Yan menüde Yapılandırma sekmesine tıklayın (dişli simgesi)
  - Veri Kaynakları sekmesinin sağ üst köşesindeki Veri kaynağı ekle'ye tıklayın
  - CSV veri kaynağını bulmak için arama kutusuna "CSV" girin
  - "CSV" yazan arama sonucuna tıklayın
  - URL alanına CSV içeriğine işaret eden bir dosya girin

4. İşin bitti! İstatistiklerle kendi gösterge tablonuzu oluşturabilmelisiniz.

Daha fazla belge için bkz: [Grafana için CSV Veri Kaynağı - Belgeler](https://grafana.github.io/grafana-csv-datasource/)

## SMP sunucunuzu güncelleme

Smp-server'ınızı en son sürüme güncellemek için kurulum yönteminizi seçin ve adımları izleyin:

   - Manuel dağıtım
     1. Sunucuyu durdurun:
        ```sh
        sudo systemctl stop smp-server
        ```
     2. Binary dosyasını güncelleyin:
        ```sh
         curl -L https://github.com/simplex-chat/simplexmq/releases/latest/download/smp-server-ubuntu-20_04-x86-64 -o /usr/local/bin/smp-server && chmod +x /usr/local/bin/smp-server
        ```
     3. Sunucuyu başlatın:
        ```sh
        sudo systemctl start smp-server
        ```

   - [Resmi kurulum betiği](https://github.com/simplex-chat/simplexmq#using-installation-script)
     1. Aşağıdaki komutu çalıştırın:
        ```sh
        sudo simplex-servers-update
        ```
     2. Tamamdır!

   - [Docker konteyner](https://github.com/simplex-chat/simplexmq#using-docker)
     1. Durdurun ve konteyner'ı kaldırın
        ```sh
        docker rm $(docker stop $(docker ps -a -q --filter ancestor=simplexchat/smp-server --format="\{\{.ID\}\}"))
        ```
     2. Güncel görüntüyü çekin:
        ```sh
        docker pull simplexchat/smp-server:latest
        ```
     3. Yeni konteyner başlatın:
        ```sh
        docker run -d \
          -p 5223:5223 \
          -v $HOME/simplex/smp/config:/etc/opt/simplex:z \
          -v $HOME/simplex/smp/logs:/var/opt/simplex:z \
          simplexchat/smp-server:latest
        ```

   - [Linode Marketyeri](https://www.linode.com/marketplace/apps/simplex-chat/simplex-chat/)
     1. Güncel görüntüyü çekin:
        ```sh
        docker-compose --project-directory /etc/docker/compose/simplex pull
        ```
     2. Konteynerleri yeniden başlatın:
        ```sh
        docker-compose --project-directory /etc/docker/compose/simplex up -d --remove-orphans
        ```
     3. Eski görüntüleri kaldırın:
        ```sh
        docker image prune
        ```

## Uygulamayı sunucuyu kullanacak şekilde yapılandırma

Uygulamayı mesajlaşma sunucunuzu kullanacak şekilde yapılandırmak için şifre dahil tam adresi kopyalayın ve uygulamaya ekleyin. Sunucunuzu önceden ayarlanmış sunucularla birlikte veya onlar olmadan kullanma seçeneğiniz vardır - bunları kaldırabilir veya devre dışı bırakabilirsiniz.

Sunucunuzun adresini, sunucu ayarlarından QR kodunu taramalarına izin vererek arkadaşlarınızla paylaşmanız da mümkündür - bu sunucu şifresini içerecektir, böylece sunucunuz aracılığıyla da mesaj alabileceklerdir.

_Lütfen dikkat_: parola desteğine sahip olmak için SMP sunucu sürüm 4.0'a ihtiyacınız vardır. Zaten konuşlandırılmış bir sunucunuz varsa, sunucu INI dosyasını düzenleyerek parola ekleyebilirsiniz.

<img src="./server_config_1.png" width="288"> &nbsp;&nbsp; <img src="./server_config_2.png" width="288"> &nbsp;&nbsp; <img src="./server_config_3.png" width="288">
