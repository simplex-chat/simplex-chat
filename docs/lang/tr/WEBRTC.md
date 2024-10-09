---
title: SimpleX Chat'te özel WebRTC ICE sunucularını kullanmak
revision: 31.01.2023
---

| Son Güncellenme 31.01.2023 | Diller: TR, [EN](/docs/WEBRTC.md), [FR](/docs/lang/fr/WEBRTC.md), [CZ](/docs/lang/cs/WEBRTC.md), [PL](/docs/lang/pl/WEBRTC.md) |

# SimpleX Chat'te özel WebRTC ICE sunucularını kullanma

## STUN/TURN sunucusunu dağıtmak

Bu kılavuz için, en özellikli ve savaşta test edilmiş STUN/TURN sunucu uygulaması olan [`coturn`](https://github.com/coturn/coturn)'u ve [`Ubuntu 20.04 LTS`](https://ubuntu.com/download/server) Linux dağıtımını kullanacağız.

0. `stun.$YOUR_DOMAIN` ve `turn.$YOUR_DOMAIN` sertifikalarını edinin.

   Bunun için [Let's Encrypt](https://letsencrypt.org/getting-started/) kullanıyoruz.

1. Ana repodan `coturn` paketini yükleyin.

```sh
apt update && apt install coturn`
```

2. `/etc/default/coturn` dosyasındaki `TURNSERVER_ENABLED=1` seçeneğini uncomment edin:

```sh
sed -i '/TURN/s/^#//g' /etc/default/coturn
```

3. `coturn` dosyasını `/etc/turnserver.conf` dosyasında yapılandırın:

   Ayrıca, lütfen her bir seçenek için yorumlara bakınız.

```sh
# TLS için 443 bağlantı noktasını dinleyin
alt-tls-listening-port=443
# TURN mesajlarında parmak izi kullanın
fingerprint
# Uzun vadeli kimlik bilgileri mekanizması kullanın
lt-cred-mech
# Kimlik bilgileriniz
user=$YOUR_LOGIN:$YOUR_PASSWORD
# Sunucu alan adınız
server-name=$YOUR_DOMAIN
# Açık bir origin/realm ilişkisi bulunamadığında kullanıcılar için kullanılacak varsayılan realm
realm=$YOUR_DOMAIN
# Sertifikalarınıza giden yol. cotun işlem kullanıcısı/grup tarafından okunabilir olduklarından emin olun
cert=/var/lib/turn/cert.pem
pkey=/var/lib/turn/key.pem
# 2066 bit önceden tanımlanmış DH TLS anahtarı kullanın
dh2066
# journalctl için log
syslog
# coturn hizmetini çalıştıracak kullanıcı/grup
proc-user=turnserver
proc-group=turnserver
# Zayıf şifrelemeyi devre dışı bırakalım
no-tlsv1
no-tlsv1_1
no-tlsv1_2
```

4. `coturn` hizmetini başlatın ve etkinleştirin:

```sh
systemctl enable coturn && systemctl start coturn
```

5. İsteğe bağlı olarak, `ufw` güvenlik duvarı kullanıyorsanız, ilgili bağlantı noktalarını açın:

- **3478** – "plain" TURN/STUN;
- **5349** – TURN/STUN over TLS;
- **443** – TURN/STUN over TLS, bu da güvenlik duvarlarını aşabilir;
- **49152:65535** – Coturn'un TURN aktarımı için varsayılan olarak kullanacağı bağlantı noktası aralığı.

```sh
# Ubuntu için
sudo ufw allow 3478 && \
sudo ufw allow 443 && \
sudo ufw allow 5349 && \
sudo ufw allow 49152:65535/tcp && \
sudo ufw allow 49152:65535/udp

# Fedora için
sudo firewall-cmd --permanent --add-port=443/tcp && \
sudo firewall-cmd --permanent --add-port=443/udp && \
sudo firewall-cmd --permanent --add-port=5349/tcp && \
sudo firewall-cmd --permanent --add-port=5349/udp && \
sudo firewall-cmd --permanent --add-port=49152:65535/tcp && \
sudo firewall-cmd --permanent --add-port=49152:65535/udp && \
sudo firewall-cmd --reload
```

## Mobil uygulamaları yapılandırmak

Mobil uygulamanızı sunucunuzu kullanacak şekilde yapılandırmak için:

1. `Ayarlar / Ağ ve Sunucular / WebRTC ICE sunucuları`nı açın ve `ICE sunucularını yapılandır` seçeneğini değiştirin.

2. Tüm sunucu adreslerini her satıra bir tane olmak üzere alana girin, örneğin sunucularınız 5349 numaralı bağlantı noktasındaysa:

```
stun:stun.example.com:5349
turn:username:password@turn.example.com:5349
```

İşte bu kadar - artık sunucularımızla herhangi bir veri paylaşmadan (E2E şifreli mesajlarda kişinizle anahtar değişimi dışında) kendi sunucunuz üzerinden sesli ve görüntülü aramalar yapabilirsiniz.

## Sorun Giderme

- **Sunucunun kullanılabilir olup olmadığını belirleyin**:

  Bu komutu terminalinizde çalıştırın:

  ```sh
  ping <domain_veya_ip_adresiniz>
  ```

  Paketler iletiliyorsa sunucu çalışıyor demektir!

- **Portların açık olup olmadığını belirleyin**:

  Bu komutu terminalinizde çalıştırın:

  ```sh
  nc -zvw10 <domain_veya_ip_adresiniz> 443 5349
  ```

  Şunu görmeyi beklemelisiniz:

  ```
  Connection to <domain_veya_ip_adresiniz> 443 port [tcp/https] succeeded!
  Connection to <domain_veya_ip_adresiniz> 5349 port [tcp/*] succeeded!
  ```

- **Test STUN/TURN bağlantısı**:

  1. [IceTest](https://icetest.info/) adresine gidin.

  2. **ICE Sunucu Listesini Oluştur** bölümüne ekleyin:

     <img src="./stun_1.png">

     - `STUN: stun:<domain_veya_ip_adresiniz>:<port>` ve `Add STUN` tuşuna basın
     - `TURN: turn:<domain_veya_ip_adresiniz>:<port>`, `Username: <kullanıcı_adınız>`, `Credential: <şifreniz>` ve ardından `Add TURN` tuşuna basın

     Burada `<port>` 443 veya 5349'dur.

  3. Sunucularınızı **ICE sunucu listesi** bölümünde görmelisiniz. Her şey doğru şekilde ayarlandıysa, `Start test` düğmesine basın:

     <img src="./stun_2.png">

  4. **Sonuçlar** bölümünde şuna benzer bir şey görmelisiniz:

     <img src="./stun_3.png">

     Sonuçlar `srflx` ve `relay` adaylarını gösteriyorsa, her şey doğru ayarlanmış demektir!

