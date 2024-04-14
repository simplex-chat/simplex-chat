---
title: Použití vlastních serverů WebRTC ICE v SimpleX Chat
revision: 31.01.2023
---
| Aktualizováno 31.01.2023 | Jazyky: CZ, [EN](/docs/WEBRTC.md), [FR](/docs/lang/fr/WEBRTC.md), [PL](/docs/lang/pl/WEBRTC.md) |

# Použití vlastních serverů WebRTC ICE v SimpleX Chat

## Nasazení serveru STUN/TURN

V tomto průvodci budeme používat nejvybavenější a nejosvědčenější implementaci serveru STUN/TURN - [`coturn`](https://github.com/coturn/coturn) a [`Ubuntu 20.04 LTS`](https://ubuntu.com/download/server) distribuci Linuxu.

0. Získejte certifikáty `stun.$Vaše_doména` a `turn.$Vaše_doména`.

   Používáme [Let's Encrypt](https://letsencrypt.org/getting-started/).

1. Nainstalujte balíček `coturn` z hlavního repozitáře.

```sh
apt update && apt install coturn`.
```

2. Odkomentujte `TURNSERVER_ENABLED=1` z `/etc/default/coturn`:

```sh
sed -i '/TURN/s/^#//g' /etc/default/coturn
```

3. Konfigurace `coturn` v souboru `/etc/turnserver.conf`:

   Viz také komentáře k jednotlivým volbám.

```sh
# Naslouchejte také na portu 443 pro tls.
alt-tls-listening-port=443
# Použijte otisky prstů ve zprávách TURN
fingerprint
# Použijte mechanismus dlouhodobých pověření
lt-cred-mech
# Vaše pověření
user=$YOUR_LOGIN:$YOUR_PASSWORD
# Vaše doména serveru
server-name=$YOUR_DOMAIN
# Výchozí sféra, která bude použita pro uživatele, pokud nebyl nalezen explicitní vztah origin/realm
realm=$YOUR_DOMAIN
# Cesta k vašim certifikátům. Ujistěte se, že jsou čitelné pro proces cotun user/group
cert=/var/lib/turn/cert.pem
pkey=/var/lib/turn/key.pem
# Použijte 2066 bitů předdefinovaného DH klíče TLS
dh2066
# Přihlaste se do journalctl
syslog
# Uživatel/skupina, která bude provozovat službu coturn
proc-user=turnserver
proc-group=turnserver
# Zakázat slabé šifrování
no-tlsv1
no-tlsv1_1
no-tlsv1_2
```

4. Spusťte a povolte službu `coturn`:

```sh
systemctl enable coturn && systemctl start coturn
```

5. Pokud používáte firewall `ufw`, otevřete případně příslušné porty:

- **3478** - "obyčejný" TURN/STUN;
- **5349** - TURN/STUN přes TLS;
- **443** - TURN/STUN přes TLS, který může obejít brány firewall;
- **49152:65535** - rozsah portů, který bude společnost Coturn ve výchozím nastavení používat pro přenos TURN.

```sh
# Pro Ubuntu
sudo ufw allow 3478 && \
sudo ufw allow 443 && \
sudo ufw allow 5349 && \
sudo ufw allow 49152:65535/tcp && \
sudo ufw allow 49152:65535/udp

# Pro Fedora
sudo firewall-cmd --permanent --add-port=443/tcp && \
sudo firewall-cmd --permanent --add-port=443/udp && \
sudo firewall-cmd --permanent --add-port=5349/tcp && \
sudo firewall-cmd --permanent --add-port=5349/udp && \
sudo firewall-cmd --permanent --add-port=49152:65535/tcp && \
sudo firewall-cmd --permanent --add-port=49152:65535/udp && \
sudo firewall-cmd --reload
```

## Konfigurace mobilních aplikací

Konfigurace mobilní aplikace pro použití vašeho serveru:

1. Otevřete `Nastavení / Síť a servery / WebRTC ICE servery` a přepněte přepínač `Konfigurovat ICE servery`.

2. Do pole zadejte všechny adresy serverů, jednu na řádek, například pokud máte servery na portu 5349:

```
stun:stun.example.com:5349
turn:username:password@turn.example.com:5349
```

To je vše - nyní můžete uskutečňovat audio a video hovory prostřednictvím vlastního serveru, aniž byste s našimi servery sdíleli jakákoli data (kromě výměny klíčů s kontaktem v šifrovaných zprávách E2E).

## Řešení problémů

- **Zjistěte, zda je server dostupný**:

  Spusťte tento příkaz v terminálu:

  ```sh
  ping <vaše_ip_nebo_doména>
  ```

  Pokud jsou pakety přenášeny, server je v provozu!

- **Zjistěte, zda jsou otevřené porty**:

  Spusťte tento příkaz v terminálu:

  ```sh
  nc -zvw10 <vaše_ip_nebo_doména> 443 5349
  ```

  Měli byste vidět:

  ```
  Připojení k portu <vaše_ip_nebo_doména> 443 [tcp/https] se podařilo!
  Připojení k <vaše_ip_nebo_doména> 5349 port [tcp/*] uspělo!
  ```

- **Test připojení STUN/TURN**:

  1. Přejděte na [IceTest](https://icetest.info/).

  2. Do části **Sestavit seznam serverů ICE** přidejte:

     <img src="/docs/stun_1.png">.

     - `STUN: stun:<vaše_ip_nebo_doména>:<port>` a stiskněte `Add STUN`.
     - `TURN: turn:<vaše_ip_nebo_doména>:<port>`, `Username: <vaše_přihlašovací jméno>`, `Credential: <vaš_pas>` a stiskněte `Add TURN`

     Kde `<port>` je 443 nebo 5349.

  3. Měli byste vidět své servery v sekci **ICE server list**. Pokud je vše správně nastaveno, stiskněte `Start test`:

     <img src="/docs/stun_2.png">

  4. V části **Výsledky** byste měli vidět něco takového:

     <img src="/docs/stun_3.png">

     Pokud výsledky zobrazují kandidáty `srflx` a `relay`, je vše nastaveno správně!
