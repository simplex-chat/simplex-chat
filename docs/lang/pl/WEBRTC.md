---
title: Korzystanie z niestandardowych serwerów WebRTC ICE w SimpleX Chat
revision: 31.01.2023
---

| Updated 31.01.2023 | Languages: EN, [FR](/docs/lang/fr/WEBRTC.md), [CZ](/docs/lang/cs/WEBRTC.md), [PL](/docs/lang/pl/WEBRTC.md) |

# Korzystanie z niestandardowych serwerów WebRTC ICE w SimpleX Chat

## Instalacja serwera STUN/TURN

W tym poradniku będziemy używać najbardziej funkcjonalnej i przetestowanej w boju implementacji serwera STUN/TURN - [`coturn`](https://github.com/coturn/coturn) i [Ubuntu 20.04 LTS`](https://ubuntu.com/download/server) dystrybucji Linuksa.

0. Uzyskaj certyfikaty `stun.$TWOJA_DOMENA` i `turn.$TWOJA_DOMENA`.

   Używamy [Let's Encrypt](https://letsencrypt.org/getting-started/).

1. Zainstaluj pakiet `coturn` z głównego repozytorium.

```sh
apt update && apt install coturn`
```

2. Odkomentuj `TURNSERVER_ENABLED=1` z `/etc/default/coturn`:

```sh
sed -i '/TURN/s/^#//g' /etc/default/coturn
```

3. Skonfiguruj `coturn` w `/etc/turnserver.conf`:

   Zobacz również komentarze dotyczące poszczególnych opcji.

```sh
# Nasłuchuj również na porcie 443 dla tls
alt-tls-listening-port=443
# Używaj odcisków palców w komunikatach TURN
fingerprint
# Użyj mechanizmu poświadczeń długoterminowych
lt-cred-mech
# Twoje poświadczenia
user=$YOUR_LOGIN:$YOUR_PASSWORD
# Domena Twojego serwera
server-name=$YOUR_DOMAIN
# Domyślny obszar, który ma być używany dla użytkowników, gdy nie znaleziono wyraźnej relacji pochodzenie/obszar
realm=$YOUR_DOMAIN
# Ścieżka do Twoich certyfikatów. Upewnij się, że są one czytelne dla użytkownika/grupy procesu cotun.
cert=/var/lib/turn/cert.pem
pkey=/var/lib/turn/key.pem
# Użyj predefiniowanego klucza DH TLS o długości 2066 bitów
dh2066
# Logowanie do journalctl
syslog
# Użytkownik/grupa, która będzie uruchamiać usługę coturn
proc-user=turnserver
proc-group=turnserver
# Wyłącz słabe szyfrowanie
no-tlsv1
no-tlsv1_1
no-tlsv1_2
```

4. Uruchom i włącz serwis `coturn`:

```sh
systemctl enable coturn && systemctl start coturn
```

5. Opcjonalnie, jeśli używasz firewalla `ufw`, otwórz odpowiednie porty:

- **3478** – "czysty" TURN/STUN;
- **5349** – TURN/STUN over TLS;
- **443** – TURN/STUN over TLS, który może omijać firewalle;
- **49152:65535** – zakres portów, który Coturn będzie domyślnie wykorzystywał dla przekaźnika TURN.

```sh
# Dla Ubuntu
sudo ufw allow 3478 && \
sudo ufw allow 443 && \
sudo ufw allow 5349 && \
sudo ufw allow 49152:65535/tcp && \
sudo ufw allow 49152:65535/udp

# Dla Fedory
sudo firewall-cmd --permanent --add-port=443/tcp && \
sudo firewall-cmd --permanent --add-port=443/udp && \
sudo firewall-cmd --permanent --add-port=5349/tcp && \
sudo firewall-cmd --permanent --add-port=5349/udp && \
sudo firewall-cmd --permanent --add-port=49152:65535/tcp && \
sudo firewall-cmd --permanent --add-port=49152:65535/udp && \
sudo firewall-cmd --reload
```

## Konfiguracja aplikacji mobilnych

Aby skonfigurować aplikację mobilną do korzystania z serwera:

1. Otwórz `Ustawienia / Sieć i serwery / Serwery WebRTC ICE` i przełącz przełącznik `Konfiguruj serwery ICE`.

2. Wprowadź wszystkie adresy serwerów w polu, po jednym na linię, na przykład jeśli serwery znajdują się na porcie 5349:

```
stun:stun.example.com:5349
turn:username:password@turn.example.com:5349
```

To tyle - teraz możesz wykonywać połączenia audio i wideo za pośrednictwem własnego serwera, bez udostępniania jakichkolwiek danych naszym serwerom (poza wymianą kluczy z kontaktem w szyfrowanych wiadomościach E2E).

## Rozwiązywanie problemów

- **Określ czy Twój serwer jest dostępny**:

  Uruchom to polecenie w terminalu:

  ```sh
  ping <twoje_ip_lub_domena>
  ```

  Jeśli pakiety są transmitowane, serwer działa!

- **Określ czy porty są otwarte**:

  Uruchom to polecenie w terminalu:

  ```sh
  nc -zvw10 <twoje_ip_lub_domena> 443 5349
  ```

  Powinno się pojawić:

  ```
  Connection to <twoje_ip_lub_domena> 443 port [tcp/https] succeeded!
  Connection to <twoje_ip_lub_domena> 5349 port [tcp/*] succeeded!
  ```

- **Test połączenia STUN/TURN**:

  1. Wejdź na [IceTest](https://icetest.info/).

  2. W sekcji **Build up ICE Server List** dodaj:

     <img src="./stun_1.png">

     - `STUN: stun:<twoje_ip_lub_domena>:<port>` kliknij `Add STUN`
     - `TURN: turn:<twoje_ip_lub_domena>:<port>`, `Username: <twój_login>`, `Credential: <twoje_hasło>` kliknij `Add TURN`

     Gdzie `<port>` to 443 lub 5349.

  3. Powinieneś zobaczyć swoje serwery w sekcji **ICE server list**. Jeśli wszystko jest skonfigurowane poprawnie, naciśnij `Start test`:

     <img src="./stun_2.png">

  4. W sekcji **Results** powinieneś zobaczyć coś takiego:

     <img src="./stun_3.png">

     Jeśli wyniki pokazują `srflx` i `relay`, wszystko jest skonfigurowane poprawnie!

