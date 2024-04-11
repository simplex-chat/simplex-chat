| Zaktualizowano 31.01.2023 | Języki: PL, [EN](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/WEBRTC.md), [FR](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/fr/WEBRTC.md), [CZ](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/cz/WEBRTC.md) |

# Używanie własnych serwerów WebRTC ICE w SimpleX Chat

## Wdrożenie serwera STUN/TURN

W tym poradniku użyjemy najbardziej funkcjonalnej i sprawdzonej w boju implementacji serwera STUN/TURN - [`coturn`](https://github.com/coturn/coturn) i [`Ubuntu 20.04 LTS`](https://ubuntu.com/download/server) dystrybucji Linuksa.

0. Uzyskaj certyfikaty `stun.$YOUR_DOMAIN` i `turn.$YOUR_DOMAIN`.

   Korzystamy z [Let's Encrypt](https://letsencrypt.org/getting-started/).

1. Zainstaluj pakiet `coturn` z głównego repozytorium.

```sh
apt update && apt install coturn`
```

2. Usuń komentarz `TURNSERVER_ENABLED=1` z `/etc/default/coturn`:

```sh
sed -i '/TURN/s/^#//g' /etc/default/coturn
```

3. Skonfiguruj `coturn` w `/etc/turnserver.conf`:

   Prosimy również o zapoznanie się z komentarzami do poszczególnych opcji.

```sh
# Również nasłuchuj na porcie 443 dla tls
alt-tls-listening-port=443
# Użyj fingerprintów w wiadomościach TURN
fingerprint
# Użyj mechanizmu długoterminowych poświadczeń
lt-cred-mech
# Twoje dane uwierzytelniające
user=$YOUR_LOGIN:$YOUR_PASSWORD
# Domena Twojego serwera
server-name=$YOUR_DOMAIN
# Domyślna przestrzeń, która będzie używana dla użytkowników, jeśli nie znaleziono wyraźnej relacji origin/realm
realm=$YOUR_DOMAIN
# Ścieżka do twoich certyfikatów. Upewnij się, że są one czytelne dla użytkownika/grupy procesu cotun
cert=/var/lib/turn/cert.pem
pkey=/var/lib/turn/key.pem
# Użyj 2066 bitów predefiniowanego klucza DH TLS
dh2066
# Zaloguj się do journalctl
syslog
# Użytkownik/grupa, która będzie uruchamiać usługę
proc-user=turnserver
proc-group=turnserver
# Wyłącz słabe szyfrowanie
no-tlsv1
no-tlsv1_1
no-tlsv1_2
```

4. Uruchom i włącz usługę `coturn`.:

```sh
systemctl enable coturn && systemctl start coturn
```

5. Opcjonalnie, jeśli używasz firewall `ufw`, otwórz odpowiednie porty::

- **3478** – "plain" TURN/STUN;
- **5349** – TURN/STUN over TLS;
- **443** – TURN/STUN over TLS, który mogą ominąć firewalle;
- **49152:65535** – zakres portów, których Coturn będzie domyślnie używał dla przekaźnika TURN.

```sh
ufw allow 3478 && \
ufw allow 443 && \
ufw allow 5349 && \
ufw allow 49152:65535/tcp && \
ufw allow 49152:65535/udp
```

## Configure mobile apps

Aby skonfigurować swoją aplikację mobilną do korzystania z Twojego serwera:

1. Otwórz `Twoje ustawienia / Sieci i serwery / Serwery WebRTC ICE` i przełącz `Skonfiguruj serwery ICE`.

2 Wprowadź wszystkie adresy serwerów w polu, po jednym w każdym wierszu, na przykład jeśli serwery są na porcie 5349:

```
stun:stun.example.com:5349
turn:username:password@turn.example.com:5349
```

To jest to - teraz możesz prowadzić rozmowy audio i wideo za pośrednictwem własnego serwera, nie udostępniając żadnych danych naszym serwerom (poza wymianą klucza z Twoim kontaktem w wiadomościach szyfrowanych E2E).

## Rozwiązywanie problemów

- **Określ, czy serwer jest dostępny**:

  Uruchom w terminalu to polecenie:

  ```sh
  ping <your_ip_or_domain>
  ```

  Jeśli pakiety są przesyłane, serwer jest w stanie gotowości!

- **Określ, czy porty są otwarte**:

  Uruchom w terminalu to polecenie:

  ```sh
  nc -zvw10 <your_ip_or_domain> 443 5349
  ```

  Powinieneś zobaczyć:

  ```
  Connection to <your_ip_or_domain> 443 port [tcp/https] succeeded!
  Connection to <your_ip_or_domain> 5349 port [tcp/*] succeeded!
  ```

- **Przetestuj łączność STUN/TURN**:

  1. Przejdź do [IceTest](https://icetest.info/).

  2. W sekcji **Build up ICE Server List** dodaj::

     <img src="https://github.com/simplex-chat/simplex-chat/blob/stable/docs/stun_1.png">

     - `STUN: stun:<your_ip_or_domain>:<port>` i naciśnij `Add STUN`
     - `TURN: turn:<your_ip_or_domain>:<port>`, `Username: <your_login>`, `Credential: <your_pass>` i naciśnij `Add TURN`

     Gdzie `<port>` to 443 lub 5349.

  3. Powinieneś zobaczyć swoje serwery w sekcji **ICE server list**. Jeśli wszystko jest skonfigurowane poprawnie, naciśnij `START TEST`:

     <img src="https://github.com/simplex-chat/simplex-chat/blob/stable/docs/stun_2.png">

  4. W sekcji **Results** powinieneś zobaczyć coś takiego:

     <img src="https://github.com/simplex-chat/simplex-chat/blob/stable/docs/stun_3.png">

     Jeśli wyniki pokazują kandydatów `srflx` i `relay`, wszystko jest ustawione poprawnie!
