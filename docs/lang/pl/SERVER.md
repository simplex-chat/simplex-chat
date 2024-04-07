---
title: Hostowanie własnego serwera SMP
revision: 31.07.2023
---

| Updated 05.06.2023 | Języki: PL, [EN](/docs/SERVER.md), [FR](/docs/lang/fr/SERVER.md), [CZ](/docs/lang/cs/SERVER.md) |

# Hostowanie własnego serwera SMP

## Informacje ogólne

Serwer SMP to serwer przekaźnikowy używany do przekazywania wiadomości w sieci SimpleX. Aplikacje SimpleX Chat mają wstępnie ustawione serwery (dla aplikacji mobilnych są to smp11, smp12 i smp14.simplex.im), ale można łatwo zmienić konfigurację aplikacji, aby korzystać z innych serwerów.

Klienty SimpleX określają tylko, który serwer jest używany do odbierania wiadomości, oddzielnie dla każdego kontaktu (lub połączenia grupowego z członkiem grupy), a serwery te są tylko tymczasowe, ponieważ adres dostawy może ulec zmianie.

_Uwaga_: gdy zmienisz serwery w ustawieniach aplikacji, wpłynie to tylko na to, który serwer będzie używany dla nowych kontaktów, istniejące kontakty nie zostaną automatycznie przeniesione na nowe serwery, ale możesz przenieść je ręcznie za pomocą przycisku ["Zmień adres odbiorczy"](../blog/20221108-simplex-chat-v4.2-security-audit-new-website.md#change-your-delivery-address-beta) na stronie z informacjami kontaktu/członka - wkrótce zostanie to zautomatyzowane.

## Instalacja

1. Najpierw zainstaluj `smp-server`:

   - Manualna instalacja (patrz niżej)

   - Półautomatyczna instalacja:
     - [Oficjalny skrypt instalacyjny](https://github.com/simplex-chat/simplexmq#using-installation-script)
     - [Kontener Dockera](https://github.com/simplex-chat/simplexmq#using-docker)
     - [Linode Marketplace](https://www.linode.com/marketplace/apps/simplex-chat/simplex-chat/)

Instalacja ręczna wymaga kilku kroków wstępnych:

1. Zainstaluj binarkę:

   - Używając oficjalnych binarek:

     ```sh
     curl -L https://github.com/simplex-chat/simplexmq/releases/latest/download/smp-server-ubuntu-20_04-x86-64 -o /usr/local/bin/smp-server && chmod +x /usr/local/bin/smp-server
     ```

   - Budowanie z kodu źródłowego:

     Zobacz [Build from source: Using your distribution](https://github.com/simplex-chat/simplexmq#using-your-distribution)

2. Utwórz użytkownika i grupę dla `smp-server`:

   ```sh
   sudo useradd -m smp
   ```

3. Utwórz niezbędne katalogi i przypisz uprawnienia:

   ```sh
   sudo mkdir -p /var/opt/simplex /etc/opt/simplex
   sudo chown smp:smp /var/opt/simplex /etc/opt/simplex
   ```

4. Zezwól na port `smp-server` w firewallu:

   ```sh
   # Dla Ubuntu
   sudo ufw allow 5223/tcp
   # Dla Fedory
   sudo firewall-cmd --permanent --add-port=5223/tcp && \
   sudo firewall-cmd --reload
   ```

5. **Opcjonalnie** - Jeśli używasz dystrybucji z `systemd`, utwórz plik `/etc/systemd/system/smp-server.service` z następującą zawartością:

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

   I uruchom `sudo systemctl daemon-reload`.

## Instalacja Tora

smp-server można również zainstalować jako serwer działający w sieci [tor](https://www.torproject.org). Uruchom następujące polecenia jako użytkownik `root`.

1. Zainstaluj Tor:

   Zakładamy, że używasz dystrybucji opartych na Ubuntu/Debian. Jeśli nie, zapoznaj się z [oficjalną dokumentacją tor](https://community.torproject.org/onion-services/setup/install/) lub poradnikiem dla Twojej dystrybucji.

   - Skonfiguruj oficjalne repozytorium Tor PPA:

     ```sh
     CODENAME="$(lsb_release -c | awk '{print $2}')"
     echo "deb [signed-by=/usr/share/keyrings/tor-archive-keyring.gpg] https://deb.torproject.org/torproject.org ${CODENAME} main
     deb-src [signed-by=/usr/share/keyrings/tor-archive-keyring.gpg] https://deb.torproject.org/torproject.org ${CODENAME} main" > /etc/apt/sources.list.d/tor.list
     ```

   - Zimportuj klucz repozytorium:

     ```sh
     curl --proto '=https' --tlsv1.2 -sSf https://deb.torproject.org/torproject.org/A3C4F0F979CAA22CDBA8F512EE8CBC9E886DDD89.asc | gpg --dearmor | tee /usr/share/keyrings/tor-archive-keyring.gpg >/dev/null
     ```

   - Zaktualizuj indeks repozytorium:

     ```sh
     apt update
     ```

   - Zainstaluj paczkę `tor`:

     ```sh
     apt install -y tor deb.torproject.org-keyring
     ```

2. Skonfiguruj tor:

   - Konfiguracja pliku:
  
     Otwórz konfigurację tora w wybranym edytorze (`nano`,`vim`,`emacs`, itp.):

     ```sh
     vim /etc/tor/torrc
     ```

     I umieść następujące linie na dole konfiguracji. Zwróć uwagę na linie zaczynające się od `#`: są to komentarze dotyczące poszczególnych opcji.

     ```sh
     # Włącz logowanie (w przeciwnym razie tor nie wyda adresu onion).
     Log notice file /var/log/tor/notices.log
     # Włącz routowanie single hop (2 opcje poniżej są zależne od trzeciej). Zmniejszy to opóźnienie w zamian za anonimowość (jako że tor działa równolegle z serwerem smp i adres onion będzie wyświetlany w klientach, jest to całkowicie w porządku).
     SOCKSPort 0
     HiddenServiceNonAnonymousMode 1
     HiddenServiceSingleHopMode 1
     # Katalog hostów i mapowanie portów usługi ukrytej smp-server
     HiddenServiceDir /var/lib/tor/simplex-smp/
     HiddenServicePort 5223 localhost:5223
     ```

   - Utwórz katalogi:

     ```sh
     mkdir /var/lib/tor/simplex-smp/ && chown debian-tor:debian-tor /var/lib/tor/simplex-smp/ && chmod 700 /var/lib/tor/simplex-smp/
     ```

3. Uruchom tor:

   Włącz usługę `systemd` oraz uruchom tor. Oficjalny `tor` jest nieco kłopotliwy przy pierwszym uruchomieniu i może nie utworzyć adresu hosta cebuli, więc na wszelki wypadek uruchamiamy go ponownie.

   ```sh
   systemctl enable tor && systemctl start tor && systemctl restart tor
   ```

4. Wyświetla onionowego hosta:

   Wykonaj następujące polecenie, aby wyświetlić adres onionowego hosta:

   ```sh
   cat /var/lib/tor/simplex-smp/hostname
   ```

## Configuration

Aby zobaczyć, jakie opcje są dostępne, wykonaj `smp-server` bez flag:

```sh
sudo su smp -c smp-server

...
Available commands:
  init                     Initialize server - creates /etc/opt/simplex and
                           /var/opt/simplex directories and configuration files
  start                    Start server (configuration:
                           /etc/opt/simplex/smp-server.ini)
  delete                   Delete configuration and log files
```

Możesz uzyskać dalszą pomoc, wykonując polecenie `sudo su smp -c "smp-server <command> -h"`

Następnie musimy skonfigurować `smp-server`:

### Interaktywnie

Wykonaj poniższe polecenie:

```sh
sudo su smp -c "smp-server init"
```

Istnieje kilka opcji, które należy rozważyć:

- `Enable store log to restore queues and messages on server restart (Yn):`

  Wpisz `y`, aby włączyć zapisywanie i przywracanie połączeń i wiadomości po ponownym uruchomieniu serwera.

  _Uwaga_: ważne jest, aby użyć SIGINT do ponownego uruchomienia serwera, ponieważ w przeciwnym razie niedostarczone wiadomości nie zostaną przywrócone. Połączenia zostaną przywrócone niezależnie od sposobu ponownego uruchomienia serwera, ponieważ w przeciwieństwie do wiadomości są one dodawane do dziennika append-only.

- `Enable logging daily statistics (yN):`

  Wpisz `y`, aby włączyć logowanie statystyk w formacie CSV, mogą one być przykładowo użyte do pokazania wykresów użycia w `Grafana`.

Statystyki te obejmują dzienną liczbę utworzonych, zabezpieczonych i usuniętych kolejek, wysłanych i odebranych wiadomości, a także dzienną, tygodniową i miesięczną liczbę aktywnych kolejek (tj. kolejek, które były używane do wysyłania wiadomości). Uważamy, że informacje te nie zawierają niczego, co pozwoliłoby na skorelowanie różnych kolejek jako należących bezpośrednio do użytkowników, ale prosimy o poufne poinformowanie nas, jeśli uważasz, że można to w jakikolwiek sposób nadużyć.

- `Require a password to create new messaging queues?`

  Wpisz `r` lub dowolne hasło, aby zabezpieczyć hasłem `smp-server`, lub `n`, aby wyłączyć ochronę hasłem.

- `Enter server FQDN or IP address for certificate (127.0.0.1):`

  Wprowadź domenę lub adres IP, na którym działa Twój smp-server - zostanie on zamieszczony w certyfikatach serwera, a także wyświetlony jako część adresu serwera.

### Za pomocą opcji wiersza poleceń

Wykonaj poniższe polecenie:

```sh
sudo su smp -c "smp-server init -h"

...
Available options:
  -l,--store-log           Enable store log for persistence
  -s,--daily-stats         Enable logging daily server statistics
  -a,--sign-algorithm ALG  Signature algorithm used for TLS certificates:
                           ED25519, ED448 (default: ED448)
  --ip IP                  Server IP address, used as Common Name for TLS online
                           certificate if FQDN is not supplied
                           (default: "127.0.0.1")
  -n,--fqdn FQDN           Server FQDN used as Common Name for TLS online
                           certificate
  --no-password            Allow creating new queues without password
  --password PASSWORD      Set password to create new messaging queues
  -y,--yes                 Non-interactive initialization using command-line
                           options
  -h,--help                Show this help text
```

Powinieneś określić, które flagi są potrzebne dla Twojego zastosowania, a następnie wykonać `smp-server init` z flagą `-y` dla nieinteraktywnej inicjalizacji:

```sh
sudo su smp -c "smp-server init -y -<your flag> <your option>"
```

Przykładowo, uruchom:

```sh
sudo su smp -c "smp-server init -y -l --ip 192.168.1.5 --password test"
```

aby zainicjować konfigurację `smp-server` z:

- przywracaniem połączeń i wiadomości po ponownym uruchomieniu serwera (flaga `-l`),
- adresem IP `192.168.1.5`,
- zabezpieczeniem `smp-server` hasłem `test`.

---

Po tym instalacja jest ukończona i powinieneś zobaczyć coś takiego:

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

Powyższy adres serwera powinien być użyty w konfiguracji klienta, a jeśli dodałeś hasło serwera, powinno ono być udostępnione innym osobom tylko wtedy, gdy chcesz zezwolić im na korzystanie z Twojego serwera do odbierania wiadomości (wszystkie Twoje kontakty będą mogły wysyłać wiadomości, ponieważ nie wymaga to hasła). Jeśli podałeś adres IP lub nazwę hosta podczas instalacji, zostanie to wyświetlone jako część adresu serwera, w przeciwnym razie zastąp `<hostnames>` rzeczywistymi adresami serwerów.

## Dokumentacja

Wszystkie niezbędne pliki dla `smp-server` znajdują się w folderze `/etc/opt/simplex/`.

Przechowywane wiadomości, połączenia, statystyki i dziennik serwera znajdują się w folderze `/var/opt/simplex/`.

### Adres serwera SMP

Adres serwera SMP ma następujący format:

```
smp://<odcisk_palca>[:<hasło>]@<publiczna_nazwa_hosta>[,<onionowa_nazwa_hosta>]
```

- `<odcisk_palca>`

  To odcisk palca certyfikatu Twojego `smp-server`. Odcisk palca certyfikatu możesz sprawdzić w `/etc/opt/simplex/fingerprint`.

- **opcjonalnie** `<hasło>`

  To ustawione przez Ciebie hasło Twojego `smp-server`. Możesz sprawdzić to hasło w pliku `/etc/opt/simplex/smp-server.ini`, w sekcji `[AUTH]` w polu `create_password:`.

- `<publiczna_nazwa_hosta>`, **optional** `<onionowa_nazwa_hosta>`

  To skonfigurowane przez Ciebie nazwy hosta Twojego `smp-server`. Nazwy hostów możesz sprawdzić w pliku `/etc/opt/simplex/smp-server.ini`, w sekcji `[TRANSPORT]` w polu `host:`.

### Komendy systemd

Aby uruchomić `smp-server` przy starcie hosta, uruchom:

```sh
sudo systemctl enable smp-server.service

Created symlink /etc/systemd/system/multi-user.target.wants/smp-server.service → /etc/systemd/system/smp-server.service.
```

Aby uruchomić `smp-server`, uruchom:

```sh
sudo systemctl start smp-server.service
```

Aby sprawdzić status `smp-server`, uruchom:

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

Aby zatrzymać `smp-server`, uruchom:

```sh
sudo systemctl stop smp-server.service
```

Aby sprawdzić zawartość dziennika `smp-server`, uruchom:

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

### Monitorowanie

Możesz włączyć statystyki `smp-server` dla dashboardu `Grafana` ustawiając wartość `on` w `/etc/opt/simplex/smp-server.ini`, w sekcji `[STORE_LOG]` w polu `log_stats:`.

Logi będą przechowywane jako plik `csv` w `/var/opt/simplex/smp-server-stats.daily.log`. Pola dla pliku `csv` to:

```sh
fromTime,qCreated,qSecured,qDeleted,msgSent,msgRecv,dayMsgQueues,weekMsgQueues,monthMsgQueues
```

- `fromTime` - timestamp; data i godzina zdarzenia

- `qCreated` - int; utworzone kolejki

- `qSecured` - int; ustanowione kolejki

- `qDeleted` - int; usunięte queues

- `msgSent` - int; wysłane wiadomości

- `msgRecv` - int; odebrane wiadomości

- `dayMsgQueues` - int; aktywnych kolejek podczas dnia

- `weekMsgQueues` - int; aktywnych kolejek w tygodniu

- `monthMsgQueues` - int; aktywnych kolejek w miesiącu

Aby zaimportować `csv` do `Grafana` należy:

1. Zainstalować wtyczkę Grafana: [Grafana - CSV datasource](https://grafana.com/grafana/plugins/marcusolsson-csv-datasource/)

2. Zezwolić na tryb lokalny, dołączając następujące elementy:

  ```sh
  [plugin.marcusolsson-csv-datasource]
  allow_local_mode = true
  ```

  ... do `/etc/grafana/grafana.ini`

3. Dodaj źródło danych CSV:

  - W menu bocznym kliknij zakładkę Configuration (ikona koła zębatego)
  - Kliknij Add data source (Dodaj źródło danych) w prawym górnym rogu zakładki Data Sources (Źródła danych).
  - Wpisz "CSV" w polu wyszukiwania, aby znaleźć źródło danych CSV.
  - Kliknij wynik wyszukiwania z napisem "CSV".
  - W polu URL wprowadź plik wskazujący na zawartość CSV.

4. Gotowe! Teraz możesz utworzyć własny pulpit nawigacyjny ze statystykami.

Dalsza dokumentacja znajduje się na stronie: [CSV Data Source for Grafana - Documentation](https://grafana.github.io/grafana-csv-datasource/).

# Aktualizowanie twojego serwera SMP

Aby zaktualizować smp-server do najnowszej wersji, wybierz metodę instalacji i postępuj zgodnie z instrukcjami:

   - Manualnie
     1. Zatrzymaj serwer:
        ```sh
        sudo systemctl stop smp-server
        ```
     2. Zaktualizuj binarkę:
        ```sh
         curl -L https://github.com/simplex-chat/simplexmq/releases/latest/download/smp-server-ubuntu-20_04-x86-64 -o /usr/local/bin/smp-server && chmod +x /usr/local/bin/smp-server
        ```
     3. Uruchom serwer:
        ```sh
        sudo systemctl start smp-server
        ```

   - Używając [oficjalnego skryptu instalacyjnego](https://github.com/simplex-chat/simplexmq#using-installation-script)
     1. Uruchom:
        ```sh
        sudo simplex-servers-update
        ```
     2. Gotowe!

   - Używając [kontenera Dockera](https://github.com/simplex-chat/simplexmq#using-docker)
     1. Zatrzymaj i usuń kontener:
        ```sh
        docker rm $(docker stop $(docker ps -a -q --filter ancestor=simplexchat/smp-server --format="\{\{.ID\}\}"))
        ```
     2. Pobierz najnowszą wersję kontenera:
        ```sh
        docker pull simplexchat/smp-server:latest
        ```
     3. Uruchom nowy kontener:
        ```sh
        docker run -d \
          -p 5223:5223 \
          -v $HOME/simplex/smp/config:/etc/opt/simplex:z \
          -v $HOME/simplex/smp/logs:/var/opt/simplex:z \
          simplexchat/smp-server:latest
        ```

   - [Linode Marketplace](https://www.linode.com/marketplace/apps/simplex-chat/simplex-chat/)
     1. Pobierz najnowsze obrazy:
        ```sh
        docker-compose --project-directory /etc/docker/compose/simplex pull
        ```
     2. Zrestartuj kontenery:
        ```sh
        docker-compose --project-directory /etc/docker/compose/simplex up -d --remove-orphans
        ```
     3. Usuń niepotrzebne obrazy:
        ```sh
        docker image prune
        ```

### Konfigurowanie aplikacji do korzystania z serwera

Aby skonfigurować aplikację do korzystania z serwera wiadomości, skopiuj jego pełny adres, w tym hasło, i dodaj go do aplikacji. Możesz używać swojego serwera razem z predefiniowanymi serwerami lub bez nich - możesz je usunąć lub wyłączyć.

Możliwe jest również udostępnienie adresu swojego serwera znajomym, pozwalając im zeskanować kod QR z ustawień serwera - będzie on zawierał hasło serwera, dzięki czemu będą mogli również otrzymywać wiadomości za pośrednictwem twojego serwera.

_Uwaga_: Do obsługi haseł wymagany jest serwer SMP w wersji 4.0. Jeśli już posiadasz serwer, możesz dodać hasło do niego poprzez wpisanie hasła do pliku INI serwera.

<img src="./server_config_1.png" width="288"> &nbsp;&nbsp; <img src="./server_config_2.png" width="288"> &nbsp;&nbsp; <img src="./server_config_3.png" width="288">
