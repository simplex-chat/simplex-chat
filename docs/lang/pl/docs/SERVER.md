| Zaktualizowano 31.01.2023 | Języki: PL, [EN](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/SERVER.md), [FR](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/fr/SERVER.md), [CZ](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/cz/SERVER.md) |

# Hosting własnego serwera SMP

## Przegląd

Serwer SMP jest serwerem przekaźnikowym używanym do przekazywania wiadomości w sieci SimpleX. Aplikacje SimpleX Chat mają wstępnie ustawione serwery (dla aplikacji mobilnych są to smp8, smp9 i smp10.simplex.im), ale możesz łatwo zmienić konfigurację aplikacji, aby używać innych serwerów.

Klienci SimpleX określają jedynie, który serwer jest używany do odbierania wiadomości, oddzielnie dla każdego kontaktu (lub połączenia grupowego z członkiem grupy), a serwery te są tylko tymczasowe, ponieważ adres dostawy może się zmienić.

_Zwróć uwagę_: gdy zmieniasz serwery w konfiguracji aplikacji, wpływa to tylko na to, który serwer będzie używany dla nowych kontaktów, istniejące kontakty nie zostaną automatycznie przeniesione na nowe serwery, ale możesz je przenieść ręcznie za pomocą przycisku ["Przełącz adres odbioru"](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/blog/20221108-simplex-chat-v4.2-security-audit-new-website.md#przełącz-adres-odbioru-beta) na stronach z informacjami o kontaktach/członkach - wkrótce zostanie to zautomatyzowane.

## Instalacja

0. Najpierw należy zainstalować `smp-server`:

   - Ręczne wdrożenie:

     - Kompilacja ze źródła](https://github.com/simplex-chat/simplexmq#using-your-distribution)
     - [Używanie prekompilowanych binariów](https://github.com/simplex-chat/simplexmq#install-binaries)

   - Alternatywnie, można wdrożyć `smp-server` używając:
     - [kontener Docker](https://github.com/simplex-chat/simplexmq#using-docker-1)
     - [Linode StackScript](https://github.com/simplex-chat/simplexmq#deploy-smp-server-on-linode)

Instalacja ręczna wymaga wykonania kilku czynności wstępnych:

1. Utwórz użytkownika i grupę dla `smp-server`:

   ```sh
   sudo useradd -m smp
   ```

2. Utwórz niezbędne katalogi i nadaj uprawnienia:

   ```sh
   sudo mkdir -p /var/opt/simplex /etc/opt/simplex
   sudo chown smp:smp /var/opt/simplex /etc/opt/simplex
   ```

3. Zezwól na port `smp-server` w firewallu:

   ```sh
   # Dla Ubuntu
   ufw allow 5223
   ```

4. **Opcjonalnie** - Jeśli używasz dystrybucji z `systemd`, utwórz plik `/etc/systemd/smp-server.service` o następującej zawartości:

   ```sh
   [Unit]
   Description=SMP server
   [Service]
   User=smp
   Group=smp
   Type=simple
   ExecStart=smp-server start
   ExecStopPost=/usr/bin/env sh -c '[ -e "/var/opt/simplex/smp-server-store.log" ] && cp "/var/opt/simplex/smp-server-store.log" "/var/opt/simplex/smp-server-store.log.bak"'
   KillSignal=SIGINT
   TimeoutStopSec=infinity
   Restart=always
   RestartSec=10
   LimitNOFILE=65535
   [Install]
   WantedBy=multi-user.target
   ```

   I wykonaj `sudo systemctl daemon-reload`.

   ## Konfiguracja

   Aby zobaczyć jakie opcje są dostępne, wykonaj `smp-server` bez flag:

```sh
sudo su smp -c smp-server

...
Available commands:
  init                     Inicjalizacja serwera - tworzy katalogi i pliki
                           konfiguracyjne w /etc/opt/simplex oraz /var/opt/simplex
  start                    Uruchom serwer (konfiguracja):
                           /etc/opt/simplex/smp-server.ini)
  delete                   Usuwanie konfiguracji i plików dziennika
```

Dalszą pomoc można uzyskać wykonując `sudo su smp -c "smp-server <command> -h"`

Następnie musimy skonfigurować `smp-server`:

### Interaktywnie

Wykonaj następujące polecenie:

```sh
sudo su smp -c "smp-server init"
```

Istnieje kilka opcji do rozważenia:

- `Enable store log to restore queues and messages on server restart (Yn):`

  Wpisz `y`, aby włączyć zapisywanie i przywracanie połączeń i wiadomości po ponownym uruchomieniu serwera.

  _Zwróć uwagę_: ważne jest, aby użyć SIGINT do ponownego uruchomienia serwera, ponieważ w przeciwnym razie niedostarczone wiadomości nie zostaną przywrócone. Połączenia zostaną przywrócone niezależnie od tego, jak serwer zostanie zrestartowany, ponieważ w przeciwieństwie do wiadomości są one dodawane do logu append-only przy każdej zmianie.

- `Enable logging daily statistics (yN):`

  Wpisz `y` aby włączyć rejestrowanie statystyk w formacie CSV, np. mogą być one użyte do pokazania zbiorczych wykresów użycia w `Grafana`.

Statystyki te zawierają dzienne zliczenia utworzonych, zabezpieczonych i usuniętych kolejek, wysłanych i odebranych wiadomości, a także dzienne, tygodniowe i miesięczne zliczenia aktywnych kolejek (czyli kolejek, które były używane do jakichkolwiek wiadomości). Wierzymy, że te informacje nie zawierają niczego, co pozwoliłoby skorelować różne kolejki jako należące do tych samych użytkowników, ale prosimy o poufne poinformowanie nas, jeśli uważasz, że można to w jakiś sposób wykorzystać.

- `Require a password to create new messaging queues?`

  Wprowadź `r` lub dowolne hasło aby zabezpieczyć hasłem `smp-server`, lub `n` aby wyłączyć ochronę hasłem.

- `Enter server FQDN or IP address for certificate (127.0.0.1):`

  Wpisz domenę lub adres ip, na którym działa Twój smp-server - będzie on zawarty w certyfikatach serwera, a także widoczny jako część adresu serwera.

  ### Przez opcje linii poleceń

  Wykonaj następujące polecenie:

```sh
sudo su smp -c "smp-server init -h"

...
Available options:
  -l,--store-log           Włączenie trwałego dziennika przechowywania
  -s,--daily-stats         Włączenie logowania codziennych statystyk serwera
  -a,--sign-algorithm ALG  Algorytm podpisu używany dla certyfikatów TLS:
                           ED25519, ED448 (domyślnie: ED448)
  --ip IP                  Adres IP serwera, używany jako Nazwa Wspólna dla
                           certyfikatu TLS online, jeśli nie podano FQDN
                           (domyślnie: "127.0.0.1")
  -n,--fqdn FQDN           FQDN serwera używany jako Nazwa Wspólna dla certyfikatu
                           TLS online
  --no-password            Pozwól na tworzenie nowych kolejek bez hasła
  --password PASSWORD      Ustawienie hasła do tworzenia nowych kolejek wiadomości
  -y,--yes                 Nieinteraktywna inicjalizacja przy użyciu opcji wiersza
                           poleceń
  -h,--help                Pokaż ten tekst pomocy
```

Powinieneś określić, które flagi są potrzebne dla danego scenariusza, a następnie wykonać `smp-server init` z flagą `-y` dla nieinteraktywnej inicjalizacji:

```sh
sudo su smp -c "smp-server init -y -<your flag> <your option>"
```

Na przykład, uruchom:

```sh
sudo su smp -c "smp-server init -y -l --ip 192.168.1.5 --password test"
```

aby zainicjować konfigurację `smp-server`:

- przywracanie połączeń i wiadomości przy ponownym uruchomieniu serwera (flaga `-l`),
- adres IP `192.168.1.5`,
- zabezpieczyć `smp-server` hasłem `test`.

---

Po tym, twoja instalacja jest zakończona i powinieneś zobaczyć w oknie teminalu coś takiego:

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

Powyższy adres serwera powinien być użyty w konfiguracji klienta, a jeśli dodałeś hasło do serwera, powinno ono być udostępnione innym osobom tylko wtedy, gdy chcesz pozwolić im na używanie twojego serwera do odbierania wiadomości (wszystkie twoje kontakty będą mogły wysyłać wiadomości, ponieważ nie wymaga to hasła). Jeśli podałeś adres IP lub nazwę hosta podczas inicjalizacji, zostaną one wypisane jako część adresu serwera, w przeciwnym razie zastąp `<hostnames>` rzeczywistymi adresami serwerów.

## Dokumentacja

Wszystkie niezbędne pliki dla `smp-server` znajdują się w folderze `/etc/opt/simplex/`.

Przechowywane wiadomości, połączenia, statystyki i log serwera znajdują się w folderze `/var/opt/simplex/`.

### Adres serwera SMP

Adres serwera SMP ma następujący format:

```
smp://<fingerprint>[:<password>]@<public_hostname>[,<onion_hostname>]
```

- `<fingerprint>`

  Twój odcisk palca certyfikatu `smp-server`. Możesz sprawdzić swój odcisk palca certyfikatu w `/etc/opt/simplex/fingerprint`.

- **opcjonalnie** `<password>`

  Twoje skonfigurowane hasło `smp-server`. Możesz sprawdzić swoje skonfigurowane hasło w `/etc/opt/simplex/smp-server.ini`, w sekcji `[AUTH]` w polu `create_password:`.

- `<public_hostname>`, **opcjonalnie** `<onion_hostname>`

  Twoja skonfigurowana nazwa (nazwy) hosta `smp-server`. Możesz sprawdzić swoje skonfigurowane hosty w `/etc/opt/simplex/smp-server.ini`, w sekcji `[TRANSPORT]` w polu `host:`.

### Polecenia Systemd

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

Aby sprawdzić koniec logu `smp-server`, należy uruchomić:

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

### Monitoring

Możesz włączyć statystyki `smp-server` dla dashboardu `Grafana` poprzez ustawienie wartości `on` w `/etc/opt/simplex/smp-server.ini`, w sekcji `[STORE_LOG]` w polu `log_stats:`.

Logi będą przechowywane jako plik `csv` w `/var/opt/simplex/smp-server-stats.daily.log`. Pola dla pliku `csv` to:

```sh
fromTime,qCreated,qSecured,qDeleted,msgSent,msgRecv,dayMsgQueues,weekMsgQueues,monthMsgQueues
```

- `fromTime` - timestamp; data i czas zdarzenia

- `qCreated` - int; kolejki utworzone

- `qSecured` - int; kolejki ustanowione

- `qDeleted` - int; kolejki usunięte

- `msgSent` - int; wiadomości wysłane

- `msgRecv` - int; wiadomości odebrane

- `dayMsgQueues` - int; kolejki aktywne w danym dniu

- `weekMsgQueues` - int; kolejki aktywne w tygodniu

- `monthMsgQueues` - int; kolejki aktywne w miesiącu

Aby zaimportować `csv` do `Grafana` należy:

1. Zainstalować plugin Grafany: [Grafana - CSV datasource](https://grafana.com/grafana/plugins/marcusolsson-csv-datasource/)

2. Zezwolić na tryb lokalny poprzez dołączenie:

   ```sh
   [plugin.marcusolsson-csv-datasource]
   allow_local_mode = true
   ```

   ... do `/etc/grafana/grafana.ini`

3. Dodaj źródło danych CSV:

   - W menu bocznym kliknij zakładkę Configuration (ikona trybiku)
   - Klikinij Add data source w prawym górnym rogu zakładki Data Sources
   - Wpisz "CSV" w polu wyszukiwania, aby znaleźć źródło danych CSV
   - Kliknij wynik wyszukiwania, który zawiera "CSV"
   - W polu URL wpisz plik, który wskazuje na zawartość CSV

4. Skończyłeś! Powinieneś być w stanie stworzyć własny dashboard ze statystykami.

Dalsza dokumentacja znajduje się na stronie: [Źródło danych CSV dla Grafany - Dokumentacja](https://grafana.github.io/grafana-csv-datasource/)

### Konfiguracja aplikacji do korzystania z serwera

Aby skonfigurować aplikację do korzystania z Twojego serwera wiadomości skopiuj jego pełny adres, w tym hasło i dodaj go do aplikacji. Masz możliwość używania swojego serwera razem z ustawionymi serwerami lub bez nich - możesz je usunąć lub wyłączyć.

Możliwe jest również udostępnienie adresu swojego serwera znajomym, pozwalając im zeskanować kod QR z ustawień serwera - będzie on zawierał hasło do serwera, więc będą mogli otrzymywać wiadomości za pośrednictwem Twojego serwera.

_Proszę zauważyć_: musisz mieć serwer SMP w wersji 4.0, aby mieć wsparcie dla hasła. Jeśli masz już wdrożony serwer, możesz dodać hasło, dodając je do pliku INI serwera.

<img src="https://github.com/simplex-chat/simplex-chat/blob/stable/docs/server_config_1.png" width="288"> &nbsp;&nbsp; <img src="https://github.com/simplex-chat/simplex-chat/blob/stable/docs/server_config_2.png" width="288"> &nbsp;&nbsp; <img src="https://github.com/simplex-chat/simplex-chat/blob/stable/docs/server_config_3.png" width="288">
