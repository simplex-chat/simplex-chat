| Zaktualizowano 31.01.2023 | Języki: PL, [EN](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/CLI.md), [FR](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/fr/CLI.md), [CZ](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/cz/CLI.md) |

# Aplikacja terminalowa (konsola) SimpleX Chat dla Linux/MacOS/Windows

## Spis treści

- [Fukcjonalność terminala czatowego](#funkcjonalność-terminala-czatowego)
- [Instalacja](#🚀-instalacja)
  - [Pobierz klienta czatu](#pobierz-klienta-czatu)
    - [Linux i MacOS](#linux-i-macos)
    - [Windows](#windows)
  - [Zbuduj ze źródła](#zbuduj-ze-źródła)
    - [Używając Dockera](#używając-docker)
    - [Używając stosu Haskell](#używając-stosu-haskell)
- [Użytkowanie](#użytkowanie)
  - [Uruchomienie klienta czatu](#uruchomienie-klienta-czatu)
  - [Dostęp do serwerów wiadomości przez Tor](#dostęp-do-serwerów-wiadomości-przez-tor)
  - [Jak używać SimpleX chat](#jak-używać-simplex-czat)
  - [Grupy](#grupy)
  - [Wysyłanie plików](#wysyłanie-plików)
  - [Adresy kontaktowe użytkowników](#adresy-kontaktowe-użytkowników)
  - [Dostęp do historii czatu](#dostęp-do-historii)

## Fukcjonalność terminala czatowego

- Czat 1 do 1 z wieloma osobami w tym samym oknie terminala.
- Wysyłanie wiadomości do grup.
- Wysyłanie plików do kontaktów i grup.
- Adresy kontaktowe użytkowników - nawiązywanie połączeń za pomocą wielokrotnych łączy kontaktowych.
- Wiadomości przechowywane w lokalnej bazie danych SQLite.
- Automatycznie uzupełniana nazwa odbiorcy - wystarczy wpisać wiadomości, aby odpowiedzieć nadawcy po nawiązaniu połączenia.
- Demo serwerów SMP dostępne i wstępnie skonfigurowane w aplikacji - lub można [wdrożyć własny serwer](https://github.com/simplex-chat/simplexmq#using-smp-server-and-smp-agent).
- Brak globalnej tożsamości lub jakichkolwiek nazw widocznych dla serwera (serwerów), zapewniając pełną prywatność kontaktów i rozmów.
- Dwie warstwy szyfrowania E2E (double-ratchet dla połączeń duplex, używając X3DH key agreement z kluczami ephemeral Curve448, oraz NaCl crypto_box dla kolejek SMP, używając kluczy Curve25519) i przekazywanie kluczy odbiorców out-of-band (patrz [Jak używać SimpleX Chat](#jak-używać-simplex-czat)).
- Walidacja integralności wiadomości (poprzez uwzględnienie digestu poprzednich wiadomości).
- Uwierzytelnianie każdego polecenia/wiadomości przez serwery SMP za pomocą automatycznie generowanych kluczy Ed448.
- Szyfrowanie transportu TLS 1.3.
- Dodatkowe szyfrowanie wiadomości od serwera SMP do odbiorcy w celu zmniejszenia korelacji ruchu.

Klucze publiczne biorące udział w wymianie kluczy nie są używane jako tożsamość, są losowo generowane dla każdego kontaktu.

Szczegóły techniczne znajdują się w dokumencie [Użyte prymitywy szyfrujące](https://github.com/simplex-chat/simplexmq/blob/master/protocol/overview-tjr.md#encryption-primitives-used).

<a name="🚀-installation"></a>

## 🚀 Instalacja

### Pobierz klienta czatu

#### Linux i MacOS

Aby **zainstalować** lub **uaktualnić** `simplex-chat`, należy uruchomić skrypt instalacyjny. Aby to zrobić, użyj następującego polecenia cURL lub Wget:

```sh
curl -o- https://raw.githubusercontent.com/simplex-chat/simplex-chat/stable/install.sh | bash
```

```sh
wget -qO- https://raw.githubusercontent.com/simplex-chat/simplex-chat/stable/install.sh | bash
```

Po pobraniu klienta czatu, możesz go uruchomić komendą `simplex-chat` w terminalu.

Alternatywnie, możesz ręcznie pobrać binarkę czatu dla swojego systemu z [najnowsze stabilne wydanie](https://github.com/simplex-chat/simplex-chat/releases) i wykonać ją jak pokazano poniżej.

```sh
chmod +x <binary>
mv <binary> ~/.local/bin/simplex-chat
```

(lub innej preferowanej lokalizacji w `PATH`).

Na MacOS musisz także [zezwolić Gatekeeperowi na uruchomienie](https://support.apple.com/en-us/HT202491).

#### Windows

```sh
move <binary> %APPDATA%/local/bin/simplex-chat.exe
```

### Zbuduj ze źródła

> **Proszę zauważyć:** aby zbudować aplikację użyj kodu źródłowego z [stabilnej gałęzi](https://github.com/simplex-chat/simplex-chat/tree/stable).

#### Używając Docker

W systemie Linux można zbudować plik wykonywalny czatu za pomocą [budowanie dockerem z niestandardowym wyjściem](https://docs.docker.com/engine/reference/commandline/build/#custom-build-outputs):

```shell
git clone git@github.com:simplex-chat/simplex-chat.git
cd simplex-chat
git checkout stable
DOCKER_BUILDKIT=1 docker build --output ~/.local/bin .
```

> **Proszę zauważyć:** Jeśli napotkasz błąd `` version `GLIBC_2.28' not found ``, przebuduj go za pomocą obrazu bazowego `haskell:8.10.7-stretch` (zmień go w swoim lokalnym [Dockerfile](Dockerfile)).

#### W dowolnym systemie operacyjnym

1. Zainstaluj [Haskell GHCup](https://www.haskell.org/ghcup/), GHC 8.10.7 i cabal:

```shell
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

2. Zbuduj projekt:

```shell
git clone git@github.com:simplex-chat/simplex-chat.git
cd simplex-chat
git checkout stable
# w Linuxie
apt-get update && apt-get install -y build-essential libgmp3-dev zlib1g-dev
cp scripts/cabal.project.local.linux cabal.project.local
# lub w MacOS:
# brew install openssl@1.1
# cp scripts/cabal.project.local.mac cabal.project.local
# możesz potrzebować zmienić cabal.project.local aby wskazać rzeczywistą lokalizację openssl
cabal update
cabal install
```

## Użytkowanie

### Uruchomienie klienta czatu

Aby uruchomić klienta czatu, uruchom `simplex-chat` z terminala.

Domyślnie, katalog danych aplikacji jest tworzony w katalogu domowym (`~/.simplex`, lub `%APPDATA%/simplex` w Windows), a dwa pliki bazy danych SQLite `simplex_v1_chat.db` i `simplex_v1_agent.db` są w nim inicjalizowane.

Aby określić inny prefiks ścieżki do plików bazy danych użyj opcji linii poleceń `-d`:
```shell
$ simplex-chat -d alice
```

Uruchomienie powyższego spowoduje na przykład utworzenie plików baz danych `alice_v1_chat.db` i `alice_v1_agent.db` w bieżącym katalogu.

Trzy domyślne serwery SMP znajdują się na Linode - są one [wstępnie skonfigurowane w aplikacji](https://github.com/simplex-chat/simplex-chat/blob/stable/src/Simplex/Chat/Options.hs#L42).

Jeśli wdrożyłeś swój własny serwer SMP możesz skonfigurować klienta poprzez opcję `-s`:

```shell
$ simplex-chat -s smp://LcJUMfVhwD8yxjAiSaDzzGF3-kLG4Uh0Fl_ZIjrRwjI=@smp.example.com
```

Zakodowany w Base64url ciąg znaków poprzedzający adres serwera to odcisk palca certyfikatu offline serwera, który jest weryfikowany przez klienta podczas TLS handshake.

Nadal możesz rozmawiać z ludźmi używając domyślnego lub dowolnego innego serwera - wpływa to tylko na lokalizację kolejki wiadomości podczas inicjowania połączenia (a kolejka odpowiedzi może być na innym serwerze, jak ustawiono przez klienta drugiej strony).

Uruchom `simplex-chat -h`, aby zobaczyć wszystkie dostępne opcje.

### Dostęp do serwerów wiadomości przez Tor

Zainstaluj Tor i uruchom go jako SOCKS5 proxy na porcie 9050, np. na Macu możesz:

```
brew install tor
brew services start tor
```

Użyj opcji `-x`, aby uzyskać dostęp do serwerów przez Tor:

```
simplex-chat -x
```

Możesz także użyć opcji `--socks-proxy=ipv4:port` lub `--socks-proxy=:port` aby skonfigurować host i port twojego SOCKS5 proxy, np. jeśli uruchamiasz go na innym hoście lub porcie.

### Jak używać SimpleX Chat

Po uruchomieniu czatu zostaniesz poproszony o określenie swojej "nazwy wyświetlanej" i opcjonalnej "pełnej nazwy" w celu utworzenia lokalnego profilu czatu. Twoja nazwa wyświetlana to alias, przez który Twoje kontakty mogą się do Ciebie odnosić - nie jest ona unikalna i nie służy jako globalna tożsamość. Jeśli niektóre z Twoich kontaktów wybrały tę samą nazwę wyświetlania, klient czatu dodaje numeryczny przyrostek do ich lokalnej nazwy wyświetlania.

Poniższy diagram przedstawia sposób łączenia się z kontaktem i wysyłania do niego wiadomości:

<div align="center">
  <img align="center" src="https://github.com/simplex-chat/simplex-chat/blob/stable/images/how-to-use-simplex.svg">
</div>

Po skonfigurowaniu lokalnego profilu, wpisz `/c` (dla `/connect`), aby stworzyć nowe połączenie i wygenerować zaproszenie. Wyślij to zaproszenie do swojego kontaktu poprzez dowolny inny kanał.

Możesz stworzyć wiele zaproszeń wpisując `/connect` wiele razy i wysyłając te zaproszenia do odpowiednich kontaktów, z którymi chcesz się połączyć.

Zaproszenie może być użyte tylko raz i nawet jeśli zostanie przechwycone, atakujący nie będzie mógł go użyć do wysłania wiadomości przez tę kolejkę, gdy Twój kontakt potwierdzi, że połączenie zostało nawiązane. Zobacz protokół agenta w celu wyjaśnienia [formatu zaproszenia](https://github.com/simplex-chat/simplexmq/blob/master/protocol/agent-protocol.md#connection-request).

Kontakt, który otrzymał zaproszenie powinien wpisać `/c <invitation>` aby zaakceptować połączenie. To ustanawia połączenie, a obie strony są powiadomione.

Następnie używają komend `@<name> <message>` do wysyłania wiadomości. Możesz także po prostu zacząć pisać wiadomość, aby wysłać ją do kontaktu, który był ostatni.

Użyj `/help` w czacie, aby zobaczyć listę dostępnych komend.

### Grupy

Aby stworzyć grupę użyj `/g <group>`, następnie dodaj do niej kontakty za pomocą `/a <group> <name>`. Następnie możesz wysyłać wiadomości do grupy wpisując `#<group> <message>`. Użyj `/help groups` dla innych komend.

![simplex-chat](https://github.com/simplex-chat/simplex-chat/blob/stable/images/groups.gif)

> **Proszę zauważyć**: grupy nie są przechowywane na żadnym serwerze, są utrzymywane jako lista członków w bazie danych aplikacji, do których będą wysyłane wiadomości.

### Wysyłanie plików

Możesz wysłać plik do kontaktu za pomocą `/f @<contact> <file_path>` - odbiorca będzie musiał go zaakceptować przed wysłaniem. Użyj `/help files` dla innych poleceń.

![simplex-chat](https://github.com/simplex-chat/simplex-chat/blob/stable/images/files.gif)

Pliki można wysłać do grupy za pomocą `/f #<group> <file_path>`.

### Adresy kontaktowe użytkowników

Jako alternatywę dla jednorazowych linków zapraszających, możesz stworzyć długoterminowy adres za pomocą `/ad` (dla `/address`). Utworzony adres może być następnie udostępniony poprzez dowolny kanał i użyty przez innych użytkowników jako link do prośby o kontakt z `/c <user_contact_address>`.

Możesz zaakceptować lub odrzucić przychodzące prośby za pomocą komend `/ac <name>` i `/rc <name>`.

Adres użytkownika jest "długoterminowy" w tym sensie, że jest to łącze wielokrotnego użytku - może być używany do momentu, gdy zostanie usunięty przez użytkownika, w którym to przypadku wszystkie ustanowione połączenia nadal pozostaną aktywne (w przeciwieństwie do tego, jak to działa w przypadku emaila, gdy zmiana adresu powoduje, że ludzie nie są w stanie wysłać do ciebie wiadomości).

Użyj `/help address` dla innych komend.

![simplex-chat](https://github.com/simplex-chat/simplex-chat/blob/stable/images/user-addresses.gif)

### Dostęp do historii czatu

SimpleX chat przechowuje wszystkie kontakty i rozmowy w lokalnej bazie danych SQLite, dzięki czemu są one z założenia prywatne i przenośne, stanowiące własność i kontrolowane przez użytkownika.

Możesz przeglądać i przeszukiwać historię czatu, wykonując zapytania do bazy danych. Uruchom poniższy skrypt, aby utworzyć widoki wiadomości w swojej bazie danych.

```sh
curl -o- https://raw.githubusercontent.com/simplex-chat/simplex-chat/stable/scripts/message_views.sql | sqlite3 ~/.simplex/simplex_v1_chat.db
```

Otwórz powłokę wiersza poleceń SQLite:

```sh
sqlite3 ~/.simplex/simplex_v1_chat.db
```

Zobacz [Zapytania wiadomości](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/docs/SQL.md) dla przykładów.

> **Proszę zauważyć:** Ograniczenia klucza obcego SQLite są domyślnie wyłączone i muszą być **[włączone oddzielnie dla każdego połączenia z bazą danych](https://sqlite.org/foreignkeys.html#fk_enable)**. Można to osiągnąć przez wykonanie polecenia `PRAGMA foreign_keys = ON;` na otwartym połączeniu z bazą danych. Wykonując zapytania zmieniające dane bez uprzedniego włączenia kluczy obcych, możesz ryzykować, że Twoja baza danych będzie w niespójnym stanie.

**Wygodne zapytania**

Pobierz wszystkie wiadomości z dnia dzisiejszego (`chat_dt` jest w UTC):

```sql
select * from all_messages_plain where date(chat_dt) > date('now', '-1 day') order by chat_dt;
```

Otrzymuj rano wiadomości z nocy:

```sql
select * from all_messages_plain where chat_dt > datetime('now', '-15 hours') order by chat_dt;
```
