---
title: Aplikacja konsolowa
revision: 31.01.2023
---

| Updated 31.01.2023 | Języki: PL, [EN](/docs/CLI.md), [FR](/docs/lang/fr/CLI.md), [CZ](/docs/lang/cs/CLI.md) |

# Terminalowa (konsolowa) aplikacja SimpleX Chat dla systemów Linux/MacOS/Windows

## Spis treści

- [Funkcje czatu w terminalu](#funkcje-czatu-w-terminalu)
- [Instalacja](#🚀-instalacja)
  - [Pobieranie klienta czatu](#pobieranie-klienta-czatu)
    - [Linux i MacOS](#linux-i-macos)
    - [Windows](#windows)
  - [Budowanie z kodu źródłowego](#budowanie-z-kodu-źródłowego)
    - [Używając dockera](#using-docker)
    - [Używając Haskella na dowolnym systemie operacyjnym](#używając-haskella-na-dowolnym-systemie-operacyjnym)
- [Używanie](#używanie)
  - [Używanie klienta czatu](#używanie-klienta-czatu)
  - [Dostęp do serwerów wiadomości przez Tor](#dostęp-do-serwerów-wiadomości-przez-tor)
  - [Jak używać czatu SimpleX](#jak-używać-czatu-simplex)
  - [Grupy](#grupy)
  - [Wysyłanie plików](#wysyłanie-plików)
  - [Adresy kontaktowe użytkowników](#adresy-kontaktowe-użytkowników)

## Funkcje czatu w terminalu

- Konwersacje 1 na 1 z wieloma osobami w tym samym oknie terminala.
- Wiadomości grupowe.
- Wysyłanie plików do kontaktów i grup.
- Adresy kontaktowe użytkowników - nawiązywanie połączeń za pomocą linków kontaktowych wielokrotnego użytku.
- Wiadomości przechowywane w lokalnej bazie danych SQLite.
- Automatycznie wypełniana nazwa odbiorcy - po nawiązaniu połączenia wystarczy po prostu napisać wiadomość, aby odpowiedzieć nadawcy.
- Dostępne wstępnie skonfigurowane przykładowe serwery SMP - można też użyć [własnego serwera](https://github.com/simplex-chat/simplexmq#using-smp-server-and-smp-agent).
- Żadna globalna tożsamość ani nazwy użytkowników nie są widoczne dla serwera (serwerów), co zapewnia pełną prywatność kontaktów i rozmów.
- Dwie warstwy szyfrowania E2E (double-ratchet dla połączeń dwukierunkowych, przy użyciu negocjacji klucza X3DH z efemerycznymi kluczami Curve448 i NaCl crypto_box dla kolejek SMP, przy użyciu kluczy Curve25519) oraz przekazywanie kluczy odbiorców za pomocą komunikacji out-of-band (zobacz [Jak używać czatu SimpleX](#how-to-use-simplex-chat)).
- Weryfikacja integralności wiadomości (poprzez uwzględnienie hashu poprzedniej wiadomości).
- Uwierzytelnianie każdego polecenia/wiadomości przez serwery SMP za pomocą automatycznie generowanych kluczy Ed448.
- Szyfrowanie transmisji przy użyciu TLS 1.3.
- Dodatkowe szyfrowanie wiadomości z serwera SMP do odbiorcy aby utrudnić możliwość korelacji ruchu.

Klucze publiczne biorące udział podczas wymiany kluczy nie są używane jako tożsamość, są one generowane losowo dla każdego kontaktu.

Aby uzyskać szczegółowe informacje techniczne zobacz [używane metody szyfrowania](https://github.com/simplex-chat/simplexmq/blob/master/protocol/overview-tjr.md#encryption-primitives-used).

<a name="🚀-installation"></a>

## 🚀 Instalacja

### Pobieranie klienta czatu

#### Linux i MacOS

By **zainstalować** lub **zaktualizować** `simplex-chat`, należy uruchomić skrypt instalacyjny. Aby to zrobić, użyj następującego polecenia cURL lub Wget:

```sh
curl -o- https://raw.githubusercontent.com/simplex-chat/simplex-chat/stable/install.sh | bash
```

```sh
wget -qO- https://raw.githubusercontent.com/simplex-chat/simplex-chat/stable/install.sh | bash
```

Po pobraniu klienta czatu można go uruchomić za pomocą polecenia `simplex-chat`.

Możesz również ręcznie pobrać plik binarny czatu dla swojego systemu z [najnowszej stabilnej wersji](https://github.com/simplex-chat/simplex-chat/releases) i uczynić go uruchamialnym w sposób pokazany poniżej.

```sh
chmod +x <binary>
mv <binary> ~/.local/bin/simplex-chat
```

(lub użyj innej preferowanej lokalizacji w `PATH`).

Na MacOS musisz również [zezwolić Gatekeeperowi, by go uruchomić](https://support.apple.com/en-us/HT202491).

#### Windows

```sh
move <binary> %APPDATA%/local/bin/simplex-chat.exe
```

### Budowanie z kodu źródłowego

> **Uwaga:** aby zbudować aplikację użyj [wersji stabilnej](https://github.com/simplex-chat/simplex-chat/tree/stable).

#### Używając Dockera

Na Linuxie, aby zbudować plik wykonywalny możesz użyć [docker build z customowym outputem](https://docs.docker.com/engine/reference/commandline/build/#custom-build-outputs):

```shell
git clone git@github.com:simplex-chat/simplex-chat.git
cd simplex-chat
git checkout stable
DOCKER_BUILDKIT=1 docker build --output ~/.local/bin .
```

> **Uwaga:** Jeśli napotkasz błąd `` version `GLIBC_2.28' not found ``, przebuduj go z obrazem bazowym `haskell:8.10.7-stretch` (zmień go w Twoim lokalnym pliku [Dockerfile](Dockerfile)).

#### Używając Haskella na dowolnym systemie operacyjnym

1. Zainstaluj [Haskell GHCup](https://www.haskell.org/ghcup/), GHC 9.6.3 i cabal 3.10.1.0:

```shell
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

Możesz użyć polecenia `ghcup tui`, aby sprawdzić lub dodać wersje GHC i cabal.

2. Sklonuj kod źródłowy:

```shell
git clone git@github.com:simplex-chat/simplex-chat.git
cd simplex-chat
git checkout stable
# lub aby zbudować konkretną wersję:
# git checkout v5.3.0-beta.8
```

`master` to branch deweloperski, może on zawierać niestabilny kod.

3. Przygotowywanie systemu:

Na Linuxie:

```shell
apt-get update && apt-get install -y build-essential libgmp3-dev zlib1g-dev
cp scripts/cabal.project.local.linux cabal.project.local
```

Na Macu:

```
brew install openssl@1.1
cp scripts/cabal.project.local.mac cabal.project.local
```

Może być konieczna zmiana cabal.project.local, aby wskazać poprawną lokalizację openssl

4. Budowanie aplikacji:

```shell
cabal update
cabal install simplex-chat
```

## Używanie

### Używanie klienta czatu

Aby uruchomić klienta, uruchom w terminalu polecenie `simplex-chat`.

Domyślnie katalog z danymi aplikacji jest tworzony w katalogu domowym (`~/.simplex`, lub `%APPDATA%/simplex` na Windowsie), a dwa pliki danych SQLite `simplex_v1_chat.db` i `simplex_v1_agent.db` są w nim zainicjowane.

Aby wskazać inny prefiks ścieżki dla plików bazy danych, należy użyć polecenia `-d`:

```shell
$ simplex-chat -d alice
```

Uruchomienie powyższego przykładu spowoduje utworzenie plików baz danych `alice_v1_chat.db` i `alice_v1_agent.db` w bieżącym katalogu.

Trzy domyślne serwery SMP są hostowane na Linode - są one [wstępnie skonfigurowane w aplikacji](https://github.com/simplex-chat/simplex-chat/blob/stable/src/Simplex/Chat/Options.hs#L42).

Jeśli posiadasz własny serwer(y) SMP, możesz skonfigurować klienta poprzez opcję `-s`:

```shell
$ simplex-chat -s smp://LcJUMfVhwD8yxjAiSaDzzGF3-kLG4Uh0Fl_ZIjrRwjI=@smp.example.com
```

Ciąg zakodowany w Base64url poprzedzający adres serwera to odcisk palca certyfikatu offline serwera, który jest weryfikowany przez klienta podczas handshake'a TLS.

Po konfiguracji innego serwera w swoim kliencie nadal możesz rozmawiać z ludźmi używającymi domyślnego lub dowolnego innego serwera - ustawienie to wpływa tylko na lokalizację kolejki wiadomości podczas nawiązywania połączenia (a kolejka odpowiedzi może znajdować się na zupełnie innym serwerze, zgodnie z ustawieniami klienta rozmówcy).

Polecenie `simplex-chat -h` pokazuje wszystkie dostępne opcje.

### Dostęp do serwerów wiadomości przez Tor

Zainstaluj Tor i uruchom go jako proxy SOCKS5 na porcie 9050, przykład dla MacOS:

```
brew install tor
brew services start tor
```

Użyj opcji `-x`, aby uzyskać dostęp do serwerów przez Tor:

```
simplex-chat -x
```

Możesz także użyć opcji `--socks-proxy=ipv4:port` lub `--socks-proxy=:port`, aby skonfigurować adres i port serwera proxy SOCKS5, przykładowo jeśli uruchamiasz go na innym hoście lub porcie.

### Jak używać czatu SimpleX

Po uruchomieniu czatu zostaniesz poproszony o podanie swojej "nazwy wyświetlanej" oraz opcjonalnej "pełnej nazwy" w celu utworzenia lokalnego profilu czatu. Nazwa wyświetlana jest aliasem, za pomocą którego kontakty mogą się do ciebie odnosić - nie jest ona unikalna i nie służy jako globalna tożsamość. Jeśli kilka kontaktów wybrało tę samą nazwę wyświetlaną, klient czatu dodaje numeryczną końcówkę (sufiks) do ich lokalnej nazwy wyświetlanej.

Poniższy schemat przedstawia sposób łączenia się z kontaktem i wysyłania do niego wiadomości:

<div align="center">
  <img align="center" src="../images/how-to-use-simplex.svg">
</div>

Gdy już skonfigurujesz swój profil lokalny, wpisz `/c` (oznaczające `/connect`), aby utworzyć nowe połączenie i wygenerować zaproszenie. Wyślij to zaproszenie do swojego kontaktu za pośrednictwem dowolnego innego kanału komunikacji.

Możesz utworzyć wiele zaproszeń, kilkukrotnie wpisując `/connect` i wysłać te zaproszenia do kontaktów, z którymi chcesz się połączyć.

Zaproszenie może być użyte tylko jeden raz i nawet jeśli zostanie ono przechwycone, atakujący nie będzie mógł go użyć do wysłania do Ciebie wiadomości za pośrednictwem tej kolejki, gdy Twój kontakt potwierdzi, że połączenie zostało nawiązane. Zobacz omówienie protokołu agenta dla [formatu zaproszeń](https://github.com/simplex-chat/simplexmq/blob/master/protocol/agent-protocol.md#connection-request).

Kontakt, który otrzymał zaproszenie powinien wpisać `/c <zaproszenie>`, aby zaakceptować połączenie. Spowoduje to nawiązanie połączenia, a obie strony zostaną o tym powiadomione.

Następnie można użyć komendy `@<nazwa_kontaktu> <wiadomość>` do wysłania wiadomości. Możesz także po prostu zacząć pisać wiadomość, aby wysłać ją do kontaktu, który był ostatni.

Użyj `/help` na czacie, by uzyskać listę pozostałych dostępnych komend.

### Grupy

Aby utworzyć grupę, użyj `/g <nazwa_grupy>`, a następnie dodaj do niej kontakty za pomocą `/a <nazwa_grupy> <nazwa_kontaktu>`. Możesz wysyłać wiadomości do grupy wpisując `#<nazwa_grupy> <wiadomość>`. Użyj `/help groups`, by uzyskać listę pozostałych dostępnych komend.

![simplex-chat](../images/groups.gif)

> **Uwaga**: informacje o grupach nie są przechowywane na żadnym serwerze, są one zapisywane jako lista członków w bazie danych aplikacji klientów, do których będą wysyłane wiadomości.

### Wysyłanie plików

Możesz wysłać plik do kontaktu za pomocą `/f @<nazwa_kontaktu> <ścieżka_do_pliku>` - odbiorca będzie musiał go zaakceptować przed rozpoczęciem wysyłania. Użyj `/help files`, by uzyskać listę pozostałych dostępnych komend.

![simplex-chat](../images/files.gif)

Możesz wysyłać pliki do grupy za pomocą `/f #<nazwa_grupy> <ścieżka_do_pliku>`.

### Adresy kontaktowe użytkowników

Alternatywą dla jednorazowych linków zapraszających są adresy długoterminowe. Możesz je utworzyć za pomocą `/ad` (oznaczające `/address`). Utworzony adres może być następnie udostępniony za pośrednictwem dowolnego innego kanału komunikacji i użyty przez innych użytkowników jako link do prośby o kontakt używając `/c <adres_kontaktowy_użytkownika>`.

Prośby o kontakt możesz przyjąć za pomocą komendy `/ac <nazwa>` oraz odrzucić za pomocą `/rc <nazwa>`.

"Długoterminowy" adres użytkownika jest długoterminowy w tym sensie, że jest to link wielokrotnego użytku - może być używany do momentu usunięcia go przez użytkownika. Po usunięciu wszystkie nawiązane połączenia pozostaną aktywne (w przeciwieństwie do tego, jak działa to w przypadku poczty e-mail, gdy zmiana adresu powoduje, że ludzie nie mogą już wysyłać do siebie wiadomości).

Użyj `/help address`, by uzyskać listę pozostałych dostępnych komend.

![simplex-chat](../images/user-addresses.gif)
