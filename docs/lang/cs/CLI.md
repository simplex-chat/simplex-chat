---
title: SimpleX Chat terminálová
revision: 31.01.2023
---
| Aktualizováno 31.01.2023 | Jazyky: CZ, [EN](/docs/CLI.md), [FR](/docs/lang/fr/CLI.md) |

# SimpleX Chat terminálová (konzolová) aplikace pro Linux/MacOS/Windows

## Obsah

- Funkce chatu v terminálu](#terminal-chat-features)
- Instalace](#🚀-installation)
  - [Stáhnout klienta chatu](#download-chat-client)
    - [Linux a MacOS](#linux-and-macos)
    - [Windows](#windows)
  - [Sestavit ze zdrojového kódu](#build-from-source)
    - [Použití Dockeru](#using-docker)
    - [Použití zásobníku Haskell](#using-haskell-stack)
- [Použití](#usage)
  - [Spuštění klienta chatu](#running-the-chat-client)
  - [Přístup k serverům pro zasílání zpráv přes Tor](#access-messaging-servers-via-tor-beta)
  - [Jak používat chat SimpleX](#how-to-use-simplex-chat)
  - [Skupiny](#groups)
  - [Posílání souborů](#sending-files)
  - [Kontaktní adresy uživatelů](#user-contact-addresses)
  - [Přístup k historii chatu](#access-chat-history)

## Funkce chatu v terminálu

- Chat 1:1 s více lidmi v jednom okně terminálu.
- Skupinové zasílání zpráv.
- Posílání souborů kontaktům a skupinám.
- Kontaktní adresy uživatelů - navázání spojení prostřednictvím odkazů na více kontaktů.
- Zprávy uchovávané v místní databázi SQLite.
- Automaticky vyplňované jméno příjemce - po navázání spojení stačí napsat zprávu a odpovědět odesílateli.
- K dispozici ukázkové servery SMP, které jsou v aplikaci předkonfigurovány - nebo si můžete [nasadit vlastní server](https://github.com/simplex-chat/simplexmq#using-smp-server-and-smp-agent).
- Žádná globální identita ani žádná jména nejsou pro server(y) viditelná, což zajišťuje plné soukromí vašich kontaktů a konverzací.
- Dvě vrstvy šifrování E2E (double-ratchet pro duplexní spojení s použitím dohody klíčů X3DH s efemérními klíči Curve448 a NaCl crypto_box pro fronty SMP s použitím klíčů Curve25519) a předávání klíčů příjemců mimo pásmo (viz [Jak používat chat SimpleX](#how-to-use-simplex-chat)).
- Ověřování integrity zpráv (pomocí zahrnutí digestů předchozích zpráv).
- Ověřování každého příkazu/zprávy servery SMP pomocí automaticky generovaných klíčů Ed448.
- Transportní šifrování TLS 1.3.
- Dodatečné šifrování zpráv od serveru SMP k příjemci za účelem snížení korelace provozu.

Veřejné klíče zapojené do výměny klíčů se nepoužívají jako identita, jsou náhodně generovány pro každý kontakt.

Technické podrobnosti viz [Použité šifrovací primitivy](https://github.com/simplex-chat/simplexmq/blob/master/protocol/overview-tjr.md#encryption-primitives-used).

<a name="🚀-installation"></a>

## 🚀 Instalace

### Stáhněte si klienta chatu

#### Linux a MacOS

Chcete-li **nainstalovat** nebo **aktualizovat** `simplex-chat`, měli byste spustit instalační skript. K tomu použijte následující příkaz cURL nebo Wget:

```sh
curl -o- https://raw.githubusercontent.com/simplex-chat/simplex-chat/stable/install.sh | bash
```

```sh
wget -qO- https://raw.githubusercontent.com/simplex-chat/simplex-chat/stable/install.sh | bash
```

Jakmile se chatovací klient stáhne, můžete jej spustit pomocí příkazu `simplex-chat` v terminálu.

Případně můžete binární soubor chatu pro váš systém stáhnout ručně z [latest stable release](https://github.com/simplex-chat/simplex-chat/releases) a spustit jej podle následujícího návodu.

```sh
chmod +x <binary>
mv <binary> ~/.local/bin/simplex-chat
```

(nebo v jakémkoli jiném preferovaném umístění v `PATH`).

V systému MacOS musíte také [povolit spuštění nástroje Gatekeeper](https://support.apple.com/en-us/HT202491).

#### Windows

```sh
move <binary> %APPDATA%/local/bin/simplex-chat.exe
```

### Sestavení ze zdrojových kódů

> **Upozornění:** pro sestavení aplikace použijte zdrojový kód ze [stabilní větve](https://github.com/simplex-chat/simplex-chat/tree/stable).

#### Použití nástroje Docker

V systému Linux můžete spustitelný soubor chatu sestavit pomocí [docker build with custom output](https://docs.docker.com/engine/reference/commandline/build/#custom-build-outputs):

```shell
git clone git@github.com:simplex-chat/simplex-chat.git
cd simplex-chat
git checkout stable
DOCKER_BUILDKIT=1 docker build --output ~/.local/bin .
```

> **Upozornění:** Pokud narazíte na chybu `` verze `GLIBC_2.28' nenalezena ``, obnovte jej pomocí základního obrazu `haskell:8.10.7-stretch` (změňte jej ve svém lokálním [Dockerfile](Dockerfile)).

#### V libovolném operačním systému

1. Nainstalujte [Haskell GHCup](https://www.haskell.org/ghcup/), GHC 8.10.7 a cabal:

```shell
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

2. Sestavte projekt:

```shell
git clone git@github.com:simplex-chat/simplex-chat.git
cd simplex-chat
git checkout stable
# v Linuxu
apt-get update && apt-get install -y build-essential libgmp3-dev zlib1g-dev
cp scripts/cabal.project.local.linux cabal.project.local
# nebo na MacOS:
# brew install openssl@1.1
# cp scripts/cabal.project.local.mac cabal.project.local
# možná budete muset změnit cabal.project.local tak, aby ukazoval na skutečné umístění openssl
cabal update
cabal install
```

## Použití

### Spuštění klienta chatu

Chcete-li spustit klienta chatu, spusťte z terminálu příkaz `simplex-chat`.

Ve výchozím nastavení je datový adresář aplikace vytvořen v domovském adresáři (`~/.simplex` nebo `%APPDATA%/simplex` ve Windows) a jsou v něm inicializovány dva databázové soubory SQLite `simplex_v1_chat.db` a `simplex_v1_agent.db`.

Chcete-li zadat jiný prefix cesty k souborům databáze, použijte volbu příkazového řádku `-d`:

```shell
$ simplex-chat -d alice
```

Spuštění výše uvedeného příkladu vytvoří databázové soubory `alice_v1_chat.db` a `alice_v1_agent.db` v aktuálním adresáři.

Na serveru Linode jsou umístěny tři výchozí servery SMP - jsou [předkonfigurovány v aplikaci](https://github.com/simplex-chat/simplex-chat/blob/stable/src/Simplex/Chat/Options.hs#L42).

Pokud jste nasadili vlastní SMP server(y), můžete klienta nakonfigurovat pomocí volby `-s`:

```shell
$ simplex-chat -s smp://LcJUMfVhwD8yxjAiSaDzzGF3-kLG4Uh0Fl_ZIjrRwjI=@smp.example.com
```

Řetězec zakódovaný v Base64url před adresou serveru je offline otisk certifikátu serveru, který je ověřován klientem během TLS handshake.

S lidmi můžete stále hovořit pomocí výchozího nebo jakéhokoli jiného serveru - ovlivňuje to pouze umístění fronty zpráv při navazování spojení (a fronta odpovědí může být na jiném serveru, jak nastaví klient druhé strany).

Spusťte `simplex-chat -h` a zobrazte všechny dostupné možnosti.

### Přístup k serverům pro zasílání zpráv přes Tor

Nainstalujte Tor a spusťte jej jako proxy server SOCKS5 na portu 9050, např. na Macu můžete:

```
brew install tor
brew services start tor
```

Pro přístup k serverům přes Tor použijte volbu `-x`:

```
simplex-chat -x
```

Můžete také použít volbu `--socks-proxy=ipv4:port` nebo `--socks-proxy=:port` pro konfiguraci hostitele a portu vašeho SOCKS5 proxy serveru, např. pokud jej provozujete na jiném hostiteli nebo portu.

### Jak používat SimpleX chat

Po spuštění chatu budete vyzváni k zadání svého "zobrazovacího jména" a volitelného "celého jména" pro vytvoření místního profilu chatu. Vaše zobrazované jméno je přezdívka, pod kterou se na vás mohou obracet vaše kontakty - není jedinečné a neslouží jako globální identita. Pokud si některé vaše kontakty zvolily stejné zobrazovací jméno, přidá chatovací klient k jejich místnímu zobrazovacímu jménu číselnou příponu.

Následující schéma ukazuje, jak se připojit ke kontaktu a poslat mu zprávu:

<div align="center">
  <img align="center" src="/images/how-to-use-simplex.svg">
</div>

Po nastavení místního profilu zadejte `/c` (pro `/connect`) pro vytvoření nového spojení a vygenerování pozvánky. Tuto pozvánku odešlete svému kontaktu prostřednictvím jakéhokoli jiného kanálu.

Můžete vytvořit více pozvánek zadáním `/connect` vícekrát a odesláním těchto pozvánek příslušným kontaktům, se kterými se chcete spojit.

Pozvánku lze použít pouze jednou, a i kdyby byla zachycena, útočník by ji nemohl použít k odeslání zpráv prostřednictvím této fronty, jakmile váš kontakt potvrdí, že spojení bylo navázáno. Vysvětlení [formátu pozvánky](https://github.com/simplex-chat/simplexmq/blob/master/protocol/agent-protocol.md#connection-request) naleznete v protokolu agenta.

Kontakt, který obdržel pozvánku, by měl zadat `/c <pozvánka>`, aby spojení přijal. Tím se spojení naváže a obě strany jsou o tom informovány.

Poté by měly použít příkazy `@<jméno> <zpráva>` k odesílání zpráv. Můžete také prostě začít psát zprávu a odeslat ji kontaktu, který byl poslední.

Seznam dostupných příkazů zobrazíte pomocí `/help` v chatu.

### Skupiny

Skupinu vytvoříte příkazem `/g <group>` a kontakty do ní přidáte příkazem `/a <group> <name>`. Do skupiny pak můžete posílat zprávy zadáním `#<skupina> <zpráva>`. Pro další příkazy použijte `/help groups`.

![simplex-chat](/images/groups.gif)

> **Upozornění**: skupiny nejsou uloženy na žádném serveru, jsou vedeny jako seznam členů v databázi aplikace, kterým budou zprávy zasílány.

### Odesílání souborů

Soubor můžete odeslat kontaktu pomocí `/f @<contact> <soubor_cesta>` - příjemce jej bude muset před odesláním přijmout. Pro další příkazy použijte `/help files`.

![simplex-chat](/images/files.gif)

Soubory můžete posílat skupině pomocí `/f #<skupina> <soubor_cesta>`.

### Kontaktní adresy uživatelů

Jako alternativu k jednorázovým pozvánkovým odkazům můžete vytvořit dlouhodobou adresu pomocí `/ad` (pro `/adresa`). Vytvořenou adresu pak můžete sdílet libovolným kanálem a používat ji ostatními uživateli jako odkaz na žádost o kontakt pomocí `/c <uživatelská_kontaktní_adresa>`.

Příchozí žádosti můžete přijmout nebo odmítnout pomocí příkazů `/ac <jméno>` a `/rc <jméno>`.

Uživatelská adresa je "dlouhodobá" v tom smyslu, že se jedná o odkaz pro vícenásobné použití - lze ji používat, dokud ji uživatel nesmaže, v takovém případě by všechna navázaná spojení zůstala stále aktivní (na rozdíl od toho, jak to funguje u e-mailu, kdy změna adresy vede k tomu, že vám lidé nemohou posílat zprávy).

Pro ostatní příkazy použijte `/help address`.

![simplex-chat](/images/user-addresses.gif)
