---
title: Hostování vlastního serveru SMP
revision: 05.06.2023
---
| Aktualizováno 05.06.2023 | Jazyky: CZ, [EN](/docs/SERVER.md), [FR](/docs/lang/fr/SERVER.md), [PL](/docs/lang/pl/SERVER.md) |

# Hostování vlastního serveru SMP

## Přehled

SMP server je relay server používaný k předávání zpráv v síti SimpleX. Aplikace SimpleX Chat mají přednastavené servery (pro mobilní aplikace jsou to smp11, smp12 a smp14.simplex.im), ale konfiguraci aplikace můžete snadno změnit a používat jiné servery.

Klienti SimpleX pouze určují, který server bude použit pro příjem zpráv, a to pro každý kontakt (nebo spojení skupiny s členem skupiny) zvlášť, přičemž tyto servery jsou pouze dočasné, protože adresa pro doručování se může změnit.

_Upozornění_: když změníte servery v konfiguraci aplikace, ovlivní to pouze to, který server bude použit pro nové kontakty, stávající kontakty se na nové servery automaticky nepřesunou, ale můžete je přesunout ručně pomocí tlačítka ["Změnit adresu příjmu"](../../../blog/20221108-simplex-chat-v4.2-security-audit-new-website.md#change-your-delivery-address-beta) na stránkách s informacemi o kontaktech/členech - brzy bude automatizováno.

## Instalace

0. Nejprve nainstalujte `smp-server`:

   - Ruční nasazení:

     - [Kompilace ze zdrojových kódů](https://github.com/simplex-chat/simplexmq#using-your-distribution)
     - [Použití předkompilovaných binárních souborů](https://github.com/simplex-chat/simplexmq#install-binaries)

   - Alternativně můžete `smp-server` nasadit pomocí:
     - [Docker kontejner](https://github.com/simplex-chat/simplexmq#using-docker-1)
     - [Linode StackScript](https://github.com/simplex-chat/simplexmq#deploy-smp-server-on-linode)

Ruční instalace vyžaduje několik předběžných úkonů:

1. Vytvoření uživatele a skupiny pro `smp-server`:

   ```sh
   sudo useradd -m smp
   ```

2. Vytvořte potřebné adresáře a přiřaďte jim oprávnění:

   ```sh
   sudo mkdir -p /var/opt/simplex /etc/opt/simplex
   sudo chown smp:smp /var/opt/simplex /etc/opt/simplex /etc/opt/simplex
   ```

3. Povolte port `smp-server` ve firewallu:

   ```sh
   # Pro Ubuntu
   sudo ufw allow 5233/tcp
   # Pro Fedora
   sudo firewall-cmd --permanent --add-port=5223/tcp && \
   sudo firewall-cmd --reload
   ```

4. **Volitelné** - Pokud používáte distribuci s `systemd`, vytvořte soubor `/etc/systemd/system/smp-server.service` s následujícím obsahem:

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
   Restart=vždy
   RestartSec=10
   LimitNOFILE=65535
   [Install]
   WantedBy=multi-user.target
   ```

   A spusťte `sudo systemctl daemon-reload`.

## Konfigurace

Chcete-li zjistit, které možnosti jsou k dispozici, spusťte `smp-server` bez příznaků:

```sh
sudo su smp -c smp-server

...
Dostupné příkazy:
  init Inicializace serveru - vytvoří /etc/opt/simplex a
                           /var/opt/simplex adresáře a konfigurační soubory.
  start Spustí server (konfigurace:
                           /etc/opt/simplex/smp-server.ini).
  delete Odstranění konfiguračních a protokolových souborů
```

Další nápovědu můžete získat příkazem `sudo su smp -c "smp-server <příkaz> -h"`

Poté musíme nakonfigurovat `smp-server`:

### Interaktivně

Spusťte následující příkaz:

```sh
sudo su smp -c "smp-server init"
```

Je třeba zvážit několik možností:

- `Povolit ukládání protokolu pro obnovení front a zpráv při restartu serveru (Yn):`

  Zadáním `y` povolíte ukládání a obnovu spojení a zpráv při restartu serveru.

  _Pozor_: je důležité použít SIGINT pro restart serveru, protože jinak nebudou nedoručené zprávy obnoveny. Spojení budou obnovena bez ohledu na to, jakým způsobem je server restartován, protože na rozdíl od zpráv jsou při každé změně přidávána do protokolu pouze pro doplnění.

- `Zapnout protokolování denních statistik (yN):`

  Zadáním `y` povolíte protokolování statistik ve formátu CSV, které lze například použít k zobrazení souhrnných grafů využití v `Grafanu`.

Tyto statistiky zahrnují denní počty vytvořených, zajištěných a smazaných front, odeslaných a přijatých zpráv a také denní, týdenní a měsíční počty aktivních front (tj. front, které byly použity pro nějaké zprávy). Domníváme se, že tyto informace neobsahují nic, co by umožňovalo korelovat různé fronty jako patřící stejným uživatelům, ale pokud se domníváte, že to lze nějak zneužít, dejte nám prosím důvěrně vědět.

- `Vyžadovat heslo pro vytvoření nové fronty zpráv?`

  Zadejte `r` nebo své libovolné heslo pro ochranu heslem `smp-server` nebo `n` pro vypnutí ochrany heslem.

- `Zadejte FQDN serveru nebo IP adresu pro certifikát (127.0.0.1):`

  Zadejte svou doménu nebo ip adresu, na které běží váš smp-server - bude zahrnuta do certifikátů serveru a také vypsána jako součást adresy serveru.

### Prostřednictvím voleb příkazového řádku

Spusťte následující příkaz:

```sh
sudo su smp -c "smp-server init -h"

...
Dostupné možnosti:
  -l,--store-log Povolit protokol úložiště pro perzistenci
  -s,--daily-stats Povolí protokolování denních statistik serveru
  -a,--sign-algorithm ALG Algoritmus podpisu používaný pro certifikáty TLS:
                           ED25519, ED448 (výchozí: ED448).
  --ip IP IP adresa serveru, používaná jako Common Name pro TLS online
                           certifikátu, pokud není zadáno FQDN
                           (výchozí: "127.0.0.1")
  -n,--fqdn FQDN FQDN serveru použitý jako Common Name pro certifikát TLS online
                           certifikát
  --no-password Povolit vytváření nových front bez hesla
  --password PASSWORD Nastavení hesla pro vytváření nových front zpráv
  -y,--yes Neinteraktivní inicializace pomocí příkazového řádku
                           volby
  -h,--help Zobrazí text nápovědy
```

Měli byste určit, které příznaky jsou pro váš případ použití potřebné, a poté spustit `smp-server init` s příznakem `-y` pro neinteraktivní inicializaci:

```sh
sudo su smp -c "smp-server init -y -<vůj příznak> <vaše volba>"
```

Spusťte například:

```sh
sudo su smp -c "smp-server init -y -l --ip 192.168.1.5 --heslo test"
```

a inicializujte konfiguraci `smp-serveru` pomocí:

- obnovení spojení a zpráv při restartu serveru (příznak `-l`),
- IP adresa `192.168.1.5`,
- chránit `smp-server` heslem `test`.

---

Poté je instalace dokončena a ve výstupu teminálu byste měli vidět něco takového:

```sh
Certificate request self-signature ok
subject=CN = 127.0.0.1
Server je inicializován, konfiguraci můžete upravit v souboru /etc/opt/simplex/smp-server.ini.
Spusťte `smp-server start` pro spuštění serveru.
----------
Měli byste bezpečně uložit soukromý klíč CA a odstranit jej ze serveru.
Pokud dojde ke kompromitaci pověření TLS serveru, lze tento klíč použít k podpisu nového, přičemž zůstane zachována stejná identita serveru a navázaná spojení.
Umístění soukromého klíče CA: /etc/opt/simplex/ca.key.
----------
SMP server v3.4.0
Otisk prstu: d5fcsc7hhtPpexYUbI2XPxDbyU2d3WsVmROimcL90ss=
Adresa serveru: smp://d5fcsc7hhtPpexYUbI2XPxDbyU2d3WsVmROimcL90ss=:V8ONoJ6ICwnrZnTC_QuSHfCEYq53uLaJKQ_oIC6-ve8=@<hostnames>
```

Výše uvedená adresa serveru by měla být použita v konfiguraci klienta a pokud jste přidali heslo k serveru, mělo by být sdíleno s ostatními lidmi pouze tehdy, když jim chcete povolit používat váš server pro příjem zpráv (všechny vaše kontakty budou moci posílat zprávy, protože nevyžadují heslo). Pokud jste při inicializaci předali IP adresu nebo názvy hostitelů, budou vypsány jako součást adresy serveru, jinak nahraďte `<hostnames>` skutečnými adresami serverů.

## Dokumentace

Všechny potřebné soubory pro `smp-server` jsou umístěny ve složce `/etc/opt/simplex/`.

Uložené zprávy, spojení, statistiky a protokol serveru jsou umístěny ve složce `/var/opt/simplex/`.

### Adresa serveru SMP

Adresa serveru SMP má následující formát:

```
smp://<otisk prstu>[:<heslo>]@<jméno_veřejného_hostitele>[,<jméno_hostitele>]
```

- `<otisk prstu>`

  Váš otisk certifikátu `smp-server`. Otisk svého certifikátu můžete zkontrolovat v souboru `/etc/opt/simplex/fingerprint`.

- **nepovinné** `<heslo>`

  Vaše nakonfigurované heslo `smp-serveru`. Nakonfigurované heslo můžete zkontrolovat v souboru `/etc/opt/simplex/smp-server.ini` v sekci `[AUTH]` v poli `create_password:`.

- `<public_hostname>`, **volitelně** `<onion_hostname>`.

  Vaše nakonfigurované jméno (jména) hostitele `smp-serveru`. Nakonfigurované hostitele můžete zkontrolovat v souboru `/etc/opt/simplex/smp-server.ini`, v části `[TRANSPORT]` v poli `host:`.

### Příkazy Systemd

Chcete-li spustit `smp-server` při startu hostitele, spusťte:

```sh
sudo systemctl enable smp-server.service

Vytvořen symlink /etc/systemd/system/multi-user.target.wants/smp-server.service → /etc/systemd/system/smp-server.service.
```

Chcete-li spustit `smp-server`, spusťte:

```sh
sudo systemctl start smp-server.service
```

Chcete-li zkontrolovat stav `smp-serveru`, spusťte:

```sh
sudo systemctl status smp-server.service

● smp-server.service - server SMP
     Načteno: načteno (/etc/systemd/system/smp-server.service; povoleno; předvolba dodavatele: povoleno)
     Aktivní: aktivní (běží) od so 2022-11-23 19:23:21 UTC; před 1min 48s
   Hlavní PID: 30878 (smp-server)
     CGroup: /docker/5588ab759e80546b4296a7c50ffebbb1fb7b55b8401300e9201313b720989aa8/system.slice/smp-server.service
             └─30878 smp-server start

Nov 23 19:23:21 5588ab759e80 systemd[1]: Spuštěn SMP server.
Nov 23 19:23:21 5588ab759e80 smp-server[30878]: SMP server v3.4.0
Nov 23 19:23:21 5588ab759e80 smp-server[30878]: Fingerprint: d5fcsc7hhtPpexYUbI2XPxDbyU2d3WsVmROimcL90ss=
Nov 23 19:23:21 5588ab759e80 smp-server[30878]: Adresa serveru: smp://d5fcsc7hhtPpexYUbI2XPxDbyU2d3WsVmROimcL90ss=:V8ONoJ6ICwnrZnTC_QuSHfCEYq53uLaJKQ_oIC6-ve8=@<hostnames>
Nov 23 19:23:21 5588ab759e80 smp-server[30878]: Uložit protokol: /var/opt/simplex/smp-server-store.log
Nov 23 19:23:21 5588ab759e80 smp-server[30878]: Naslouchání na portu 5223 (TLS)...
Nov 23 19:23:21 5588ab759e80 smp-server[30878]: neaktivní klienti nevypršeli.
Nov 23 19:23:21 5588ab759e80 smp-server[30878]: vytváření nových front vyžaduje heslo
```

Chcete-li zastavit `smp-server`, spusťte:

```sh
sudo systemctl stop smp-server.service
```

Chcete-li zkontrolovat ocas protokolu `smp-server`, spusťte:

```sh
sudo journalctl -fu smp-server.service

Nov 23 19:23:21 5588ab759e80 systemd[1]: Spuštěn SMP server.
Nov 23 19:23:21 5588ab759e80 smp-server[30878]: SMP server v3.4.0
Nov 23 19:23:21 5588ab759e80 smp-server[30878]: Fingerprint: d5fcsc7hhtPpexYUbI2XPxDbyU2d3WsVmROimcL90ss=
Nov 23 19:23:21 5588ab759e80 smp-server[30878]: Adresa serveru: smp://d5fcsc7hhtPpexYUbI2XPxDbyU2d3WsVmROimcL90ss=:V8ONoJ6ICwnrZnTC_QuSHfCEYq53uLaJKQ_oIC6-ve8=@<hostnames>
Nov 23 19:23:21 5588ab759e80 smp-server[30878]: Uložit protokol: /var/opt/simplex/smp-server-store.log
Nov 23 19:23:21 5588ab759e80 smp-server[30878]: Naslouchání na portu 5223 (TLS)...
Nov 23 19:23:21 5588ab759e80 smp-server[30878]: neaktivní klienti nevypršeli.
Nov 23 19:23:21 5588ab759e80 smp-server[30878]: vytváření nových front vyžaduje heslo
```

### Monitoring

Statistiky `smp-serveru` pro `Grafana` dashboard můžete povolit nastavením hodnoty `on` v souboru `/etc/opt/simplex/smp-server.ini` v sekci `[STORE_LOG]` v poli `log_stats:`.

Protokoly budou uloženy jako soubor `csv` v souboru `/var/opt/simplex/smp-server-stats.daily.log`. Pole pro soubor `csv` jsou:

```sh
fromTime,qCreated,qSecured,qDeleted,msgSent,msgRecv,dayMsgQueues,weekMsgQueues,monthMsgQueues
```

- `fromTime` - časová značka; datum a čas události

- `qCreated` - int; vytvořené fronty

- `qSecured` - int; vytvořené fronty

- `qDeleted` - int; smazané fronty

- `msgSent` - int; odeslané zprávy

- `msgRecv` - int; přijaté zprávy

- `dayMsgQueues` - int; aktivní fronty za den

- `weekMsgQueues` - int; aktivní fronty za týden

- `monthMsgQueues` - int; aktivní fronty za měsíc

Pro import `csv` do `Grafana` je třeba:

1. Nainstalovat zásuvný modul Grafana: [Grafana - CSV datasource](https://grafana.com/grafana/plugins/marcusolsson-csv-datasource/)

2. Povolit místní režim připojením následujícího:

   ```sh
   [plugin.marcusolsson-csv-datasource].
   allow_local_mode = true
   ```

   ... do `/etc/grafana/grafana.ini`

3. Přidejte zdroj dat CSV:

   - V postranní nabídce klikněte na záložku Configuration (ikona ozubeného kola).
   - V pravém horním rohu karty Zdroje dat klikněte na tlačítko Přidat zdroj dat.
   - Do vyhledávacího pole zadejte "CSV" a vyhledejte zdroj dat CSV.
   - Klikněte na výsledek hledání s nápisem "CSV".
   - Do pole URL zadejte soubor, který odkazuje na obsah CSV

4. Hotovo! Měli byste být schopni vytvořit vlastní řídicí panel se statistikami.

Další dokumentaci naleznete na adrese: [CSV Data Source for Grafana - Documentation](https://grafana.github.io/grafana-csv-datasource/)

### Konfigurace aplikace pro použití serveru

Chcete-li aplikaci nakonfigurovat tak, aby používala váš server pro zasílání zpráv, zkopírujte jeho úplnou adresu včetně hesla a přidejte ji do aplikace. Máte možnost používat svůj server společně s přednastavenými servery nebo bez nich - můžete je odebrat nebo zakázat.

Adresu svého serveru můžete také sdílet se svými přáteli tak, že je necháte naskenovat QR kód z nastavení serveru - ten bude obsahovat heslo serveru, takže budou moci přijímat zprávy i prostřednictvím vašeho serveru.

_Upozornění_: pro podporu hesla je třeba mít SMP server verze 4.0. Pokud již máte nasazený server, můžete heslo přidat přidáním do souboru INI serveru.

<img src="/docs/server_config_1.png" width="288"> &nbsp;&nbsp; <img src="/docs/server_config_2.png" width="288"> &nbsp;&nbsp; <img src="/docs/server_config_3.png" width="288">
