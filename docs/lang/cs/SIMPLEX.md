---
title: Platforma SimpleX
revision: 07.02.2023
---
| Aktualizováno 07.02.2023 | Jazyky: CZ, [EN](/docs/SIMPLEX.md), [FR](/docs/lang/fr/SIMPLEX.md), [PL](/docs/lang/pl/SIMPLEX.md) |

# Platforma SimpleX - motivace a srovnání

## Problémy

Stávající chatovací platformy a protokoly mají některé nebo všechny následující problémy:

- Nedostatek soukromí uživatelského profilu a kontaktů (soukromí metadat).
- Žádná ochrana (nebo pouze volitelná ochrana) implementací [E2EE][1] před útoky MITM prostřednictvím poskytovatele.
- Nevyžádané zprávy (spam a zneužití).
- Chybějící vlastnictví a ochrana dat.
- Složitost použití všech necentralizovaných protokolů pro netechnické uživatele.

Koncentrace komunikace v malém počtu centralizovaných platforem činí řešení těchto problémů poměrně obtížným.

## Navrhované řešení

Navrhovaný zásobník protokolů řeší tyto problémy tím, že zprávy i kontakty jsou uloženy pouze v klientských zařízeních, čímž se role serverů omezuje na pouhé zprostředkovatele zpráv, kteří vyžadují pouze autorizaci zpráv odesílaných do front, ale NEvyžadují autentizaci uživatelů - chráněny jsou nejen zprávy, ale i metadata, protože uživatelé nemají přiřazeny žádné identifikátory - na rozdíl od jiných platforem.

Více informací o cílech a technickém návrhu platformy naleznete v dokumentu [SimpleX whitepaper](https://github.com/simplex-chat/simplexmq/blob/master/protocol/overview-tjr.md).

## Proč používat SimpleX

## Jedinečný přístup SimpleX k ochraně soukromí a zabezpečení

Každý by měl dbát na soukromí a bezpečnost své komunikace - i obyčejné konverzace vás mohou ohrozit.

### Úplné soukromí vaší identity, profilu, kontaktů a metadat.

**Na rozdíl od všech ostatních existujících platforem pro zasílání zpráv nemá SimpleX žádné identifikátory přiřazené uživatelům** - k identifikaci svých uživatelů nepoužívá telefonní čísla (jako Signal nebo WhatsApp), adresy založené na doméně (jako e-mail, XMPP nebo Matrix), uživatelská jména (jako Telegram), veřejné klíče nebo dokonce náhodná čísla (jako všechny ostatní messengery) - ani nevíme, kolik lidí SimpleX používá.

K doručování zpráv místo identifikátorů uživatelů, které používají všechny ostatní platformy, používá SimpleX adresy jednosměrných (simplexních) front zpráv. Používání služby SimpleX je jako mít pro každý kontakt jinou e-mailovou adresu nebo telefonní číslo, ale bez starostí se správou všech těchto adres. V blízké budoucnosti budou aplikace SimpleX také automaticky měnit fronty zpráv a přesouvat konverzace z jednoho serveru na druhý, aby uživatelům poskytly ještě lepší soukromí.

Tento přístup chrání soukromí toho, s kým komunikujete, a skrývá ho před servery platformy SimpleX a před jakýmikoli pozorovateli. Své soukromí můžete dále zlepšit nastavením přístupu k síti tak, abyste se k serverům SimpleX připojovali prostřednictvím některé překryvné transportní sítě, např. sítě Tor.

### Nejlepší ochrana proti spamu a zneužití

Protože na platformě SimpleX nemáte žádný identifikátor, nelze vás kontaktovat, pokud nesdílíte odkaz s jednorázovou pozvánkou nebo volitelnou dočasnou uživatelskou adresu. I v případě volitelných uživatelských adres je sice lze využít k zasílání nevyžádaných kontaktů, ale můžete je změnit nebo zcela odstranit, aniž byste přišli o jakékoli spojení.

### Úplné vlastnictví, kontrola a zabezpečení vašich údajů

SimpleX ukládá všechna uživatelská data v klientských zařízeních, zprávy jsou pouze dočasně uchovávány na relay serverech SimpleX, dokud nejsou přijaty.

Používáme přenosný formát databáze, který lze použít na všech podporovaných zařízeních - brzy přidáme možnost exportovat databázi chatu z mobilní aplikace, aby ji bylo možné použít na jiném zařízení.

Na rozdíl od serverů federativních sítí (e-mail, XMPP nebo Matrix) servery SimpleX neukládají uživatelské účty, pouze předávají zprávy příjemcům, čímž chrání soukromí obou stran. Mezi odesílaným a přijímaným provozem serveru nejsou žádné společné identifikátory ani šifrované zprávy, a to díky dodatečné vrstvě šifrování doručovaných zpráv. Pokud tedy kdokoli sleduje provoz serveru, nemůže snadno zjistit, kdo s kým komunikuje (známé útoky na korelaci provozu viz [SimpleX whitepaper](https://github.com/simplex-chat/simplexmq/blob/master/protocol/overview-tjr.md)).

### Uživatelé vlastní síť SimpleX

Můžete používat SimpleX s vlastními servery a přitom komunikovat s lidmi, kteří používají servery předkonfigurované v aplikacích nebo jakékoli jiné servery SimpleX.

Platforma SimpleX používá otevřený protokol a poskytuje SDK pro vytváření chatovacích botů, což umožňuje implementaci služeb, s nimiž mohou uživatelé komunikovat prostřednictvím aplikací SimpleX Chat - opravdu se těšíme, jaké služby SimpleX bude možné vytvořit.

Pokud uvažujete o vývoji s platformou SimpleX, ať už jde o služby chatovacích botů pro uživatele aplikací SimpleX, nebo o integraci knihovny SimpleX Chat do vašich mobilních aplikací, obraťte se na nás pro případné rady a podporu.

## Srovnání s jinými protokoly

| | SimpleX chat | Signal, big platforms | XMPP, Matrix | P2P protocols |
| :--------------------------------------------- | :----------------: | :-------------------: | :-------------: | :-------------: |
| Vyžaduje uživatelské identifikátory | Ne = private | Ano<sup>1</sup> | Ano<sup>2</sup> | Yes<sup>3</sup> |
| Možnost MITM útoku | Žádná = bezpečné | Ano<sup>4</sup> | Ano | Ano | Ano |
| Závislost na DNS | Ne = neumlčitelné | Ano | Ano | Ne |
| Single operator or network | Ne = decentralizované | Ano | Ne | Ano<sup>5</sup> |
| Centrální součást nebo jiný celosíťový útok | Ne = neumlčitelné | Ano | Ano<sup>2</sup> | Ano<sup>6</sup> |

1. Obvykle na základě telefonního čísla, v některých případech na základě uživatelských jmen.
2. Založeno na DNS.
3. Veřejný klíč nebo jiné globálně jedinečné ID.
4. Pokud jsou servery provozovatele kompromitovány.
5. Sítě P2P a sítě založené na kryptoměnách jsou sice distribuované, ale nejsou decentralizované - fungují jako jediná síť s jediným jmenným prostorem uživatelských adres.
6. Sítě P2P mají buď centrální autoritu, nebo může být kompromitována celá síť - viz následující část.

## Srovnání s protokoly pro zasílání zpráv [P2P][9].

Existuje několik protokolů P2P pro chatování/zprávy a jejich implementací, jejichž cílem je vyřešit problém soukromí a centralizace, ale mají svůj vlastní soubor problémů, kvůli kterým jsou méně spolehlivé než navrhovaný návrh, složitější na implementaci a analýzu a zranitelnější vůči útokům.

1. Sítě [P2P][9] používají k směrování zpráv/požadavků v síti některou variantu [DHT][10]. Implementace DHT mají složité návrhy, které musí vyvažovat spolehlivost, záruku doručení a latenci. Navrhovaný návrh má lepší záruky doručení i nižší latenci (zpráva je předávána vícekrát paralelně, pokaždé přes jeden uzel, přičemž se používají servery vybrané příjemcem, zatímco v sítích P2P je zpráva předávána přes `O(log N)` uzlů postupně, přičemž se používají uzly vybrané algoritmem).

2. Navrhovaný návrh na rozdíl od většiny sítí P2P nemá žádné globální identifikátory uživatelů, a to ani dočasné.

3. P2P samo o sobě neřeší problém [útoku MITM][2] a většina existujících řešení nepoužívá pro počáteční výměnu klíčů zprávy mimo pásmo. Navrhované řešení využívá pro počáteční výměnu klíčů zprávy mimo pásmo nebo v některých případech již existující bezpečná a důvěryhodná spojení.

4. Implementace P2P mohou být blokovány některými poskytovateli internetu (jako například [BitTorrent][11]). Navrhovaný návrh je transportně agnostický - může fungovat přes standardní webové protokoly a servery mohou být nasazeny na stejných doménách jako webové stránky.

5. Všechny známé sítě P2P jsou pravděpodobně zranitelné vůči [Sybilovu útoku][12], protože každý uzel je zjistitelný a síť funguje jako celek. Známá opatření ke snížení pravděpodobnosti Sybilova útoku buď vyžadují centralizovanou složku, nebo nákladný [proof of work][13]. Navrhovaný návrh naopak nemá žádnou zjistitelnost serverů - servery nejsou propojeny, nejsou známy navzájem ani všem klientům. Síť SimpleX je fragmentovaná a funguje jako několik izolovaných spojení. To znemožňuje útoky na celou síť SimpleX - i když jsou některé servery kompromitovány, ostatní části sítě mohou fungovat normálně a postižení klienti mohou přejít na používání jiných serverů, aniž by ztratili kontakty nebo zprávy.

6. Sítě P2P jsou pravděpodobně [zranitelné][14] vůči [útoku DRDoS][15]. V navrhovaném návrhu klienti pouze předávají provoz ze známého důvěryhodného spojení a nelze je použít k odrážení a zesilování provozu v celé síti.

[1]: https://en.wikipedia.org/wiki/End-to-end_encryption
[2]: https://en.wikipedia.org/wiki/Man-in-the-middle_attack
[9]: https://en.wikipedia.org/wiki/Peer-to-peer
[10]: https://en.wikipedia.org/wiki/Distributed_hash_table
[11]: https://en.wikipedia.org/wiki/BitTorrent
[12]: https://en.wikipedia.org/wiki/Sybil_attack
[13]: https://en.wikipedia.org/wiki/Proof_of_work
[14]: https://www.usenix.org/conference/woot15/workshop-program/presentation/p2p-file-sharing-hell-exploiting-bittorrent
[15]: https://en.wikipedia.org/wiki/Denial-of-service_attack#Reflected_attack
