---
title: Přispívání překladů do SimpleX Chat
revision: 07.02.2023
---
| Aktualizováno 07.02.2023 | Jazyky: CZ, [EN](/docs/TRANSLATIONS.md), [PL](/docs/lang/pl/TRANSLATIONS.md) |

# Přispívání překladů do SimpleX Chat

Obrovské díky za váš zájem o překlady SimpleX Chat - velmi nám to pomáhá zpřístupnit jej širšímu okruhu uživatelů a vaší pomoci si opravdu vážíme.

Vyžaduje to značnou časovou investici - většina lidí ji zpočátku podceňuje - a průběžnou údržbu v průběhu vývoje aplikace.

Tento dokument vznikl proto, abychom tento proces urychlili, a podělili se s vámi o některé důležité "háčky", které jsme objevili při práci s Weblate - platformou, kterou používáme pro překlady rozhraní.

## Než začnete překládat

1. Vytvořte si účet na Weblate a použijte stejný e-mail, který používáte na GitHubu - díky tomu budou vaše příspěvky spojeny s vaším účtem na GitHubu a v některých případech vám to může pomoci. Jakmile bude překlad uvolněn pro uživatele, přidáme jméno vašeho účtu do [seznamu překladatelů](https://github.com/simplex-chat/simplex-chat#translate-the-apps).

2. Před zahájením překladu je třeba podepsat jednoduchou smlouvu s přispěvatelem prostřednictvím Weblate - to proto, aby se předešlo případným konfliktům kolem práv duševního vlastnictví.

3. Můžeme vás také přidat do skupiny překladatelů pro případné dotazy a aktualizace - spojte se se mnou přes chat.

## Průběh překladu

1. Začněte prosím od [aplikace pro Android](https://hosted.weblate.org/projects/simplex-chat/android/), a to jak při časově nejnáročnějším počátečním překladu, tak při pozdějším přidávání případných řetězců. Za prvé, řetězce pro iOS se mohou ve Weblate objevit s určitým zpožděním, protože to od nás vyžaduje ruční krok, než se zobrazí. Zadruhé, aplikace Android je nastavena jako slovník pro aplikaci iOS a 2/3 všech řetězců vyžadují jen kliknutí pro jejich převod z Androidu do iOS (to ještě nějakou dobu trvá, Weblate to bohužel neautomatizuje).

2. Některé řetězce není třeba překládat, ale přesto je třeba je překopírovat - v uživatelském rozhraní Weblate je k tomu tlačítko:

<img src="/docs/images/weblate_1.png" alt="weblate: zkopírovat zdroj do překladu" width="100%">

3. Weblate má také automatické návrhy, které mohou proces urychlit. Někdy je lze použít tak, jak jsou, jindy je třeba je upravit - kliknutím je použijete v překladu.

4. Jakmile jsou všechny řetězce v aplikaci pro Android přeloženy, zkontrolujte je, abyste zajistili konzistentní styl a jazyk, aby se pro podobné uživatelské akce důsledně používala stejná slova jako v angličtině. Někdy budete muset použít různá slova v případech, kdy angličtina má jen jedno, snažte se prosím tyto volby používat důsledně v podobných kontextech, aby to bylo pro koncové uživatele jednodušší.

5. Při překladu [aplikace pro iOS](https://hosted.weblate.org/projects/simplex-chat/ios/) je velká část řetězců naprosto stejná - lze je jedním kliknutím překopírovat do sekce glosář. Vizuální nápovědou, že to lze provést, je to, že celý zdrojový řetězec je zvýrazněn žlutě. Mnoho dalších řetězců je velmi podobných, liší se pouze syntaxí interpolace nebo způsobem použití tučného písma - vyžadují minimální úpravy. Existují některé řetězce, které jsou jedinečné pro platformu iOS - ty je třeba přeložit zvlášť.

<img src="/docs/images/weblate_2.png" alt="weblate: automatické návrhy" width="100%">

## Po dokončení překladu

Jakmile budou aplikace pro Android i iOS přeloženy, dejte nám prosím vědět.

My pak:
  - zkontrolujeme všechny překlady a navrhneme případné opravy - to také zabere trochu času :)
  - sloučíme je do zdrojového kódu - během toho bude weblate uzamčen pro změny.
  - vytvoříme beta verze aplikací pro iOS i Android - můžeme vás také přidat do interních skupin testerů, abyste mohli aplikace instalovat dříve než ostatní.
  - Vydáme ji pro naše uživatele beta verzí - jedná se o více než tisíc lidí, kteří používají naše beta verze.
  - Vydání aplikace a uvedení nového jazyka v oznámení.

*Upozornění*: pokud je to možné, snažíme se zachovat konzistentní funkce aplikace mezi platformami Android a iOS, takže nový jazyk vydáme a oznámíme, až budou obě platformy přeloženy. Neznamená to, že to musíte udělat vy, ale budeme muset počkat, až druhou platformu přeloží někdo jiný. Pokud ale začnete od Androidu, přidání iOS obvykle trvá 3-4x kratší dobu.

## Co bude dál

1. Jakmile budeme aplikaci aktualizovat, zveřejníme aktualizace ve skupině překladatelů. Nemáte absolutně žádnou povinnost tyto dodatečné řetězce překládat. Nesmírně si však vážíme toho, pokud tak učiníte, protože uživatelé mají mnohem lepší zážitek, když jsou závislí na vašich překladech, než když nějaká nová část aplikace přeložena není.

2. Adopci ve vaší zemi / jazykové skupině můžete pomoci také překládáním našich dokumentů - právě jsme s tím začali - a také obsahu našich webových stránek. Objevilo se mnoho žádostí o to a v současné době přidáváme překladový rámec pro webové stránky.

3. Také pokud chcete být moderátorem/adminem skupiny uživatelů ve vašem jazyce, jakmile bude aplikace přeložena, můžeme takovou skupinu hostit - připravujeme komunitní směrnice a přidáváme do aplikace některé moderátorské nástroje, které budou vydány v březnu ve verzi v5.

Ještě jednou děkujeme, že nám pomáháte rozvíjet SimpleX Chat!

Evgeny, zakladatel SimpleX Chat.
