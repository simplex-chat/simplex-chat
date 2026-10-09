# The World's Most Secure Messaging

**Ultimate security**: SimpleX network uses the most secure end-to-end encryption, with continuous post-quantum key exchange to protect all messages and metadata.

**Unique privacy**: SimpleX network has no user profile IDs, not even random numbers or keys. It provides better privacy of your contacts, protecting who you talk with from network servers.

**No spam**: nobody can contact you unless you share 1-time link or long-term address.

**Data ownership**: only your device stores your profiles, contacts and messages. You can securely move your data to another device. Servers store encrypted messages only while your device is offline.

**Secure decentralization**: you control which servers to connect to. For security 4 different servers are used in each chat — they can't observe which IP addresses talk to each other.

#### How to connect to others

- Tap new chat button in the corner, then create 1-time link.
- Share the link with your contact via any other messenger or email - it is secure.
- Ask your contact to use the link in the app - click it after the app is installed or paste into the search field in the app.

## Miért egyedülálló a SimpleX

### Teljes magánéletet élvezhet

A SimpleX megvédi a profilhoz tartozó partnereket és azok metaadatait is, elrejtve azokat a SimpleX hálózat kiszolgálói és a megfigyelők elől.

Minden más létező üzenetküldő hálózattól eltérően a SimpleX nem rendelkezik a felhasználókhoz rendelt azonosítókkal — **még véletlenszerű számokkal sem**.

### Véd a kéretlen tartalmaktól és a visszaélésektől

Mivel a SimpleX hálózaton senkinek sincs azonosítója vagy állandó címe, ezért senki sem tud kapcsolatba lépni a felhasználókkal, hacsak nem osztanak meg egy egyszeri vagy ideiglenes felhasználói címet, például QR-kódot vagy hivatkozást.

### A saját adatai felett rendelkezhet

A SimpleX Chat az összes felhasználói adatot kizárólag a klienseken tárolja egy **hordozható titkosított adatbázis-formátumban** —, amely exportálható és átvihető bármely más támogatott eszközre.

A végpontok között titkosított üzenetek átmenetileg a SimpleX átjátszóin tartózkodnak, amíg be nem érkeznek a címzetthez, majd automatikusan véglegesen törlődnek onnan.

### A felhasználóké a SimpleX hálózat

A SimpleX hálózat teljesen decentralizált és független bármely kriptopénztől vagy bármely más hálózattól, kivéve az internetet.

Használhatja **a SimpleXet a saját kiszolgálóival** vagy az általunk biztosított kiszolgálókkal, és továbbra is kapcsolódhat bármely felhasználóhoz.

# Személyazonosságának, profiljának, partnereinek és a metaadatok teljes körű védelme

Más üzenetküldő hálózatoktól eltérően a SimpleX **nem rendel azonosítókat a felhasználókhoz**. Nem támaszkodik telefonszámokra, tartomány-alapú címekre (mint az e-mail, XMPP vagy a Matrix), felhasználónevekre, nyilvános kulcsokra vagy akár véletlenszerű számokra a felhasználók azonosításához — a SimpleX kiszolgálók üzemeltetői nem tudják, hogy hányan használják a kiszolgálóikat.

Az üzenetek kézbesítéséhez a SimpleX egyirányú várólistákat használ az üzenetekhez, [páronkénti, névtelen címekkel](https://csrc.nist.gov/glossary/term/Pairwise_Pseudonymous_Identifier), külön a fogadott és külön az elküldött üzenetekhez, általában különböző kiszolgálókon keresztül.

Ez a kialakítás védi partnerei adatait, elrejtve azt a SimpleX hálózat kiszolgálói és a külső megfigyelők elől. Az IP-címe elrejtésének érdekében a **Tor hálózaton keresztül is kapcsolódhat a SimpleX kiszolgálókhoz**.

# A legjobb védelem a kéretlen tartalmak és a visszaélések ellen

Mivel senki sem rendelkezik azonosítóval a SimpleX hálózaton, ezért senki sem tud kapcsolatba lépni Önnel, hacsak nem oszt meg egy egyszeri vagy ideiglenes felhasználói címet, például QR-kódot vagy hivatkozást.

Még a felhasználói cím használata esetén is, aminek használata nem kötelező – ugyanakkor ez a kéretlen kapcsolatkérelmek küldésére is használható – módosíthatja vagy teljesen törölheti a címet anélkül, hogy elveszítené a meglévő kapcsolatait.

# Az adatok biztonságát és kezelését teljes egészében kézben tarthatja

A SimpleX Chat az összes felhasználói adatot kizárólag a klienseken tárolja egy **hordozható titkosított adatbázis-formátumban**, amely exportálható és átvihető bármely más támogatott eszközre.

A végpontok között titkosított üzenetek átmenetileg a SimpleX átjátszóin tárolódnak, amíg meg nem érkeznek a címzetthez, majd automatikusan véglegesen törlődnek onnan.

A föderált hálózatok kiszolgálóitól (e-mail, XMPP vagy Matrix) eltérően a SimpleX kiszolgálók nem tárolják a felhasználói fiókokat, csak továbbítják az üzeneteket, így védve mindkét fél magánéletét.

A küldött és a fogadott kiszolgálóforgalom között nincsenek közös azonosítók vagy titkosított szövegek — ha bárki megfigyeli, nem tudja könnyen megállapítani, hogy ki kivel kommunikál, még akkor sem, ha a TLS-t kompromittálják.

# Teljesen decentralizált — a SimpleX hálózat a felhasználóké

Használhatja **a SimpleXet a saját kiszolgálóival**, és továbbra is kommunikálhat azokkal, akik az előre beállított kiszolgálókat használják az alkalmazásban.

A SimpleX hálózat [nyitott protokollt](https://github.com/simplex-chat/simplexmq/blob/stable/protocol/overview-tjr.md) használ és [SDK-t biztosít a csevegési botok létrehozásához](https://github.com/simplex-chat/simplex-chat/tree/stable/packages/simplex-chat-client/typescript), lehetővé téve olyan szolgáltatások megvalósítását, amelyekkel a felhasználók a SimpleX Chat alkalmazásokon keresztül léphetnek kapcsolatba — mi már nagyon várjuk, hogy milyen SimpleX szolgáltatásokat készítenek a lelkes közreműködők.

Ha a SimpleX hálózatra való fejlesztést fontolgatja, például a SimpleX alkalmazások felhasználóinak szánt csevegési botot, vagy a SimpleX Chat könyvtárbotjának integrálását más mobilalkalmazásba, [lépjen velünk kapcsolatba](https://simplex.chat/contact#/?v=1&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2FK1rslx-m5bpXVIdMZg9NLUZ_8JBm8xTt%23MCowBQYDK2VuAyEALDeVe-sG8mRY22LsXlPgiwTNs9dbiLrNuA7f3ZMAJ2w%3D), ha bármilyen tanácsot vagy támogatást szeretne kapni.

## Funkciók

Végpontok között titkosított üzenetek markdown formázással és szerkesztéssel

Végpontok között titkosított
képek, videók és fájlok

Végpontok között titkosított, decentralizált csoportok — csak a felhasználók tudják, hogy ezek léteznek

Végpontok között titkosított hangüzenetek

Eltűnő üzenetek

Végpontok között titkosított
hang- és videóhívások

Hordozható, titkosított alkalmazás-adattárolás — profil átköltöztetése egy másik eszközre

Az inkognitómód —
egyedülálló a SimpleX Chatben

## Mitől lesz a SimpleX privát

### Ideiglenes, névtelen, páronkénti azonosítók

A SimpleX ideiglenes, névtelen, páros címeket és hitelesítő adatokat használ minden egyes felhasználói kapcsolathoz vagy csoporttaghoz.

Lehetővé teszi az üzenetek felhasználói profilazonosítók nélküli kézbesítését, ami az alternatíváknál jobb metaadat-védelmet biztosít.

### Sávon kívüli kulcscsere

Számos kommunikációs hálózat sebezhető a kiszolgálók vagy a hálózat-szolgáltatók MITM-támadásaival szemben.

Ennek megakadályozása érdekében a SimpleX alkalmazások egyszeri kulcsokat adnak át sávon kívül, amikor egy címet hivatkozásként vagy QR-kódként oszt meg.

### Kétrétegű végpontok közötti titkosítás

Dupla racsnis protokoll —
OTR-üzenetküldés, kompromittálás előtti és utáni titkosság-védelemmel.

NaCL cryptobox minden egyes várólistához, hogy megakadályozza a forgalom korrelációját az üzenetek várólistái között, ha a TLS veszélybe kerül.

### Üzenetintegritás ellenőrzés

Az integritás garantálása érdekében az üzenetek sorszámozással vannak ellátva, és tartalmazzák az előző üzenet kivonatát.

Ha bármilyen üzenetet hozzáadnak, eltávolítanak vagy módosítanak, a címzett értesítést kap róla.

### További rétege a kiszolgáló-titkosítás

Kiegészítő kiszolgálótitkosítási réteg a címzettnek történő kézbesítéshez, hogy megakadályozza a fogadott és az elküldött kiszolgálóforgalom közötti korrelációt, ha a TLS veszélybe kerül.

### Üzenetek keverése a korreláció csökkentése érdekében

A SimpleX kiszolgálók alacsony késleltetésű keverési csomópontokként működnek — a bejövő és kimenő üzenetek sorrendje eltérő.

### Biztonságos, hitelesített TLS-adatátvitel

A kliens és a kiszolgálók közötti kapcsolatokhoz csak az erős algoritmusokkal rendelkező TLS 1.2/1.3 protokollt használja.

A kiszolgáló ujjlenyomata és a csatornakötés megakadályozza a MITM- és a visszajátszási támadásokat.

Az újrakapcsolódás le van tiltva a munkamenet elleni támadások megelőzése érdekében.

### Hozzáférés a Tor hálózaton keresztül (nem kötelező)

Az IP-cím védelme érdekében a kiszolgálókat a Tor hálózaton vagy más átvitelátfedő hálózaton keresztül is elérheti.

A SimpleX, Tor hálózaton keresztüli használatához telepítse az [Orbot alkalmazást](https://guardianproject.info/apps/org.torproject.android/) és engedélyezze a SOCKS5 proxyt (vagy a VPN-t [az iOS-ban](https://apps.apple.com/us/app/orbot/id1609461599?platform=iphone)).

### Egyirányú várólista az üzenetekhez

Minden várólista az üzenetekhez egy irányba továbbítja az üzeneteket, a különböző küldési és fogadási címeken.

Kevesebb támadási felülettel rendelkezik, mint a hagyományos üzenetváltó alkalmazások, és kevesebb metaadatot tesz elérhetővé.

### Többrétegű tartalomkitöltés

A SimpleX minden titkosítási réteghez tartalomkitöltést használ az üzenetméretre irányuló támadások meghiúsítása érdekében.

A kiszolgálók és a hálózatot megfigyelők számára a különböző méretű üzenetek egyformának tűnnek.

## SimpleX hálózat

A SimpleX Chat a P2P- és a föderált hálózatok előnyeinek kombinálásával biztosítja a legjobb adatvédelmet.

### A P2P-hálózatokkal ellentétben

Minden üzenet a kiszolgálókon keresztül kerül elküldésre, ami jobb metaadat-védelmet és megbízható aszinkron üzenetkézbesítést biztosít, miközben elkerülhető a sok .

# Összehasonlítás más P2P-üzenetküldő protokollokkal

A [P2P](https://en.wikipedia.org/wiki/Peer-to-peer) üzenetküldő protokollok és alkalmazások számos problémával küzdenek, amelyek miatt kevésbé megbízhatóak, mint a SimpleX, bonyolultabb az elemzésük és többféle támadással szemben sebezhetőek.

1. A P2P-hálózatok az üzenetek továbbítására a [DHT](https://en.wikipedia.org/wiki/Distributed_hash_table) valamelyik változatát használják. A DHT kialakításakor egyensúlyt kell teremteni a kézbesítési garancia és a késleltetés között. A SimpleX jobb kézbesítési garanciával és alacsonyabb késleltetéssel rendelkezik, mint a P2P, mivel az üzenet redundánsan, a címzett által kiválasztott kiszolgálók segítségével több kiszolgálón keresztül párhuzamosan továbbítható. A P2P-hálózatokban az üzenet *O(log N)* csomóponton halad át szekvenciálisan, az algoritmus által kiválasztott csomópontok segítségével.
2. A SimpleX kialakítása a legtöbb P2P-hálózattól eltérően nem rendelkezik semmiféle globális felhasználói azonosítóval, még ideiglenessel sem, és csak az üzenetekhez használ ideiglenes, páros azonosítókat, ami jobb névtelenséget és metaadat-védelmet biztosít.
3. A P2P nem oldja meg a [MITM-támadás](https://en.wikipedia.org/wiki/Man-in-the-middle_attack) problémát, és a legtöbb létező implementáció nem használ sávon kívüli üzeneteket a kezdeti kulcscseréhez. A SimpleX a kezdeti kulcscseréhez sávon kívüli üzeneteket, vagy bizonyos esetekben már meglévő biztonságos és megbízható kapcsolatokat használ.
4. A P2P-megvalósításokat egyes internetszolgáltatók blokkolhatják (mint például a [BitTorrent](https://en.wikipedia.org/wiki/BitTorrent)). A SimpleX átvitel-független — a szabványos webes protokollokon, például WebSocketsen keresztül is működik.
5. Minden ismert P2P-hálózat sebezhető [Sybil támadással](https://en.wikipedia.org/wiki/Sybil_attack), mert minden egyes csomópont felderíthető, és a hálózat egészként működik. A támadások enyhítésére szolgáló ismert intézkedés lehet egy központi kiszolgáló (például: tracker), vagy egy drága [tanúsítvány](https://en.wikipedia.org/wiki/Proof_of_work). A SimpleX hálózat nem ismeri fel a kiszolgálókat, töredezett és több elszigetelt alhálózatként működik, ami lehetetlenné teszi az egész hálózatra kiterjedő támadásokat.
6. A P2P-hálózatok sebezhetőek lehetnek a [DRDoS-támadással](https://www.usenix.org/conference/woot15/workshop-program/presentation/p2p-file-sharing-hell-exploiting-bittorrent) szemben, amikor a kliensek képesek a forgalmat újraközvetíteni és felerősíteni, ami az egész hálózatra kiterjedő szolgáltatásmegtagadást eredményez. A SimpleX kliensek csak az ismert kapcsolatokból származó forgalmat továbbítják, és a támadó nem használhatja őket arra, hogy az egész hálózatban felerősítse a forgalmat.

---

### A föderált hálózatokkal ellentétben

A SimpleX átjátszó kiszolgálói NEM tárolnak felhasználói profilokat, kapcsolatokat és kézbesített üzeneteket, NEM kapcsolódnak egymáshoz, és NINCS kiszolgálójegyzék.

---

### SimpleX hálózat

a kiszolgálók egyirányú várólistákat biztosítanak a felhasználók összekapcsolásához, de nem látják a hálózati kapcsolati gráfot; azt csak a felhasználók látják.

## A SimpleX bemutatása

1. Felhasználói élmény

2. Hogyan működik

3. Mit látnak a kiszolgálók

1. Felhasználói élmény

Csoportokat hozhat létre, valamint kétirányú beszélgetéseket folytathat a partnereivel, ugyanúgy mint bármely más üzenetváltó alkalmazásban.

Hogyan működhet egyirányú várólistával és felhasználói profilazonosítók nélkül?

2. Hogyan működik

Minden kapcsolathoz két különböző üzenetküldési várólistát használ a különböző kiszolgálókon keresztül történő üzenetküldéshez és -fogadáshoz.

A kiszolgálók csak egyetlen irányba továbbítják az üzeneteket, anélkül, hogy teljes képet kapnának a felhasználók beszélgetéseiről vagy kapcsolatairól.

3. Mit látnak a kiszolgálók

A kiszolgálók minden egyes várólistához külön névtelen hitelesítő-adatokkal rendelkeznek, és nem tudják, hogy melyik felhasználóhoz tartoznak.

A felhasználók tovább fokozhatják a metaadatok védelmét, ha a Tor hálózat használatával férnek hozzá a kiszolgálókhoz, így megakadályozva az IP-cím szerinti korrelációt.

## Összehasonlítás más protokollokkal

|  |  | Signal, és a nagy platformok | XMPP, Matrix | P2P-protokollok |
| --- | --- | --- | --- | --- |
| Globális személyazonosságot igényel | Nem - privát | Igen [1] | Igen [2] | Igen [3] |
| A MITM lehetősége | Nem - biztonságos [4] | Igen [5] | Igen | Igen |
| Függés a DNS-től | Nem - ellenálló | Igen | Igen | Nem |
| Egyetlen vagy központosított hálózat | Nem - decentralizált | Igen | Nem - föderált [6] | Igen [7] |
| Központi komponens vagy más hálózati szintű támadás | Nem - ellenálló | Igen | Igen [2] | Igen [8] |

---

1. Általában telefonszám alapján, néhány esetben felhasználónév alapján
2. DNS-alapú címek
3. Nyilvános kulcs vagy más globális egyedi azonosító
4. A SimpleX átjátszói nem veszélyeztethetik a végpontok közötti titkosítást. Ellenőrizze a biztonsági kódot a sávon kívüli csatorna elleni támadások veszélyeinek csökkentésére
5. Ha az üzemeltetett kiszolgálók veszélybe kerülnek. Ellenőrizze a biztonsági kódot a Signal vagy más biztonságos üzenetküldő alkalmazás segítségével a támadások veszélyeinek csökkentésére
6. Nem védi a felhasználók metaadatait
7. Bár a P2P elosztott, de nem föderált — egyetlen hálózatként működnek
8. A P2P-hálózatoknak vagy van egy központi hitelesítője, vagy az egész hálózat kompromittálódhat — [tekintse meg itt](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/SIMPLEX.md#comparison-with-p2p-messaging-protocols)

## A végpontok közötti titkosítás összehasonlítása más üzenetváltó alkalmazásokkal

|  | Session | Briar | Element | Cwtch | Signal | SimpleX |
| --- | --- | --- | --- | --- | --- | --- |
| Tartalomkitöltés | ✗ | ✔︎[1] | ✗ | ✔︎ | ✔︎[1] | ✔︎ |
| Letagadhatóság | ✗ | ✗ | ✗ | ✔︎[2] | ✔︎[3] | ✔︎ |
| Kompromittálás előtti titkosságvédelem | ✗ | ✔︎ | ✔︎ | ✔︎ | ✔︎ | ✔︎ |
| Kompromittálás utáni titkosságvédelem | ✗ | ✗ | ✗ | ✔︎ | ✗[4] | ✔︎ |
| Kétlépcsős kulcscsere | ✔︎ | ✔︎[5] | ✔︎[5] | ✔︎ | ✔︎[5] | ✔︎ |
| Kvantumbiztos, hibrid kriptográfia | ✗ | ✗ | ✗ | ✗ | ✔︎[6] | ✔︎ |

---

1. A Briar 1024 bájtra, a Signal pedig 160 bájtra kerekítve tölti ki az üzenetek tartalmát
2. A letagadhatóság nem foglalja magába a kliens és a kiszolgáló közötti kapcsolatot.
3. Úgy tűnik, hogy a kriptográfiai aláírások használata rontja a letagadhatóságot, de ez tisztázásra szorul.
4. A többeszközös megvalósítás rontja a dupla racsni kompromittálás utáni biztonságát — [tekintse meg itt](https://eprint.iacr.org/2021/626.pdf).
5. A kétlépcsős kulcscsere nem követelmény a biztonsági kód ellenőrzéséhez.
6. A kvantumbiztos kulcscsere „ritka” — csak a racsnis lépések egy részét védi.
