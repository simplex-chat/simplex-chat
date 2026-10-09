# SimpleX fájlátvitel

Húzzon ide

vagy

Válasszon ki egy fájlt

Legfeljebb 100 MB – a [SimpleX Chat alkalmazás](https://simplex.chat/downloads) viszont 1 GB méretű fájlokat is támogat

Fájlok biztonságos küldése végpontok közötti titkosítással – felhasználói fiókok és nyomon követés nélkül.

## Szerezze be a SimpleX Chat alkalmazást – a legbiztonságosabb és legbizalmasabb üzenetváltó programot

Az imént használt fájlátvitel ugyanazt az útválasztó protokollt használja, mint a SimpleX Chat alkalmazás. Az alkalmazás végpontok közötti titkosított üzenetküldést, hang- és videohívásokat, csoportos csevegéseket és fájlok küldését teszi lehetővé. Nem szükséges hozzá felhasználói fiók, telefonszám, e-mail-cím és nem használ felhasználói profilazonosítókat sem.

# XFTP-protokoll: a legbiztonságosabb fájlátvitel

### Nincs szükség felhasználói fiókra

Minden fájldarab új, véletlenszerű kulcsot használ. Az útválasztóknak nincsenek „felhasználóik” vagy „fájljaik” – rögzített méretű titkosított fájldarabokat továbbítanak.

### Háromrétegű titkosítás a böngészőben

A fájl titkosítási kulcsa csak a webcím kivonattöredékében található – a böngésző soha nem küldi el azt a kiszolgálónak. A fájlok háromrétegű titkosítással rendelkeznek: TLS-átviteli, címzettenkénti (egyedi, ideiglenes kulcs átvitelenként) és végpontok közötti titkosítással.

### Független útválasztók

Amikor a fájl töredékekre oszlik, akkor a független felek által üzemeltetett hálózati útválasztókon keresztül kerül továbbításra. Egyetlen üzemeltető sem láthatja a fájl tényleges méretét és nevét. Még ha egy útválasztó biztonsága meg is sérül, csak a rögzített méretű titkosított töredékeket „láthatja”. A fájltöredékeket a hálózati útválasztók körülbelül 48 órán át tárolják a gyorsítótárban.

[Olvassa el az XFTP-protokoll leírását →](https://github.com/simplex-chat/simplexmq/blob/stable/protocol/xftp.md)
