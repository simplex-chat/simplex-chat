| Zaktualizowano 07.02.2023 | Języki: PL, [EN](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/SIMPLEX.md), [FR](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/fr/SIMPLEX.md), [CZ](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/cz/SIMPLEX.md) |
# Platforma SimpleX - motywacja i porównanie

## Problemy

Istniejące platformy i protokoły czatowe mają niektóre lub wszystkie z następujących problemów:

- Brak prywatności profilu użytkownika i kontaktów (prywatność metadanych).
- Brak ochrony (lub tylko opcjonalna ochrona) implementacji [E2EE][1] przed atakami MITM przez dostawcę.
- Niezamówione wiadomości (spam i nadużycia).
- Brak własności i ochrony danych.
- Złożoność użytkowania wszystkich niescentralizowanych protokołów dla nietechnicznych użytkowników.

Koncentracja komunikacji w małej liczbie scentralizowanych platform sprawia, że rozwiązanie tych problemów jest dość trudne.

## Proponowane rozwiązanie

Proponowany stos protokołów rozwiązuje te problemy sprawiając, że zarówno wiadomości jak i kontakty przechowywane są tylko na urządzeniach klienckich, redukując rolę serwerów do prostych przekaźników wiadomości, które wymagają jedynie autoryzacji wiadomości wysyłanych do kolejek, ale NIE wymagają uwierzytelniania użytkowników - chronione są nie tylko wiadomości, ale także metadane, ponieważ użytkownicy nie mają przypisanych żadnych identyfikatorów - inaczej niż w przypadku jakichkolwiek innych platform.

Więcej informacji na temat celów platformy i projektu technicznego można znaleźć w [SimpleX whitepaper](https://github.com/simplex-chat/simplexmq/blob/master/protocol/overview-tjr.md).

## Dlaczego warto korzystać z SimpleX

## Unikalne podejście SimpleX do prywatności i bezpieczeństwa

Każdy powinien dbać o prywatność i bezpieczeństwo swojej komunikacji - nawet zwykłe rozmowy mogą narazić Cię na niebezpieczeństwo.

### Pełna prywatność Twojej tożsamości, profilu, kontaktów i metadanych

**W przeciwieństwie do każdej innej istniejącej platformy do przesyłania wiadomości, SimpleX nie ma identyfikatorów przypisanych do użytkowników** - nie używa numerów telefonów (jak Signal czy WhatsApp), adresów opartych na domenie (jak e-mail, XMPP czy Matrix), nazw użytkowników (jak Telegram), kluczy publicznych czy nawet liczb losowych (jak wszystkie inne komunikatory) do identyfikacji swoich użytkowników - nie wiemy nawet, ile osób korzysta z SimpleX.

Do dostarczania wiadomości zamiast identyfikatorów użytkowników, których używają wszystkie inne platformy, SimpleX wykorzystuje adresy jednokierunkowych (simplex) kolejek wiadomości. Korzystanie z SimpleX jest jak posiadanie innego adresu e-mail lub numeru telefonu dla każdego kontaktu, ale bez kłopotliwego zarządzania wszystkimi tymi adresami. W najbliższej przyszłości aplikacje SimpleX będą również automatycznie zmieniać kolejki wiadomości, przenosząc rozmowy z jednego serwera na drugi, aby zapewnić użytkownikom jeszcze lepszą prywatność.

Takie podejście chroni prywatność tego, z kim się komunikujesz, ukrywając ją przed serwerami platformy SimpleX i przed wszelkimi obserwatorami. Możesz dodatkowo zwiększyć swoją prywatność, konfigurując dostęp do sieci, aby łączyć się z serwerami SimpleX za pośrednictwem jakiejś nakładkowej sieci transportowej, np. Tor.

### Najlepsza ochrona przed spamem i nadużyciami

Ponieważ nie masz żadnego identyfikatora na platformie SimpleX, nie można się z Tobą skontaktować, chyba że udostępnisz jednorazowy link zapraszający lub opcjonalny tymczasowy adres użytkownika. Nawet w przypadku opcjonalnych adresów użytkowników, choć mogą być one wykorzystywane do wysyłania spamowych próśb o kontakt, można je zmienić lub całkowicie usunąć bez utraty jakichkolwiek połączeń.

### Pełna własność, kontrola i bezpieczeństwo Twoich danych

SimpleX przechowuje wszystkie dane użytkownika na urządzeniach klienckich, wiadomości są tylko tymczasowo przechowywane na serwerach przekaźnikowych SimpleX do momentu ich odebrania.

Używamy przenośnego formatu bazy danych, który może być używany na wszystkich obsługiwanych urządzeniach - wkrótce dodamy możliwość eksportu bazy danych czatu z aplikacji mobilnej, aby można było z niej korzystać na innym urządzeniu.

W przeciwieństwie do serwerów sieci federacyjnych (e-mail, XMPP czy Matrix), serwery SimpleX nie przechowują kont użytkowników, po prostu przekazują wiadomości do odbiorców, chroniąc prywatność obu stron. Nie ma żadnych identyfikatorów ani zaszyfrowanych wiadomości wspólnych dla ruchu wysyłanego i odbieranego przez serwer, dzięki dodatkowej warstwie szyfrowania dostarczanych wiadomości. Jeśli więc ktokolwiek obserwuje ruch na serwerze, nie może łatwo określić, kto z kim się komunikuje (patrz [SimpleX whitepaper](https://github.com/simplex-chat/simplexmq/blob/master/protocol/overview-tjr.md), aby poznać znane ataki korelacji ruchu).

### Własna sieć SimpleX

Możesz używać SimpleX z własnymi serwerami i nadal komunikować się z osobami korzystającymi z serwerów wstępnie skonfigurowanych w aplikacjach lub z dowolnymi innymi serwerami SimpleX.

Platforma SimpleX wykorzystuje otwarty protokół i zapewnia SDK do tworzenia czatbotów, umożliwiając implementację usług, z którymi użytkownicy mogą wchodzić w interakcje za pośrednictwem aplikacji SimpleX Chat - naprawdę nie możemy się doczekać, aby zobaczyć, jakie usługi SimpleX można zbudować.

Jeśli rozważasz rozwój z platformą SimpleX, czy to dla usług czatbot dla użytkowników aplikacji SimpleX, czy też w celu integracji biblioteki SimpleX Chat w swoich aplikacjach mobilnych, skontaktuj się z nami w celu uzyskania wszelkich porad i wsparcia.

## Porównanie z innymi protokołami

|                                                |    SimpleX chat    | Sygnał, duże platformy |  XMPP, Matrix   |  Protokoły P2P  |
| :--------------------------------------------- | :----------------: | :-------------------: | :-------------: | :-------------: |
| Wymaga identyfikatorów użytkowników                      |    Nie = prywatny    |    Tak<sup>1</sup>    | Tak<sup>2</sup> | Tak<sup>3</sup> |
| Możliwość przeprowadzenia ataku MITM                            |    Nie = bezpieczny     |    Tak<sup>4</sup>    |       Tak       |       Tak       |
| Zależność od DNS                              |   Nie = odporny   |          Tak          |       Tak       |       Nie        |
| Jeden operator lub sieć                     | Nie = zdecentralizowany |          Tak          |       Nie        | Tak<sup>5</sup> |
| Centralny komponent lub inny atak w całej sieci |   Nie = odporny   |          Tak          | Tak<sup>2</sup> | Tak<sup>6</sup> |

1. Zazwyczaj opierając się o numer telefonu, w niektórych przypadkach na podstawie nazwy użytkownika.
2. Oparte na DNS.
3. Klucz publiczny lub jakiś inny globalnie unikalny identyfikator.
4. W przypadku kompromitacji serwerów operatora.
5. Sieci P2P i sieci oparte na kryptowalutach są wprawdzie rozproszone, ale nie są zdecentralizowane - działają jako jedna sieć, z jedną przestrzenią nazw adresów użytkowników.
6. Sieci P2P albo mają centralny organ, albo cała sieć może zostać skompromitowana - patrz następna sekcja.

## Porównanie z protokołami komunikacyjnymi [P2P][9]

Istnieje kilka protokołów i implementacji protokołów czatu/wiadomości P2P, które mają na celu rozwiązanie problemu prywatności i centralizacji, ale mają one swój własny zestaw problemów, który sprawia, że są mniej niezawodne niż proponowany projekt, bardziej skomplikowane w implementacji i analizie oraz bardziej podatne na ataki.

1. Sieci [P2P][9] używają jakiegoś wariantu [DHT][10] do kierowania wiadomości/żądań przez sieć. Implementacje DHT mają złożone projekty, które muszą równoważyć niezawodność, gwarancję dostarczenia i opóźnienie. Proponowany projekt ma zarówno lepszą gwarancję dostarczenia, jak i mniejsze opóźnienie (wiadomość jest przekazywana wielokrotnie równolegle, za każdym razem przez jeden węzeł, z wykorzystaniem serwerów wybranych przez odbiorcę, podczas gdy w sieciach P2P wiadomość jest przekazywana przez `O(log N)` węzłów sekwencyjnie, z wykorzystaniem węzłów wybranych przez algorytm).

2. Proponowany projekt, w przeciwieństwie do większości sieci P2P, nie posiada żadnych globalnych identyfikatorów użytkowników, nawet tymczasowych.

3. Sama sieć P2P nie rozwiązuje problemu [ataku MITM][2], a większość istniejących rozwiązań nie wykorzystuje wiadomości out-of-band do początkowej wymiany klucza. Proponowany projekt wykorzystuje wiadomości out-of-band lub, w niektórych przypadkach, istniejące wcześniej bezpieczne i zaufane połączenia do początkowej wymiany kluczy.

4. Implementacje P2P mogą być blokowane przez niektórych dostawców Internetu (jak [BitTorrent][11]). Proponowany projekt jest niezależny od transportu - może działać przez standardowe protokoły internetowe, a serwery mogą być rozmieszczone w tych samych domenach co strony internetowe.

5. Wszystkie znane sieci P2P są prawdopodobnie podatne na [atak Sybil][12], ponieważ każdy węzeł jest możliwy do odkrycia, a sieć działa jako całość. Znane środki zmniejszające prawdopodobieństwo ataku Sybil albo wymagają scentralizowanego komponentu, albo kosztownych [proof of work][13]. W proponowanym projekcie, przeciwnie, nie ma odkrywalności serwerów - serwery nie są połączone, nie są znane sobie nawzajem i wszystkim klientom. Sieć SimpleX jest fragmentaryczna i działa jako wiele izolowanych połączeń. Sprawia to, że ataki na sieć SimpleX w skali całej sieci są niemożliwe - nawet jeśli niektóre serwery zostaną skompromitowane, inne części sieci mogą działać normalnie, a dotknięci nimi klienci mogą przełączyć się na korzystanie z innych serwerów bez utraty kontaktów czy wiadomości.

6. Sieci P2P są prawdopodobnie [podatne][14] na [atak DRDoS][15]. W proponowanym projekcie klienci przekazują jedynie ruch ze znanego zaufanego połączenia i nie mogą być wykorzystywani do odbijania i wzmacniania ruchu w całej sieci.

[1]: https://en.wikipedia.org/wiki/End-to-end_encryption
[2]: https://en.wikipedia.org/wiki/Man-in-the-middle_attack
[9]: https://en.wikipedia.org/wiki/Peer-to-peer
[10]: https://en.wikipedia.org/wiki/Distributed_hash_table
[11]: https://en.wikipedia.org/wiki/BitTorrent
[12]: https://en.wikipedia.org/wiki/Sybil_attack
[13]: https://en.wikipedia.org/wiki/Proof_of_work
[14]: https://www.usenix.org/conference/woot15/workshop-program/presentation/p2p-file-sharing-hell-exploiting-bittorrent
[15]: https://en.wikipedia.org/wiki/Denial-of-service_attack#Reflected_attack
