---
title: Platfoma SimpleX
revision: 07.02.2023
---

| Updated 07.02.2023 | Języki: PL, [EN](/docs/SIMPLEX.md), [FR](/docs/lang/fr/SIMPLEX.md), [CZ](/docs/lang/cs/SIMPLEX.md) |
# Platfoma SimpleX - motywacja i porównanie

## Problemy

Istniejące komunikatory oraz protokoły borykają się ze wszystkimi lub kilkoma podanymi problemami:

- Brak zachowania prywatności profilu i kontaktów użytkownika (zachowanie poufności metadanych).
- Brak ochrony (lub jedynie opcjonalna ochrona) przed atakami MITM przez dostawcę usług przy użyciu szyfrowania [end to end](1)
- Niechciane wiadomości (spam i nadużycia).
- Brak własności danych i ich ochrony.
- Dla nietechnicznych użytkowników używanie niescentralizowanych protokołów jest skomplikowane.

Koncentracja komunikacji na niewielkiej liczbie scentralizowanych platform sprawia, że rozwiązanie tych problemów jest dość trudne.

## Proponowane rozwiązanie

Proponowany zestaw protokołów pozwala rozwiązać te problemy poprzez przechowywanie zarówno wiadomości, jak i kontaktów wyłącznie na urządzeniach klienckich, redukując rolę serwerów do zwykłych przekaźników wiadomości. Wymagają one jedynie autoryzacji wiadomości wysyłanych do kolejek, ale NIE wymagają uwierzytelniania użytkowników - dzięki temu chronione są nie tylko wiadomości, ale także metadane, ponieważ użytkownicy nie mają przypisanych do siebie żadnych identyfikatorów - w przeciwieństwie do innych platform.

Zobacz [whitepaper](https://github.com/simplex-chat/simplexmq/blob/master/protocol/overview-tjr.md) po więcej informacji o zadaniach platformy oraz by dowiedzieć się jak wygląda koncepcja techniczna modelu.

## Dlaczego SimpleX

## SimpleX podchodzi do problemu prywatności i bezpieczeństwa w unikalny sposób

Każdy powinien zwracać uwagę na prywatność i bezpieczeństwo swojej komunikacji - nawet zwykłe rozmowy mogą narazić Cię na niebezpieczeństwo.

### Pełna prywatność Twojej tożsamości, profilu, kontaktu i metadanych

**W przeciwieństwie do innych komunikatorów, SimpleX nie posiada żadnych identyfikatorów przypisanych do użytkowników** - nie wymaga użycia numeru telefonu (jak Signal czy Whatsapp), adresu opartego o domenę (jak email, XMPP czy Matrix), nazw użytkownika (jak Telegram), kluczy publicznych czy nawet losowych numerów (jak pozostałe komunikatory) do identyfikowania użytkowników - nie wiemy nawet ile osób używa SimpleX. 

Do dostarczania wiadomości zamiast identyfikatorów użytkowników, których używają wszystkie inne platformy, SimpleX wykorzystuje adresy jednokierunkowych (simpleksowych) kolejek wiadomości. Korzystanie z SimpleX jest jak posiadanie innego adresu e-mail lub numeru telefonu dla każdego kontaktu, ale bez kłopotów z zarządzaniem tymi wszystkimi adresami. W niedalekiej przyszłości aplikacje SimpleX będą również automatycznie zmieniać kolejki wiadomości, przenosząc konwersacje z jednego serwera na drugi, aby zapewnić użytkownikom jeszcze lepszą prywatność.

Takie podejście chroni prywatność tego, z kim się komunikujesz, ukrywając to przed serwerami platformy SimpleX i wszelkimi obserwatorami. Prywatność komunikacji można dodatkowo zwiększyć, konfigurując dostęp do sieci w taki sposób, by łączyć się z serwerami SimpleX za pośrednictwem sieci transportowej typu overlay, np. sieci Tor.

### Najlepsza ochrona przed spamem i nadużyciami

Ponieważ nie masz żadnego identyfikatora na platformie SimpleX, nie można się z Tobą skontaktować, chyba że udostępnisz jednorazowy link z zaproszeniem lub opcjonalny tymczasowy adres użytkownika. Nawet przy użyciu opcjonalnych adresów użytkownika, które mogą być wykorzystywane do wysyłania spamu z prośbami o kontakt, można je zmienić lub całkowicie usunąć bez utraty jakichkolwiek połączeń (kontaktów).

### Pełna kontrola i bezpieczeństwo Twoich danych

SimpleX przechowuje wszystkie dane użytkownika na urządzeniach klienckich, wiadomości są przetrzymywane tylko tymczasowo na serwerach przekaźnikowych SimpleX do momentu ich odebrania, po czym są trwale usuwane.

Używamy przenośnego formatu bazy danych, który może być używany na wszystkich obsługiwanych urządzeniach - wkrótce dodamy możliwość eksportu bazy danych czatu z aplikacji mobilnej, aby można było jej używać na innym urządzeniu.

W przeciwieństwie do serwerów sieci federowanych (e-mail, XMPP lub Matrix), serwery SimpleX nie przechowują kont użytkowników, a jedynie przekazują wiadomości do odbiorców, chroniąc prywatność obu stron. Nie ma żadnych identyfikatorów ani zaszyfrowanych wiadomości występujących wspólnie z wysłanym i odbieranym ruchem serwera, dzięki dodatkowej warstwie szyfrowania dostarczanych wiadomości. Jeśli więc ktoś obserwuje ruch na serwerze, nie może łatwo określić, kto komunikuje się z kim (sprawdź [SimpleX whitepaper](https://github.com/simplex-chat/simplexmq/blob/master/protocol/overview-tjr.md) by dowiedzieć się o znanych atakach korelacji ruchu).

### Użytkownicy są właścicielami sieci SimpleX

Możesz używać SimpleX na własnych serwerach i nadal komunikować się z ludźmi za pomocą serwerów, które są wstępnie skonfigurowane w aplikacjach lub z dowolnymi innymi serwerami SimpleX.

Platforma SimpleX korzysta z otwartego protokołu i zapewnia zestaw SDK do tworzenia czatbotów, umożliwiając implementację usług, z którymi użytkownicy mogą wchodzić w interakcje za pośrednictwem aplikacji SimpleX Chat - naprawdę nie możemy się doczekać, aby zobaczyć, jakie usługi oparte o SimpleX można stworzyć.

Jeśli rozważasz stworzenie czegoś w oparciu o platformę SimpleX, niezależnie od tego, czy chodzi o usługi czatbotów dla użytkowników aplikacji SimpleX, czy też integrację biblioteki SimpleX Chat z aplikacjami mobilnymi, skontaktuj się z nami, aby uzyskać porady i wsparcie.

## Porównanie z innymi protokołami

|                                                             |       SimpleX Chat       | Signal, duże platformy |   XMPP, Matrix  |  Protokoły P2P  |
| :---------------------------------------------------------- | :----------------------: | :--------------------: | :-------------: | :-------------: |
| Wymaga identyfikatorów użytkownika                          |      Nie = prywatny      |     Tak<sup>1</sup>    | Tak<sup>2</sup> | Tak<sup>3</sup> |
| Możliwość ataku MITM                                        |     Nie = bezpieczny     |     Tak<sup>4</sup>    |       Tak       |       Tak       |
| Polega na DNS                                               | Nie = odporny na cenzurę |           Tak          |       Tak       |       Nie       |
| Pojedynczy operator lub sieć                                |  Nie = zdecentralizowany |           Tak          |       Nie       | Tak<sup>5</sup> |
| Scentralizowanie lub możliwość ataku obejmującego całą sieć | Nie = odporny na cenzurę |           Tak          | Tak<sup>2</sup> | Tak<sup>6</sup> |

1. Zwykle opiera się na numerze telefonu, w niektórych przypadkach na nazwie użytkownika.
2. Bazuje na DNS.
3. Klucz publiczny lub inny globalnie unikalny identyfikator.
4. Jeśli serwery operatora zostaną przejęte.
5. Mimo że sieci P2P i sieci oparte na kryptowalutach są rozproszone, nie są w pełni zdecentralizowane - działają jako pojedyncza sieć, z pojedynczą przestrzenią nazw adresów użytkowników.
6. Sieci P2P albo mają jakiś centralny serwer, albo cała sieć może zostać przejęta - patrz następna sekcja.

## Porównanie z komunikatorami [P2P][9]

Istnieje kilka protokołów czatu/wiadomości P2P i implementacji, które mają na celu rozwiązanie problemu prywatności i centralizacji, ale mają one swój własny szereg problemów, które sprawiają, że są mniej niezawodne niż proponowany projekt, są bardziej skomplikowane w implementacji i analizie oraz są bardziej podatne na ataki.

1. Sieci [P2P][9] korzystają z jakiegoś rodzaju [DHT][10] do routowania wiadomości/zapytań po sieci. Implementacje DHT mają złożone konstrukcje, muszą równoważyć niezawodność, gwarancję dostawy i czas oczekiwania. Proponowany model zapewnia zarówno większą gwarancję dostarczalności, jak i mniejsze opóźnienia (wiadomość jest przekazywana wiele razy równolegle, za każdym razem przez jeden węzeł, przy użyciu serwerów wybranych przez odbiorcę, podczas gdy w sieciach P2P wiadomość jest przekazywana przez `O(log N)` węzłów sekwencyjnie, przy użyciu węzłów wybranych przez algorytm).

2. Proponowany model, w przeciwieństwie do większości sieci P2P, nie posiada żadnych globalnych identyfikatorów użytkowników, nawet tymczasowych.

3. P2P samo w sobie nie rozwiązuje problemu [ataku MITM][2], a większość istniejących rozwiązań nie wykorzystuje komunikacji out-of-band do początkowej wymiany kluczy. Proponowany projekt wykorzystuje wiadomości out-of-band lub (w niektórych przypadkach) istniejące wcześniej bezpieczne i zaufane połączenia do początkowej wymiany kluczy.

4. Implementacje P2P mogą być blokowane przez niektórych dostawców Internetu (tak jak [BitTorrent][11]). Proponowany model jest niezależny od rodzaju transmisji - może działać na standardowych protokołach sieciowych, a serwery mogą działać na tych samych domenach, co strony internetowe.

5. Wszystkie znane sieci P2P mogą być podatne na [atak typu Sybil][12], ponieważ każdy węzeł jest wykrywalny, a sieć działa jako całość. Znane środki mające na celu zmniejszenie prawdopodobieństwa ataku typu Sybil wymagają zastosowania scentralizowanego komponentu lub kosztownego [proof of work][13]. Proponowany model, przeciwnie, nie ma możliwości wykrycia serwera - serwery nie są połączone, nie są znane sobie nawzajem i wszystkim klientom. Sieć SimpleX jest pofragmentowana i działa jako wiele odizolowanych połączeń. Uniemożliwia to ataki na całą sieć SimpleX - nawet jeśli niektóre serwery są zagrożone, inne części sieci mogą działać normalnie, a dotknięci atakiem użytkownicy mogą przełączyć się na inne serwery bez utraty kontaktów lub wiadomości.

6. Sieci P2P są prawdopodobnie [podatne][14] na [atak DRDoS][15]. W proponowanym modelu klienci przekazują tylko ruch ze znanych zaufanych połączeń i nie mogą być wykorzystywani do odbijania i wzmacniania ruchu w całej sieci.

[1]: https://pl.wikipedia.org/wiki/Szyfrowanie_od_ko%C5%84ca_do_ko%C5%84ca
[2]: https://pl.wikipedia.org/wiki/Atak_man_in_the_middle
[9]: https://pl.wikipedia.org/wiki/Peer-to-peer
[10]: https://pl.wikipedia.org/wiki/Rozproszona_tablica_mieszaj%C4%85ca
[11]: https://pl.wikipedia.org/wiki/BitTorrent
[12]: https://en.wikipedia.org/wiki/Sybil_attack
[13]: https://pl.wikipedia.org/wiki/Proof_of_Work
[14]: https://www.usenix.org/conference/woot15/workshop-program/presentation/p2p-file-sharing-hell-exploiting-bittorrent
[15]: https://pl.wikipedia.org/wiki/DRDoS
