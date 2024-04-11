---
layout: layouts/article.html
title: "SimpleX announces SimpleX Chat v3 &mdash; with encrypted calls and iOS push notifications"
date: 2022-07-11
image: images/20220711-call.png
previewBody: blog_previews/20220711.html
permalink: "/blog/20220711-simplex-chat-v3-released-ios-notifications-audio-video-calls-database-export-import-protocol-improvements.html"
---

# SimpleX zapowiada SimpleX Chat v3 - z szyfrowanymi rozmowami i powiadomieniami push na iOS

**Opublikowano:** 11 lipca, 2022

## Nowości w wersji 3

- [natychmiastowe powiadomienia dla iOS](#natychmiastowe-powiadomienia-dla-ios)
- [szyfrowane połączenia audio/video end-to-end](#szyfrowane-połączenia-audiovideo-end-to-end)
- [eksport i import bazy danych](#eksport-i-import-bazy-danych)
- [prywatność protokołu i poprawa wydajności](#prywatność-protokołu-i-poprawa-wydajności)

### Natychmiastowe powiadomienia dla iOS

Pisałem wcześniej o [naszym projekcie powiadomień dla iOS](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/blog/20220404-simplex-chat-instant-notifications.md#problem--użytkownicy-oczekują-to-być-instantly-notified-when-messages-arrive) - jest to już wydane. Aplikacja po aktualizacji zaproponuje migrację bazy danych, a następnie należy wybrać tryb powiadomień - natychmiastowe lub okresowe powiadomienia push, lub wcześniej dostępne okresowe odświeżanie w tle, które nie wykorzystuje powiadomień push.

Do dostarczania powiadomień na urządzenia z iOS wykorzystujemy nasz serwer powiadomień, ponieważ istnieje jeden klucz prywatny, który Apple wydaje dla aplikacji. Serwer ten posiada minimalną ilość informacji o aktywności użytkownika na czacie:

- nie posiada adresów kolejek wiadomości używanych do wysyłania i odbierania wiadomości - istnieje dodatkowy adres wykorzystywany przez serwer powiadomień do odbierania powiadomień z serwerów komunikatorów.
- same powiadomienia nie zawierają treści wiadomości ani kontaktów, nawet w formie zaszyfrowanej - zawierają jedynie zaszyfrowane metodą end-to-end metadane o serwerze i kolejce, w której dostępne są wiadomości - serwery Apple nie mają więc dostępu do informacji o tym, ile masz kontaktów ani jak często każdy z nich wysyła Ci wiadomości - mogą jedynie zobaczyć całkowitą liczbę powiadomień, które otrzymuje Twoje urządzenie.
- nadawcy wiadomości nie łączą się z serwerem powiadomień, więc nie może on w żaden sposób skorelować ruchu wysłanego i odebranego na wielu urządzeniach.

<img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20220711-notifications.png" width="480">

**Zwróć uwagę**: aby okresowe powiadomienia działały, aplikacja musi być w tle - jeśli aplikacja jest całkowicie zatrzymana (usunięta z ostatnich aplikacji), nie będzie sprawdzać nowych wiadomości i nie otrzymasz powiadomień. Natychmiastowe powiadomienia działają nawet wtedy, gdy aplikacja jest całkowicie zatrzymana.

### Szyfrowane połączenia audio/video end-to-end

Możesz teraz dzwonić do swoich kontaktów przez WebRTC, łącząc się przez serwery przekaźnikowe SimpleX Chat lub peer-to-peer, a w najbliższej przyszłości będziesz mógł skonfigurować własne serwery STUN/TURN używane do nawiązania połączenia. Połączenia są szyfrowane end-to-end - klucz jest negocjowany poprzez połączenie, które już masz z kontaktem na czacie, które jest również wykorzystywane jako warstwa sygnalizacyjna dla WebRTC - w większości przypadków tylko trzy wiadomości w sumie muszą być wysłane przez klienta Twojego i Twojego kontaktu, aby połączenie się rozpoczęło, w tym wstępne zaproszenie do połączenia.

<img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20220711-call-in-progress.png" width="480">

Połączenia są nadal dość ograniczone, zwłaszcza na iOS, - na przykład nie można kontynuować rozmowy, gdy aplikacja jest w tle.

### Eksport i import bazy danych

Wielu użytkowników pytało - _jak mogę przenieść mój profil czatu na nowe urządzenie_? SimpleX Chat v3 ma na to rozwiązanie - możesz teraz eksportować bazę danych czatu z jednego urządzenia i importować ją do innego - nawet na inną platformę, np. możesz przenieść bazę danych czatu z telefonu z systemem Android do iOS lub do klienta terminalowego (konsoli).

<img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20220711-database-chat-stopped.png" width="480">

Kilka ważnych ograniczeń:

- nie można uruchomić tego samego profilu czatu z dwóch urządzeń, ani w tym samym czasie, ani na zmianę. należy używać tylko najnowszej wersji bazy danych - za każdym razem, gdy chcesz przenieść ją na inne urządzenie, musisz wyeksportować nowe archiwum czatu z urządzenia, które ostatnio go używało, i zaimportować je na urządzenie, na którym chcesz go używać w następnej kolejności.
- Planujemy wkrótce dodać wewnętrzne szyfrowanie bazy danych, ale obecnie jest ona przechowywana na urządzeniu bez szyfrowania, a eksportowane archiwum również nie jest szyfrowane. Powinieneś go bezpiecznie przechowywać i szyfrować, jeśli przechowujesz go w chmurze lub wysyłasz przez e-mail.

Ta funkcja może być przydatna w niektórych innych scenariuszach:

- zarządzanie wieloma profilami czatu - nie jest to zbyt wygodne, a lepsza obsługa wielu profili pojawi się wkrótce.
- współdzielenie dostępu do tego samego profilu z innymi osobami, o ile nie jest on używany w tym samym czasie. Dla tego przypadku użycia może być lepiej uruchomić klienta terminalowego SimpleX Chat w chmurze, tak jak robimy to z naszym publicznym kontem, z którym można się połączyć za pośrednictwem aplikacji.
- zachowaj profil czatu, gdy musisz tymczasowo usunąć aplikację z urządzenia.

### Prywatność protokołu i poprawa wydajności

Dodanie powiadomień push dla iOS wymagało zmian w SimpleX Messaging Protocol. Udało nam się nie tylko utrzymać ten sam poziom prywatności metadanych przed pasywnymi obserwatorami, ale także go poprawić - teraz wszystkie metadane wiadomości, które są przekazywane z serwera do odbiorcy, są zawarte w tej samej zaszyfrowanej kopercie, co sama wiadomość - tak jak wcześniej, nie ma żadnych identyfikatorów ani szyfrogramów wspólnych wewnątrz ruchu TLS między odebranym i wysłanym ruchem serwera, a teraz nie ma także znacznika czasu wiadomości wewnątrz TLS.

Ulepszyliśmy również przepływ protokołu do ustanawiania dwukierunkowego połączenia pomiędzy dwoma użytkownikami - jest on teraz znacznie szybszy, zużywając znacznie mniej ruchu sieciowego i baterii. Poprawia to czas potrzebny na połączenie z kontaktami i rozpoczęcie dostarczania obrazów i plików.

Wszystkie te zmiany nie wpłynęły na kompatybilność wsteczną - jeśli Twój kontakt ma poprzednią wersję klienta, lub łączysz się z poprzednią wersją serwera, poprzednia wersja protokołu będzie używana - SimpleX ma niezależną negocjację wersji w 4 warstwach protokołu [od v1](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/blog/20220112-simplex-chat-v1-released.md#stable-protocol-implementation), co pozwala nam na ewolucję protokołów bez zakłóceń dla użytkowników.

## Platforma SimpleX

Budujemy nową platformę dla rozproszonych aplikacji internetowych, gdzie prywatność wiadomości _i_ sieci ma znaczenie. [SimpleX Chat](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/README.md) to nasza pierwsza aplikacja, aplikacja do przesyłania wiadomości zbudowana na platformie SimpleX.

### Pierwsza (i naszym zdaniem jedyna) platforma do przesyłania wiadomości bez jakichkolwiek identyfikatorów użytkowników - w 100% prywatna z założenia!

Aby chronić tożsamość użytkowników i ich połączeń, SimpleX Chat nie ma identyfikatorów użytkowników widocznych dla serwerów i sieci - w przeciwieństwie do każdej innej platformy do przesyłania wiadomości. Nie tylko SimpleX nie używa numerów telefonów ani e-maili, jak Signal i wiele innych platform, ale także nie ma żadnych trwałych identyfikatorów do identyfikacji użytkowników - w przeciwieństwie do wielu innych komunikatorów uważanych za prywatne - Session, Cwtch, Ricochet, Briar, Jami, itp, - wszystkie te platformy mają globalne identyfikatory użytkowników, jednoznacznie identyfikując swoich użytkowników i tworząc ryzyko de-anonimizacji użytkowników.

### Dlaczego posiadanie identyfikatorów użytkowników jest złe dla użytkowników?

Kiedy każdy użytkownik ma unikalny identyfikator na platformie, nawet jeśli jest to tylko losowa liczba, np. jako ID sesji, stwarza to ryzyko, że ktokolwiek uzyska dostęp do danych platformy, może obserwować, jak użytkownicy są połączeni i ile wiadomości jest przesyłanych między nimi, a następnie skorelować te informacje z istniejącymi publicznymi sieciami społecznymi, określając prawdziwe tożsamości niektórych użytkowników. Nawet w przypadku najbardziej prywatnych komunikatorów zbudowanych na szczycie sieci Tor, posiadanie trwałej tożsamości oznacza, że jeśli rozmawiasz z dwoma różnymi użytkownikami za pośrednictwem tego samego profilu, mogą oni udowodnić, że komunikują się z tą samą osobą, ponieważ użyliby tego samego adresu do wysyłania wiadomości.

Platforma SimpleX unika tego ryzyka, ponieważ nie posiada w swojej konstrukcji żadnej tożsamości użytkownika - więc nawet jeśli rozmawiasz z dwoma różnymi osobami z tego samego profilu czatu, nie będą one w stanie udowodnić, że rozmawiają z tą samą osobą - jedynie to, że profile użytkowników wyglądają tak samo. I planujemy dodać funkcję pozwalającą na posiadanie innej nazwy wyświetlanej dla każdego kontaktu, z którym się łączysz - sporo użytkowników o to prosiło.

### Jak to działa

Wiele osób pytało: _jeśli SimpleX nie ma identyfikatorów użytkowników, jak może dostarczać wiadomości?

Napisałem o tym w [ogłoszenie wydania v2](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/blog/20220511-simplex-chat-v2-images-files.md), a więcej informacji o celach platformy SimpleX i projekcie technicznym można uzyskać w [the whitepaper](https://github.com/simplex-chat/simplexmq/blob/master/protocol/overview-tjr.md).

## Prosimy o pomoc w opłaceniu audytu bezpieczeństwa przeprowadzanego przez trzecią stronę

Przejdę od razu do rzeczy: proszę o wsparcie SimpleX Chat poprzez darowizny.

Priorytetem jest dla nas prywatność i bezpieczeństwo użytkowników - nie byłoby to możliwe bez Waszego wsparcia, które mieliśmy do tej pory.

Planujemy przeprowadzenie audytu bezpieczeństwa aplikacji przez stronę trzecią i byłoby to dla nas bardzo pomocne, gdyby część z tych ponad 20000$ wydatków mogła być pokryta z darowizn.

Naszym zobowiązaniem wobec użytkowników jest to, że protokoły SimpleX są i pozostaną otwarte, i w domenie publicznej, - więc każdy może budować przyszłe implementacje dla klientów i serwerów. Budujemy platformę SimpleX w oparciu o te same zasady, co w przypadku poczty elektronicznej i sieci, ale znacznie bardziej prywatną i bezpieczną.

Jeśli już korzystasz z SimpleX Chat lub planujesz korzystać z niego w przyszłości, gdy będzie miał więcej funkcji, rozważ przekazanie darowizny - pomoże nam to zebrać więcej funduszy. Przekazanie dowolnej kwoty, nawet ceny filiżanki kawy, zrobiłoby dla nas ogromną różnicę.

Możliwe jest [przekazanie darowizny przez GitHub](https://github.com/sponsors/simplex-chat), nie pobiera prowizji od nas, lub [przez OpenCollective](https://opencollective.com/simplex-chat), który również przyjmuje darowizny w kryptowalutach, ale pobiera prowizję.

Dziękuję,

Evgeny

Założyciel SimpleX Chat
