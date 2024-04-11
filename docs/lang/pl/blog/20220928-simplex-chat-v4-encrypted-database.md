---
layout: layouts/article.html
title: "SimpleX Chat v4.0 with encrypted database is released"
date: 2022-09-28
image: images/20220928-passphrase.png
imageBottom: true
previewBody: blog_previews/20220928.html
permalink: "/blog/20220928-simplex-chat-v4-encrypted-database.html"
---

# SimpleX Chat v4 z szyfrowaną lokalną bazą danych został wydany

**Opublikowano:** 28 września, 2022

## Co nowego w v4

- [szyfrowanie lokalnej bazy danych czatu](#szyfrowanie-lokalnej-bazy-danych-czatu).
- [wsparcie dla samodzielnie hostowanych serwerów WebRTC ICE](#samodzielnie-hostowane-serwery-webrtc-ice).
- [poprawa stabilności tworzenia nowych połączeń: większa niezawodność grup, plików i kontaktów](#improved-stability-of-creating-new-connections).
- [usuwanie plików i mediów](#usuwanie-plików-i-mediów).
- [dla deweloperów - TypeScript SDK do integracji z SimpleX Chat](#dla-deweloperów---typescript-sdk-do-integracji-z-simplex-chat) (np. boty lub asystenci czatu).
- animowane obrazy w aplikacji na Androida.
- wyłączenie wiadomości na kontakt / grupę w aplikacji terminalowej (jest to już obsługiwane w aplikacjach mobilnych).

Ponadto, ta wersja dodaje język niemiecki do UI aplikacji mobilnych - ogromne podziękowania dla [Michael](https://github.com/mlanp), który je stworzył!

Inne nowe funkcje od v3:

- tajne grupy czatu (zobacz szczegóły w [ogłoszeniu v3.1](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/blog/20220808-simplex-chat-v3.1-chat-groups.md#tajne-grupy-czatowe) - są one w pełni zdecentralizowane, tylko ich członkowie wiedzą o istnieniu tych grup.
- dostęp do serwerów wiadomości przez Tor z obsługą adresów serwerów .onion (zobacz szczegóły w ogłoszeniach [v3.1](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/blog/20220808-simplex-chat-v3.1-chat-groups.md#dostęp-do-serwerów-wiadomości-przez-tor) i [v3.2](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/blog/20220901-simplex-chat-v3.2-incognito-mode.md#używanie-adresów-serwerów-onion-z-tor)) - aby chronić anonimowość użytkowników na poziomie transportu TCP.
- Tryb incognito - udostępnianie losowej nazwy profilu każdemu nowemu kontaktowi, aby całkowicie wyeliminować wszelkie współdzielone dane między nimi (zobacz szczegóły w [v3.2](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/blog/20220901-simplex-chat-v3.2-incognito-mode.md#tryb-incognito) ogłoszeniu).
- nie kończące się przewijanie i wyszukiwanie w czatach.
- zmniejszony rozmiar Android APK do bezpośredniego pobrania i w repozytorium F-Droid z 200 do 50Mb!

[Audyt wdrożeniowy jest zaplanowany na październik](#we-ask-you-to-help-us-pay-for-3rd-party-security-audit)!

### Szyfrowanie lokalnej bazy danych czatu

<img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20220928-passphrase.png" width="330">

SimpleX Chat zawsze skupiał się na ochronie wiadomości w trakcie dostarczania, a nie gdy są one przechowywane na urządzeniu. To wydanie to zmienia - teraz wszystkie wiadomości, które otrzymujesz i wysyłasz są przechowywane na urządzeniu zaszyfrowane za pomocą [SQLCipher](https://github.com/sqlcipher/sqlcipher).

**Zwróć uwagę**: Jeśli używasz już SimpleX Chat, Twoja baza danych pozostanie niezaszyfrowana, dopóki nie wprowadzisz frazy hasła poprzez ustawienia aplikacji. Musisz zapamiętać wybrane hasło, ponieważ nie ma możliwości odzyskania go w przypadku utraty.

Domyślnie Twoja fraza hasła będzie bezpiecznie przechowywana na urządzeniu (w KeyChain na iOS lub zaszyfrowana kluczem przechowywanym w TPM, jeśli jest dostępny, na Androidzie) - jest dostępna tylko dla aplikacji i tylko na jednym urządzeniu. Przechowywanie frazy hasła jest wymagane do działania natychmiastowych powiadomień. W tym przypadku, jeśli zgubisz hasło, aplikacja będzie nadal działać, ale nie będziesz mógł zmienić hasła i przenieść swojego profilu użytkownika na inne urządzenie.

Dla dodatkowego bezpieczeństwa swoich wiadomości masz również możliwość usunięcia frazy hasła z urządzenia. W tym przypadku będziesz musiał wprowadzić frazę hasła przy każdym uruchomieniu aplikacji. Powiadomienia będą działać tylko wtedy, gdy aplikacja będzie w tle. Dla iOS oznacza to, że powiadomienia okresowe i lokalne będą działać, ale powiadomienia błyskawiczne pokażą tylko, że jest dostępna wiadomość, ale nie treść wiadomości ani od kogo jest - aby zobaczyć wiadomości, trzeba będzie otworzyć aplikację i wprowadzić frazę hasła. W tym przypadku, jeśli stracisz frazę hasła, nie będziesz w stanie otworzyć aplikacji lub odszyfrować bazy danych - więc upewnij się, że przechowujesz ją bezpiecznie.

### Samodzielnie hostowane serwery WebRTC ICE

<img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20220928-ice-servers.png" width="330">

Mogliście wykonywać połączenia audio i wideo za pośrednictwem serwerów SimpleX Chat WebRTC od [v3](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/blog/20220711-simplex-chat-v3-released-ios-notifications-audio-video-calls-database-export-import-protocol-improvements.md#szyfrowane-połączenia-audiovideo-end-to-end) - oznaczało to, że nasze serwery mogły obserwować wasze adresy IP. To wydanie dodaje konfigurację do używania własnych serwerów STUN/TURN, pomagając Ci chronić Twoją prywatność.

Zobacz [ten przewodnik](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/docs/WEBRTC.md), aby dowiedzieć się, jak wdrożyć własny serwer `coturn` i skonfigurować aplikacje mobilne do korzystania z niego.

### Poprawiona stabilność tworzenia nowych połączeń

Tajne grupy uczyniły SimpleX Chat znacznie bardziej użytecznym, ale ponieważ grupy SimpleX są całkowicie zdecentralizowane i aby działały, każdy członek powinien połączyć się ze wszystkimi innymi członkami, czasami te połączenia zawodzą, a grupa staje się fragmentaryczna - niektórzy członkowie nie otrzymują wszystkich wiadomości. Było to bardziej powszechne dla większych grup, ponieważ liczba wymaganych połączeń członków jest O(n^2) wielkości grupy.

Powodem tego problemu było to, że niektóre operacje sieciowe wymagane dla połączeń grupowych nie były ponownie próbowane. To wydanie poprawia stabilność wszystkich operacji sieciowych - odbierania wiadomości, nawiązywania nowych połączeń kontaktowych, odbierania plików i łączenia się z członkami w dołączonych grupach.

### Usuwanie plików i mediów

<img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20220928-files-media.png" width="330">

Podczas gdy baza danych z wiadomościami i wszystkimi kontaktami jest teraz szyfrowana, pliki, które otrzymujesz i wysyłasz, nie są (planujemy to poprawić później). Aby chronić bezpieczeństwo plików, dodaliśmy opcję usuwania wszystkich plików z magazynu aplikacji - upewnij się tylko, że nie zrobisz tego, zanim pliki, które wysyłasz, zostaną dostarczone do odbiorców, lub nie otrzymają ich.

### Dla deweloperów - TypeScript SDK do integracji z SimpleX Chat

Minęło sporo czasu, odkąd możliwe było stworzenie chatbota przy użyciu SimpleX Chat jako biblioteki - ale trzeba było albo napisać kod Haskella, albo użyć interfejsu obcych funkcji w innym języku.

Z v4 ogłaszamy [TypeScript SimpleX Chat Client SDK](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/apps/packages/simplex-chat-client/typescript/README.md), które można wykorzystać do tworzenia dowolnych integracji z SimpleX Chat CLI.

Możesz uruchomić SimpleX Chat CLI jako lokalny serwer WebSockets na dowolnym porcie, my używamy 5225 tutaj:

```bash
simplex-chat -p 5225
```

Następnie można stworzyć aplikację JavaScript lub TypeScript, która łączyłaby się z nim i sterowała nim za pomocą prostego API WebSocket. TypeScript SDK definiuje wszystkie niezbędne typy i funkcje wygody do wykorzystania w twoich aplikacjach. Zobacz to [przykładowy bot](https://github.com/simplex-chat/simplex-chat/blob/stable/packages/simplex-chat-client/typescript/examples/squaring-bot.js) i [strona README](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/apps/packages/simplex-chat-client/typescript/README.md).

SimpleX Chat API pozwala na:

- tworzyć i zmieniać profil użytkownika (choć w większości przypadków zrobiłbyś to ręcznie, za pośrednictwem aplikacji terminalowej SimpleX Chat).
- tworzyć i akceptować zaproszenia lub łączyć się z kontaktami.
- utworzenie i zarządzanie długoterminowym adresem użytkownika, akceptując żądania połączenia z kodu lub automatycznie.
- tworzyć, dołączać i zarządzać grupami - może to być wykorzystane na przykład do połączenia dwóch różnych osób, które połączyły się z chat-botem.
- wysyłać i odbierać pliki.

Niektóre możliwe aplikacje, które możesz stworzyć:

- boty do handlu peer-to-peer, które łączyłyby ludzi z pasującymi zleceniami kupna/sprzedaży,
- boty do dostępu do informacji,
- boty tłumaczące języki,
- itp.

Ponieważ protokoły SimpleX Chat zapewniają silne szyfrowanie i autoryzację połączeń, mógłbyś je wykorzystać nie tylko w różnych scenariuszach komunikacyjnych, ale także do zdalnego sterowania dowolnym sprzętem, gdzie wymagany jest wysoki poziom bezpieczeństwa, np:

- automatyka inteligentnego domu,
- usługi sieciowe,
- zdalne usuwanie danych z aplikacji,
- itp.

Naprawdę czekamy na to, jakie aplikacje stworzycie - prosimy o zgłaszanie swoich pomysłów i implementacji, linki opublikujemy na osobnej stronie w serwisie i repozytorium GitHub.

## Platforma SimpleX

[Jak SimpleX może dostarczać wiadomości bez identyfikatorów użytkowników](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/blog/20220511-simplex-chat-v2-images-files.md#pierwsza-platforma-komunikacyjna-która-nie-posiada-żadnych-identyfikatorów-użytkowników).

[Jakie są zagrożenia związane z posiadaniem identyfikatorów przypisanych do użytkowników](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/blog/20220711-simplex-chat-v3-released-ios-notifications-audio-video-calls-database-export-import-protocol-improvements.md#dlaczego-posiadanie-identyfikatorów-użytkowników-jest-złe-dla-użytkowników).

[Szczegóły techniczne i ograniczenia](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/blog/20220723-simplex-chat-v3.1-tor-groups-efficiency.md#prywatność-szczegóły-techniczne-i-ograniczenia).

[Jak SimpleX różni się od Session, Matrix, Signal, etc.](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/README.md#najczęściej-zadawane-pytania).

## Prosimy o pomoc w opłaceniu audytu bezpieczeństwa przeprowadzanego przez trzecią stronę

Podpisaliśmy już umowę i zapłaciliśmy za audyt bezpieczeństwa!

Jest on zaplanowany na październik i jeśli nie będzie większych problemów, opublikujemy ten raport od razu, w przeciwnym razie - gdy tylko je naprawimy.

Jest to duży wydatek - ponad 20 000 dolarów - będę bardzo wdzięczny, jeśli pomożecie nam pokryć część tego kosztu z darowizn.

Naszą obietnicą dla naszych użytkowników jest to, że protokoły SimpleX są i pozostaną otwarte, i w domenie publicznej, - więc każdy może budować przyszłe implementacje klientów i serwerów. W tym roku ustanowimy ramy prawne, aby zapewnić, że nie ulegną one zmianie, jeśli w przyszłości zmieni się właściciel SimpleX Chat Ltd.

Proszę rozważyć przekazanie darowizny - pomoże nam to zebrać więcej funduszy. Przekazanie dowolnej kwoty, nawet ceny filiżanki kawy, zrobiłoby dla nas ogromną różnicę.

Możliwe jest przekazanie darowizny poprzez:

- [GitHub](https://github.com/sponsors/simplex-chat): nie pobiera prowizji od nas.
- [OpenCollective](https://opencollective.com/simplex-chat): przyjmuje również darowizny w kryptowalutach, ale pobiera prowizję.
- Portfel Monero: 8568eeVjaJ1RQ65ZUn9PRQ8ENtqeX9VVhcCYYhnVLxhV4JtBqw42so2VEUDQZNkFfsH5sXCuV7FN8VhRQ21DkNibTZP57Qt
- Portfel Bitcoin: 1bpefFkzuRoMY3ZuBbZNZxycbg7NYPYTG

Dziękuję,

Evgeny

Założyciel SimpleX Chat
