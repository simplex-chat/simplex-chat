---
layout: layouts/article.html
title: "SimpleX Chat reviews and v4.3 released – with instant voice messages, irreversible deletion of sent messages and improved server configuration."
date: 2022-12-06
image: images/20221206-voice.png
imageBottom: true
previewBody: blog_previews/20221206.html
permalink: "/blog/20221206-simplex-chat-v4.3-voice-messages.html"
---

# Recenzje SimpleX Chat i wydana v4.3 - z błyskawicznymi wiadomościami głosowymi, nieodwracalnym usuwaniem wysłanych wiadomości i poprawioną konfiguracją serwera.

**Opublikowano:** 6 grudnia 2022 r.

## Recenzje SimpleX Chat

Odkąd opublikowaliśmy [ocenę bezpieczeństwa SimpleX Chat](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/blog/20221108-simplex-chat-v4.2-security-audit-new-website.html) ukończoną przez Trail of Bits w listopadzie, kilka stron opublikowało recenzje i włączyło ją do swoich zaleceń:

- Privacy Guides dodał SimpleX Chat do [polecanych prywatnych i bezpiecznych komunikatorów](https://www.privacyguides.org/real-time-communication/#simplex-chat).
- Mike Kuketz - znany ekspert ds. bezpieczeństwa - opublikował [recenzję SimpleX Chat](https://www.kuketz-blog.de/simplex-eindruecke-vom-messenger-ohne-identifier/) i dodał go do [messenger matrix](https://www.messenger-matrix.de).
- Supernova opublikowała [recenzję](https://supernova.tilde.team/detailed_reviews.html#simplex) i zwiększyła [oceny rekomendacji SimpleX Chat](https://supernova.tilde.team/messengers.html).

## Co nowego w v4.3.

- [natychmiastowe wiadomości głosowe!](#natychmiastowe-wiadomości-głosowe)
- [nieodwracalne usuwanie wysłanych wiadomości dla wszystkich odbiorców](#nieodwracalne-usuwanie-wiadomości)
- [ulepszona konfiguracja serwerów SMP i wsparcie dla haseł serwerowych](#serwery-smp-konfiguracja-i-hasła)
- [poprawki prywatności i bezpieczeństwa](#poprawki-prywatności-i-bezpieczeństwa):
  - ochrona ekranu aplikacji w ostatnich aplikacjach i zapobieganie zrzutom ekranu
  - poprawiona prywatność i bezpieczeństwo linków zaproszenia SimpleX w aplikacji
  - opcjonalna kopia zapasowa danych aplikacji Android
  - opcjonalnie umożliwić bezpośrednie wiadomości między członkami grupy

### Natychmiastowe wiadomości głosowe

<img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20221206-voice.png" width="288">

Wiadomości głosowe, w przeciwieństwie do zwykłych plików, są wysyłane natychmiast, w istniejącym połączeniu z kontaktem i bez akceptacji ze strony odbiorcy. Z tego powodu ograniczyliśmy rozmiar wiadomości głosowych do ~92,5kb (odpowiednik 6 wiadomości), co ogranicza czas trwania do 30 sekund na iOS i do ~42 sekund na Androidzie (rozmiar jest inny z powodu różnych koderów), ze średnią jakością dźwięku. Wiadomości głosowe są wysyłane w formacie MP4AAC, który jest natywnie obsługiwany zarówno na iOS, jak i na Androidzie, a pliki wiadomości głosowych można odtwarzać poza aplikacją SimpleX Chat.

Użytkownicy, którzy nie chcą otrzymywać wiadomości głosowych, mogą je wyłączyć, albo globalnie, dla wszystkich kontaktów, albo dla każdego kontaktu niezależnie. Należy pamiętać, że globalna zmiana preferencji będzie miała wpływ tylko na kontakty, w których udostępniłeś swój główny profil (nie kontakty incognito) i w których nie zmieniłeś preferencji dla konkretnego kontaktu. Grupy mają osobną politykę, która pozwala wyłączyć wiadomości głosowe dla wszystkich członków (są one domyślnie dozwolone). Właściciel może ustawić tę politykę podczas tworzenia grupy lub później, poprzez stronę Preferencje grupy.

### Nieodwracalne usuwanie wiadomości

<img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20221206-deleted1.png" width="288"> &nbsp;&nbsp; <img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20221206-deleted2.png" width="288">

Kiedy odbierasz pocztę elektroniczną, masz pełne przekonanie, że nadawca nie może usunąć swojego maila z Twojej skrzynki po tym, jak go otrzymałeś. I wydaje się to słuszne - w końcu to Twoje urządzenie i nikt nie powinien mieć możliwości usunięcia z niego jakichkolwiek danych.

Większość istniejących komunikatorów podjęła odwrotną decyzję - nadawcy mogą nieodwracalnie usunąć swoje wiadomości z urządzeń odbiorców po ich dostarczeniu, niezależnie od tego, czy odbiorcy się na to zgadzają, czy nie. I to też wydaje się poprawne - to jest twoja wiadomość, powinieneś móc ją usunąć, przynajmniej na określony czas; to, że wiadomość jest na urządzeniu odbiorcy, nie zmienia twojej własności do tej wiadomości.

O ile oba te stwierdzenia wydają się poprawne, przynajmniej dla niektórych osób, to po prostu nie mogą być jednocześnie poprawne, gdyż przeczą sobie nawzajem - albo jedno, albo oba muszą być błędne. Wydaje się to być bardzo polaryzującym tematem, a [sondaże](https://mastodon.social/@simplex/109461879089268041) [które przeprowadziłem](https://www.reddit.com/r/SimpleXChat/comments/zdam11/poll_irreversible_message_deletion_by_sender_what/) [wczoraj](https://twitter.com/epoberezkin/status/1599797374389727233) [pokaż to](https://www.linkedin.com/feed/update/urn:li:activity:7005564342502842368/) - głosy są podzielone po równo.

Możesz chcieć mieć możliwość usuwania swoich wiadomości nawet po ich otrzymaniu, aby chronić swoją prywatność i bezpieczeństwo, i chcesz, aby produkt komunikacyjny, którego używasz, egzekwował to. Ale możesz też mieć wiele powodów, aby nie zgodzić się na usuwanie wiadomości na swoim urządzeniu z kilku różnych powodów:

- może to być kontekst biznesowy i albo polityka Twojej organizacji, albo wymóg zgodności z przepisami stanowi, że każda otrzymana wiadomość musi być zachowana przez jakiś czas.
- wiadomości te mogą zawierać groźby lub nadużycia i chcesz je zachować jako dowód.
- być może zapłaciłeś za tę wiadomość (np. może to być raport z konsultacji) i nie chcesz, aby nagle zniknęła, zanim miałeś szansę ją przechować poza rozmową.

Zamiast brać stronę w tym wyborze, postanowiliśmy umożliwić zmianę tego zachowania globalnie lub oddzielnie dla każdego kontaktu lub grupy. To sprawia, że SimpleX Chat jest wyjątkowy, nadając się zarówno do kontekstów komunikacyjnych, w których tradycyjnie używa się poczty elektronicznej, jak i do kontekstów nieformalnych lub wrażliwych na prywatność, które pozwoliłyby nadawcom na nieodwracalne usunięcie wiadomości, pod warunkiem, że odbiorcy się na to zgodzą.

W każdym razie, nadawcy nigdy nie mogą być w 100% pewni, że wiadomość została usunięta z urządzenia odbiorcy - odbiorca może uruchomić zmodyfikowanego klienta, który nie honoruje ustawienia konwersacji, i nie ma sposobu, aby upewnić się, jaki kod twój kontakt uruchamia na swoim urządzeniu.

Jeśli nieodwracalne usuwanie wiadomości nie jest dozwolone w konwersacji, nadawcy mogą nadal oznaczać swoje wiadomości jako usunięte, a to pokaże "mark deleted" placeholder w konwersacji. Odbiorcy mogą wtedy zarówno ujawnić treść oryginalnej wiadomości, jak i całkowicie usunąć ją na swoich urządzeniach.

### Serwery SMP komfiguracja i hasła

<img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20221206-server1.png" width="288"> &nbsp;&nbsp; <img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20221206-server2.png" width="288"> &nbsp;&nbsp; <img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20221206-server3.png" width="288">

Kiedy samodzielnie hostujesz swój własny serwer SMP, możesz chcieć go upublicznić, aby każdy mógł używać go do odbierania wiadomości. Ale wielu użytkowników chce hostować swoje prywatne serwery, tak aby tylko oni i ich przyjaciele mogli używać ich do odbierania wiadomości.

Wersja v4.0 serwera SMP i nowa wersja aplikacji dodaje wsparcie dla haseł serwera. Jest ono wybierane losowo podczas inicjalizacji nowego serwera, a jeśli masz już serwer, możesz je zmienić. Nadal każdy może wysłać do Ciebie wiadomość, nie wymaga to znajomości hasła, a linki, które udostępniasz nie zawierają go, ale aby móc odbierać wiadomości musisz znać adres serwera, który zawiera hasło. W pewnym sensie jest to podobne do tego, jak działa podstawowe uwierzytelnianie w HTTP i jak przeglądarki obsługują URI z dołączonymi danymi uwierzytelniającymi.

Nowa sekcja konfiguracji serwerów pozwala teraz przetestować serwery przed rozpoczęciem korzystania z nich, a także można udostępnić adresy serwerów za pomocą kodu QR, dzięki czemu znajomi lub zespół mogą również z nich korzystać, bez konieczności kopiowania i wklejania adresów.

Możesz przeczytać jak zainstalować i skonfigurować serwery SMP w [tym przewodniku](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/docs/SERVER.md).

### Poprawki prywatności i bezpieczeństwa

#### Ekran aplikacji Protect

<img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20221206-protect.png" width="330">.

Obecnie jest domyślnie włączona, ale można ją wyłączyć poprzez ustawienia.

Aplikacja na iOS ukrywa tylko ekran aplikacji w ostatnich aplikacjach, aplikacja na Androida oprócz tego zapobiega również zrzutom ekranu.

To nie jest rozwiązanie zabezpieczające dla nadawców, i zrobiliśmy to opcjonalnie, ponieważ odbiorca może obejść to i tak - to jest dla Ciebie, aby chronić swój ekran aplikacji, gdy dajesz swój telefon do kogoś.

#### Prywatność i bezpieczeństwo linków do zaproszeń SimpleX

Wcześniej, gdy wysyłałeś komuś link z zaproszeniem, adresem kontaktowym lub linkiem do grupy, zajmował on pół ekranu na czacie, a w niektórych przypadkach mógł się otworzyć w przeglądarce. Ponadto, ponieważ linki te są dość duże, nie jest łatwo sprawdzić, czy domena strony nie została złośliwie podmieniona, przez jaki serwer SMP miałoby przechodzić połączenie lub jaki jest to rodzaj linku.

Ta wersja zamiast pokazywać pełny link pokazuje krótki opis, oraz zastępuje publiczny adres www wewnętrznym schematem URI, którego używa aplikacja (simplex:/) - takie linki otwierają się bezpośrednio w aplikacji. Istnieje opcja pokazania pełnego linku, jeśli jest potrzebny, a nawet otwarcia go w przeglądarce z aplikacji, ale w tym przypadku, jeśli ten link nie używa strony https://simplex.chat/pl, pokaże się jako czerwony, aby go podkreślić.

### Opcjonalna kopia zapasowa danych aplikacji Android

Poprzednia wersja zawsze tworzyła kopię zapasową danych aplikacji w sposób skonfigurowany przez system. Teraz można to nadpisać z wnętrza aplikacji, zapobiegając tworzeniu kopii zapasowej nawet jeśli jest ona włączona przez ustawienia systemowe. Ta wersja wymaga wyłączenia jej ręcznie, sprawimy, że będzie domyślnie wyłączona w następnym wydaniu (v4.3.1).

### Bezpośrednie wiadomości pomiędzy członkami grupy

Nowa wersja nie pozwala na to domyślnie, ale może być włączona przez właścicieli grup w ustawieniach grupy, kiedy grupa jest tworzona lub w dowolnym późniejszym momencie.

## Platforma SimpleX

Kilka linków odpowiadających na najczęstsze pytania:

[Jak SimpleX może dostarczać wiadomości bez identyfikatorów użytkowników](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/blog/20220511-simplex-chat-v2-images-files.md#pierwsza-platforma-komunikacyjna-która-nie-posiada-żadnych-identyfikatorów-użytkowników).

[Jakie są zagrożenia związane z posiadaniem identyfikatorów przypisanych do użytkowników](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/blog/20220711-simplex-chat-v3-released-ios-notifications-audio-video-calls-database-export-import-protocol-improvements.md#dlaczego-posiadanie-identyfikatorów-użytkowników-jest-złe-dla-użytkowników).

[Szczegóły techniczne i ograniczenia](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/blog/20220723-simplex-chat-v3.1-tor-groups-efficiency.md#prywatność-szczegóły-techniczne-i-ograniczenia).

[Jak SimpleX różni się od Session, Matrix, Signal, etc.](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/README.md#najczęściej-zadawane-pytania).

Proszę również zapoznać się z informacjami na naszej [nowej stronie internetowej](https://simplex.chat/pl) - ona również odpowiada na wszystkie te pytania.

## Wspomóż nas darowiznami

Ogromne podziękowania dla wszystkich, którzy przekazali darowiznę na rzecz SimpleX Chat!

Stawiamy na pierwszym miejscu prywatność i bezpieczeństwo użytkowników - bez waszego wsparcia byłoby to niemożliwe.

Zobowiązujemy się, że protokoły SimpleX są i pozostaną otwarte, w domenie publicznej, więc każdy może budować przyszłe implementacje klientów i serwerów. Budujemy platformę SimpleX w oparciu o te same zasady, co w przypadku poczty elektronicznej i Internetu, ale znacznie bardziej prywatną i bezpieczną.

Wasze darowizny pomagają nam zebrać więcej funduszy - każda kwota, nawet cena filiżanki kawy, robi dla nas wielką różnicę.

Możliwe jest przekazanie darowizny poprzez:

- [GitHub](https://github.com/sponsors/simplex-chat) - nie pobiera prowizji od nas.
- [OpenCollective](https://opencollective.com/simplex-chat) - pobiera prowizję, a także przyjmuje darowizny w wielu kryptowalutach.
- Adres Monero: 8568eeVjaJ1RQ65ZUn9PRQ8ENtqeX9VhcCYYhnVLxhV4JtBqw42so2VEUDQZNkFfsH5sXCuV7FN8VhRQ21DkNibTZP57Qt
- Adres Bitcoin: 1bpefFkzuRoMY3ZuBbZNZxycbg7NYPYTG
- Adres Ethereum: 0x83fd788f7241a2be61780ea9dc72d2151e6843e2
- proszę dać nam znać, za pośrednictwem wydania GitHub lub czatu, jeśli chcesz przekazać darowiznę w jakiejś innej kryptowalucie - dodamy adres do listy.

Dziękujemy,

Evgeny

Założyciel SimpleX Chat
