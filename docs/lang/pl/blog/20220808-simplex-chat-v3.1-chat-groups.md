---
layout: layouts/article.html
title: "SimpleX Chat v3.1 is released &mdash; with secret groups and server access via Tor"
date: 2022-08-08
image: images/20220808-tor1.png
imageBottom: true
previewBody: blog_previews/20220808.html
permalink: "/blog/20220808-simplex-chat-v3.1-chat-groups.html"
---

# SimpleX Chat v3.1 został wydany - z tajnymi grupami i dostępem do serwera przez Tor

**Opublikowano:** 8 sierpnia 2022 r.

## Co nowego

- [tajne grupy czatowe](#tajne-grupy-czatowe)!
- [dostęp do serwerów wiadomości przez Tor](#dostęp-do-serwerów-wiadomości-przez-tor)
- [zaawansowane ustawienia sieci](#zaawansowane-ustawienia-sieci)
- [opublikowano protokół czatu](#opublikowano-protokół-czatu)
- [nowe ikony aplikacji](#nowe-ikony-aplikacji)
- [inne zmiany od v3](#inne-zmiany-od-wersji-3):
  - zoptymalizowane zużycie baterii i ruchu - nawet 90x redukcja!
  - dwie konfiguracje docker dla samodzielnie utrzymywanych serwerów SMP

### Tajne grupy czatowe

<img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20220808-group1.png" width="330"> <img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20220808-group2.png" width="330"> <img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20220808-group3.png" width="330">

Minął [prawie rok](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/blog/20210914-simplex-chat-v0.4-released.md), odkąd użytkownicy aplikacji terminalowej SimpleX Chat zaczęli eksperymentować z grupami, a teraz jest ona dostępna również dla użytkowników aplikacji mobilnej. Naprawiono wiele błędów, poprawiono stabilność, ale są zarówno funkcje, które musimy dodać, jak i błędy, które musimy naprawić, aby grupy były bardziej użyteczne - naprawdę czekamy na Wasze opinie. Możesz wysłać wszelkie sugestie za pośrednictwem aplikacji, wybierając `Czat z deweloperami` w Ustawieniach aplikacji (lub używając polecenia `/simplex` w aplikacji terminalowej) - to połączy Cię z zespołem SimpleX poprzez jego [stały adres czatu](https://simplex.chat/pl/contact#/?v=1&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2FK1rslx-m5bpXVIdMZg9NLUZ_8JBm8xTt%23MCowBQYDK2VuAyEALDeVe-sG8mRY22LsXlPgiwTNs9dbiLrNuA7f3ZMAJ2w%3D).

Sieć SimpleX jest zdecentralizowana, więc jak działają grupy? W przeciwieństwie do Matrixa lub Signala, które hostują profil grupy i listę członków grupy na swoich serwerach, serwery SimpleX nie mają informacji o istnieniu grupy - mają je tylko jej członkowie. Sieć SimpleX nie przypisuje grupie żadnych globalnie unikalnych identyfikatorów, istnieje jedynie lokalny identyfikator w bazie danych oraz lista członków przechowywana na urządzeniach członków. Użytkownik ma niezależne połączenie z każdym członkiem grupy. Gdy użytkownik wysyła wiadomość do grupy, aplikacja wysyła tę wiadomość niezależnie do każdego członka. Możesz przeczytać więcej o tym, jak działają grupy w [Protokół SimpleX Chat](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/docs/protocol/simplex-chat.md#podprotokół-dla-grup-czatowych).

Ale jak to może się skalować, możesz zapytać? Po prostu nie będzie, a obecny projekt grup nadaje się tylko dla stosunkowo małych grup ludzi, którzy dobrze się znają, zdecydowanie nie większych niż kilkaset członków - ten projekt nadał priorytet prywatności i bezpieczeństwa grupy nad jej wielkością lub wydajnością. Przykładowo, aby wysłać wiadomość do grupy liczącej 100 członków użytkownik musiałby wysłać łącznie ~1.6mb danych (ponieważ każda wiadomość wykorzystuje blok o stałym rozmiarze 16kb). A jeśli użytkownik miałby wysłać plik o wielkości 1mb to wymagałoby to również wysłania go 100 razy (pod warunkiem, że każdy członek go zaakceptuje).

Co zrobić, gdy trzeba wysłać wiele dużych plików do członków grupy? Będziemy rozwijać serwer hostingu plików, gdzie użytkownicy będą mogli przesłać plik (lub obraz) raz, a jedynie wysłać link do pliku i dane uwierzytelniające do wszystkich członków grupy, bez potrzeby wysyłania samego pliku. Mały limit hostingowy będzie dostępny dla wszystkich użytkowników za darmo, opłacany z darowizn, a w przypadku większych plików lub w celu zwiększenia całkowitego limitu użytkownicy będą musieli albo zapłacić niewielki koszt hostingu, albo samemu utrzymywać ten serwer - będzie on dostępny jako kod open-source.

A co jeśli potrzebujesz dużej grupy - np. 100 000 członków lub więcej? Jeszcze w tym roku wprowadzimy kanały SimpleX, które mogą być zarówno publiczne, jak i prywatne. Kanały te będą wymagały serwera do ich hostowania, dostarczonego przez SimpleX Chat lub własnego - tak jak w przypadku strony internetowej. Jeśli jest to kanał publiczny, będzie on opcjonalnie dostępny również przez przeglądarkę internetową i będzie można go osadzić na dowolnej stronie internetowej.

Inne usprawnienia dotyczące grup dodamy wkrótce:

- zarządzaj powiadomieniami w każdej grupie niezależnie.
- wyszukiwanie wiadomości - jest to przydatne również w rozmowach bezpośrednich, ale ważniejsze dla grup.

### Dostęp do serwerów wiadomości przez Tor

<img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20220808-tor1.png" width="330"> &nbsp; <img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20220808-tor2.png" width="330">

Protokoły SimpleX koncentrują się na ochronie metadanych na poziomie aplikacji - nie mają żadnych identyfikatorów użytkownika używanych przez wszystkie inne platformy do przesyłania wiadomości, zamiast tego polegając na identyfikatorach połączeń parami.

Istnieją jednak scenariusze, w których użytkownicy muszą chronić swoje adresy IP przed serwerami i wszelkimi obserwatorami sieci - najlepiej zrobić to za pomocą Tor, aby uzyskać dostęp do wszelkich usług sieciowych.

To wydanie pozwala na dostęp do serwerów wiadomości SimpleX przez Tor na wszystkich platformach:

- aplikacja terminal beta wspiera to od kilku tygodni: aby uzyskać dostęp do serwerów SimpleX przez Tor należy zainstalować Tor proxy i uruchomić simplex-chat z opcją `-x`. Zobacz [dokument aplikacji terminalowej](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/docs/CLI.md#dostęp-do-serwerów-wiadomości-przez-tor) aby uzyskać więcej informacji.
- Aplikacja na Androida obsługuje dostęp przez Tor za pomocą Orbot SOCKS proxy. Po zainstalowaniu i uruchomieniu Orbota, musisz włączyć ustawienie `Sieć i serwery / Użyj proxy SOCKS` w aplikacji, aby uzyskać dostęp do serwerów SimpleX przez Tor.
- Aplikacja iOS może być również używana z aplikacją Orbot iOS (która jest zainstalowana jako systemowy dostawca VPN). Jedynym ustawieniem, które może być potrzebne do zmiany jest zwiększenie timeoutów sieciowych w aplikacji - aby to zrobić należy włączyć `Narzędzia deweloperskie`, a następnie wybrać `Sieć i serwery / Zaawansowane ustawienia sieci / Ustaw timeouty dla proxy`.

Obecnie wszystkie serwery są dostępne poprzez ich publiczne adresy internetowe, a podczas gdy użytkownicy mogą samodzielnie hostować serwery wiadomości na adresach .onion (jako ukryte usługi v3), wymagałoby to, aby oba połączone kontakty używały Tor. Planujemy wkrótce dodać wsparcie dla podwójnych adresów serwerów, aby umożliwić dostęp do tego samego serwera zarówno przez jego publiczny adres internetowy, jak i przez adres .onion, dzięki czemu użytkownicy mogą uzyskać dostęp do serwerów bez wychodzenia z Tora (przez adres .onion), ale ich kontakty mogą uzyskać dostęp do tych samych serwerów bez korzystania z Tora.

### Zaawansowane ustawienia sieci

Aby zmniejszyć ruch w powolnych sieciach dodaliśmy ustawienia dostępu do sieci. Aby użyć tych ustawień, musisz najpierw włączyć `Narzędzia deweloperskie`, a następnie wybrać `Sieć i serwery / Zaawansowane ustawienia sieci`:

- jeśli Twoje połączenia z serwerami są niestabilne i często widzisz obrotówki na liście czatów, zwiększ timeouty połączenia i protokołu - powinno to zmniejszyć ruch, ale może też sprawić, że aplikacja będzie nieco wolniejsza, gdy Twoje połączenie internetowe jest powolne.
- jeśli Twoje połączenie z serwerami wydaje się stabilne, ale ruch jest wysoki, spróbuj wyłączyć ustawienie "utrzymywanie aktywności TCP" lub zwiększyć okres bezczynności utrzymywania aktywności (`TCP_KEEP_IDLE`) i interwał (`TCP_KEEP_INTVL`).

Po zbadaniu jak te ustawienia wpływają na ruch i doświadczenie użytkownika, uprościmy je - ogromne podziękowania dla wszystkich testujących je i zgłaszających wszelkie problemy z ruchem.

### Nowe ikony aplikacji

<img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20220808-icons.png" width="330">

Wielu użytkowników prosiło o umożliwienie personalizacji aplikacji, to dopiero początek - teraz możesz wybrać opcję jasnej lub ciemnej ikony poprzez ustawienia `Wygląd`.

Więcej opcji dostosowywania aplikacji nadchodzi - proszę dać nam znać, co jest najważniejsze.

### Opublikowano protokół czatu

Protokoły [niskopoziomowe protokoły SimpleX](https://github.com/simplex-chat/simplexmq/blob/stable/protocol/) zostały opublikowane dawno temu i aktualizowane w celu odzwierciedlenia ewolucji protokołów, wysokopoziomowy protokół czatu nie został opublikowany wcześniej. Powodem tego było umożliwienie nam szybkiej iteracji, bez zobowiązywania się do jakichkolwiek decyzji.

To jest [pierwszy szkic Protokołu SimpleX Chat](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/docs/protocol/simplex-chat.md) - daj nam znać o wszelkich pytaniach lub sugestiach.

### Inne zmiany od wersji 3

Od wydania v3 zoptymalizowaliśmy również zużycie baterii i ruch - z nawet 90x redukcją ruchu w niektórych przypadkach - i opublikowaliśmy dwie konfiguracje docker dla samodzielnie hostowanych serwerów SMP. Przeczytaj więcej o tym w poprzednim [ogłoszeniu wersji beta](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/blog/20220723-simplex-chat-v3.1-tor-groups-efficiency.md).

## Platforma SimpleX

Kilka linków odpowiadających na najczęstsze pytania:

[Jak SimpleX może dostarczać wiadomości bez identyfikatorów użytkowników](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/blog/20220511-simplex-chat-v2-images-files.md#pierwsza-platforma-komunikacyjna-która-nie-posiada-żadnych-identyfikatorów-użytkowników).

[Jakie są zagrożenia związane z posiadaniem identyfikatorów przypisanych do użytkowników](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/blog/20220711-simplex-chat-v3-released-ios-notifications-audio-video-calls-database-export-import-protocol-improvements.md#dlaczego-posiadanie-identyfikatorów-użytkowników-jest-złe-dla-użytkowników).

[Szczegóły techniczne i ograniczenia](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/blog/20220723-simplex-chat-v3.1-tor-groups-efficiency.md#prywatność-szczegóły-techniczne-i-ograniczenia).

[Jak SimpleX różni się od Session, Matrix, Signal, etc.](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/README.md#najczęściej-zadawane-pytania).

## Prosimy o pomoc w opłaceniu audytu bezpieczeństwa przeprowadzanego przez trzecią stronę

Przejdę od razu do rzeczy: proszę o wsparcie SimpleX Chat poprzez darowizny.

Priorytetem jest dla nas prywatność i bezpieczeństwo użytkowników - byłoby to niemożliwe bez Waszego wsparcia, które do tej pory mieliśmy szczęście otrzymać.

Planujemy przeprowadzenie audytu bezpieczeństwa aplikacji przez stronę trzecią i byłoby to dla nas bardzo pomocne, gdyby część z tych ponad 20000$ wydatków mogła być pokryta z darowizn.

Naszym zobowiązaniem wobec użytkowników jest to, że protokoły SimpleX są i pozostaną otwarte, i w domenie publicznej, - więc każdy może budować przyszłe implementacje dla klientów i serwerów. Budujemy platformę SimpleX w oparciu o te same zasady, co w przypadku poczty elektronicznej i sieci, ale znacznie bardziej prywatną i bezpieczną.

Jeśli już korzystasz z SimpleX Chat lub planujesz korzystać z niego w przyszłości, gdy będzie miał więcej funkcji, rozważ przekazanie darowizny - pomoże nam to zebrać więcej funduszy. Przekazanie dowolnej kwoty, nawet ceny filiżanki kawy, zrobiłoby dla nas ogromną różnicę.

Możliwe jest przekazanie darowizny poprzez:

- [GitHub](https://github.com/sponsors/simplex-chat): nie pobiera prowizji od nas.
- [OpenCollective](https://opencollective.com/simplex-chat): przyjmuje również darowizny w kryptowalutach, ale pobiera prowizję.
- Portfel Monero: 8568eeVjaJ1RQ65ZUn9PRQ8ENtqeX9VVhcCYYhnVLxhV4JtBqw42so2VEUDQZNkFfsH5sXCuV7FN8VhRQ21DkNibTZP57Qt

Dziękuję,

Evgeny

Założyciel SimpleX Chat
