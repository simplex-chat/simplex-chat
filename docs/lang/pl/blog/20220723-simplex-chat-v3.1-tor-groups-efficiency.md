---
layout: layouts/article.html
title: "SimpleX Chat v3.1-beta is released &mdash; improved battery/traffic usage"
date: 2022-07-23
image: images/20220723-group-invite.png
imageBottom: true
previewBody: blog_previews/20220723.html
permalink: "/blog/20220723-simplex-chat-v3.1-tor-groups-efficiency.html"
---

# SimpleX Chat v3.1-beta został wydany - poprawiono zużycie baterii/ruchu

**Opublikowano:** Jul 23, 2022

## Co nowego

- aplikacja terminalowa: [dostęp do serwerów wiadomości przez SOCKS5 proxy](#aplikacja-terminal-dostęp-do-serwerów-wiadomości-przez-socks5-proxy--tor) (np. Tor).
- aplikacje mobilne: [dołączanie i opuszczanie grup czatowych](#aplikacje-mobilne-dołączanie-i-opuszczanie-grup-czatu).
- [optymalizacja zużycia baterii i ruchu - redukcja do 90x!](#zoptymalizowane-zużycie-baterii-i-ruchu--redukcja-do-90x).
- [dwie konfiguracje dockera dla samodzienie hostowanych serwerów SMP](#konfiguracja-dockera-dla-samodzielnie-hostowanych-serwerów-smp).

### Aplikacja Terminall: dostęp do serwerów wiadomości przez SOCKS5 proxy / Tor

<img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20220723-tor.jpg" width="480">

Podczas gdy protokół SMP skupia się na ochronie metadanych na poziomie aplikacji poprzez używanie identyfikatorów połączeń parami zamiast identyfikatorów użytkowników (które są używane przez wszystkie inne platformy do przesyłania wiadomości), istnieją scenariusze, w których ważne jest, aby użytkownicy chronili swoje adresy IP przed serwerami - sporo użytkowników było nieco rozczarowanych, że nie dodaliśmy tego wcześniej.

To wydanie aplikacji terminalowej obsługuje dostęp do serwerów poprzez Tor, ale same serwery są nadal dostępne na swoich zwykłych adresach. Planujemy dodać adresy .onion (ukryta usługa v3) do wszystkich serwerów wiadomości, które dostarczamy, a użytkownicy, którzy samodzielnie hostują serwery, będą mogli również mieć podwójne adresy serwerów - tak, że jedna strona w rozmowie może uzyskać dostęp do serwerów za pośrednictwem adresu .onion, bez konieczności wymagania, aby druga strona również używała Tor.

Aby uzyskać dostęp do serwerów SimpleX przez Tor należy zainstalować Tor proxy i uruchomić simplex-chat z opcją `-x`. Więcej informacji można znaleźć w [dokumencie aplikacji terminalowej](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/docs/CLI.md#dostęp-do-serwerów-wiadomości-przez-tor).

Ponieważ jest to wydanie beta, aby je zainstalować musisz użyć tej komendy:

```sh
curl -o- https://raw.githubusercontent.com/simplex-chat/simplex-chat/stable/install.sh | sh -s -- v3.1.0-beta.0
```

### Aplikacje mobilne: dołączanie i opuszczanie grup czatu

<img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20220723-group-invite.png" width="330"> <img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20220723-group-accept.png" width="330"> <img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20220723-group-leave.png" width="330">

Grupy są obsługiwane przez rdzeń SimpleX Chat od bardzo dawna, ale w aplikacjach mobilnych nie było interfejsu użytkownika, aby z nich korzystać - użytkownicy musieli używać konsoli czatu, aby tworzyć grupy, dodawać członków i akceptować zaproszenia.

To wydanie pozwala na akceptowanie zaproszeń do grup poprzez UI aplikacji mobilnych, co znacznie ułatwia tworzenie grup - tylko jeden użytkownik (właściciel grupy) musi używać konsoli czatu, podczas gdy wszyscy inni członkowie grupy muszą tylko dotknąć przycisku w UI, aby dołączyć lub opuścić grupę. Pełne UI grupy pojawi się w v3.1 za 1-2 tygodnie, ale już dziś możesz zacząć używać grup instalując wersje beta aplikacji mobilnych poprzez [TestFlight](https://testflight.apple.com/join/DWuT2LQu), [Google PlayStore Beta](https://play.google.com/apps/testing/chat.simplex.app) i [APK](https://github.com/simplex-chat/simplex-chat/releases/latest/download/simplex.apk).

Aby zarządzać grupami poprzez aplikację terminalową lub poprzez konsolę czatu w aplikacjach mobilnych należy użyć następujących komend:

- aby utworzyć grupę: `/g <group_name> [<description>]`.
- aby dodać członka (domyślnie admin): `/a <group_name> <contact_name> [owner/admin/member]`.
- aby usunąć członka z grupy: `/rm <group_name> <contact_name>`.

Przyjmowanie zaproszeń do grup, opuszczanie i usuwanie grup nie wymaga już używania poleceń konsoli.

### Zoptymalizowane zużycie baterii i ruchu - redukcja do 90x

Aby zmniejszyć zużycie baterii i ruchu, ta wersja zaktualizowała protokół SMP, aby umożliwić łączenie wielu poleceń serwera (do 90!) w jeden blok ruchu - pod warunkiem, że zarówno serwer jak i klient są zaktualizowane. Oznacza to, że jeśli masz 90 kontaktów (lub członków grupy) na jednym serwerze, aby zapisać się do wszystkich kolejek wiadomości, musisz teraz wysłać tylko jeden blok 16kb zamiast ~1.5Mb ruchu (90 bloków). Ukrywa również, ile kontaktów masz od wszelkich napastników, którzy obserwują twoją sieć.

Możesz zauważyć, że wysyłanie poleceń do wielu kolejek w jednym bloku pozwoliłoby serwerowi skorelować, że wszystkie te kolejki należą do tego samego użytkownika, nawet jeśli obecna implementacja serwera, której używamy, nie robi tego. Jednak nawet bez podziału na partie, ponieważ polecenia są wysyłane przez to samo połączenie TCP, ta korelacja była już możliwa, więc w porównaniu z poprzednią wersją nie ma minusów.

Aby zmniejszyć ryzyko, że serwery skorelują twoje kolejki wiadomości, wkrótce dodamy opcję dostępu do każdej kolejki przez osobne połączenie TCP, które będzie musiało być używane razem z dostępem przez Tor (ponieważ w przeciwnym razie serwery nadal widziałyby ten sam adres IP). Chociaż zwiększy to zużycie baterii i ruchu, zapewni również najwyższy poziom prywatności.

### Konfiguracja Dockera dla samodzielnie hostowanych serwerów SMP

Kiedy wydaliśmy SimpleX Chat v3 dwa tygodnie temu, wielu użytkowników chciało hostować serwery wiadomości w kontenerach docker. Dlatego teraz oferujemy [dwie wersje konfiguracji docker](https://github.com/simplex-chat/simplexmq/tree/stable/scripts/docker):

- szybka i wygodna - pobiera binarkę serwera SMP z GitHuba.
- bardziej bezpieczną - buduje serwer SMP z kodu źródłowego.

Daj nam znać, jak działa dla Ciebie!

## Platforma SimpleX

Budujemy nową platformę dla rozproszonych aplikacji internetowych, gdzie prywatność wiadomości _i_ sieci ma znaczenie. [SimpleX Chat](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/README.md) to nasza pierwsza aplikacja, aplikacja do przesyłania wiadomości zbudowana na platformie SimpleX.

### Pierwsza (i jedyna?) platforma komunikacyjna bez jakichkolwiek identyfikatorów użytkowników - 100% prywatna z założenia!

Aby chronić tożsamość użytkowników i ich połączeń, zamiast identyfikatorów użytkowników widocznych dla serwerów i/lub sieci (które są używane przez wszystkie inne platformy do przesyłania wiadomości), SimpleX Chat używa [identyfikatorów parami](https://csrc.nist.gov/glossary/term/Pairwise_Pseudonymous_Identifier) połączeń między użytkownikami - w każdym połączeniu są dwie kolejki, każda kolejka ma 2 różne identyfikatory do wysyłania i odbierania wiadomości. Zwiększa to liczbę używanych identyfikatorów do kwadratu liczby użytkowników, utrudniając (lub uniemożliwiając) ustalenie, kto z kim rozmawia. Napisałem [wcześniej](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/blog/20220711-simplex-chat-v3-released-ios-notifications-audio-video-calls-database-export-import-protocol-improvements.md#dlaczego-posiadanie-identyfikatorów-użytkowników-jest-złe-dla-użytkowników) dlaczego posiadanie jakichkolwiek identyfikatorów, nawet losowych liczb, związanych z ich profilami jest złe dla prywatności użytkowników.

### Jeśli SimpleX nie ma identyfikatorów użytkowników, jak może dostarczać wiadomości?

Napisałem o tym w [ogłoszenie wydania v2](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/blog/20220511-simplex-chat-v2-images-files.md), a więcej informacji o celach platformy SimpleX i projekcie technicznym można uzyskać w [the whitepaper](https://github.com/simplex-chat/simplexmq/blob/master/protocol/overview-tjr.md).

### Prywatność: szczegóły techniczne i ograniczenia

Projekt SimpleX podąża za zasadami bezpieczeństwa "głęboka obrona" mając wiele nakładających się mechanizmów obronnych w celu ochrony prywatności i bezpieczeństwa użytkowników:

- Transport TLS z walidacją tożsamości serwera i wiązaniem kanałów, ograniczony do najbezpieczniejszych algorytmów kryptograficznych.
- Trzy poziomy szyfrowania, które chronią zarówno treść wiadomości, jak i metadane, zapobiegając korelacji ruchu, nawet jeśli bezpieczeństwo protokołu TLS zostanie naruszone:
  - szyfrowanie end-to-end w każdej kolejce komunikatów.
  - szyfrowanie end-to-end konwersacji przy użyciu algorytmów podwójnej kraty, które zapewniają komunikację OTR z zachowaniem tajemnicy transmisji w przód i możliwością odzyskania danych po włamaniu.
  - dodatkowa warstwa szyfrowania pomiędzy serwerem a odbiorcą wiadomości, aby zapobiec korelacji ruchu na podstawie szyfrogramu lub jakichkolwiek identyfikatorów.
- Cztery poziomy paddingu wiadomości, aby zapobiec atakom opartym na wielkości zawartości - blok transportowy TLS jest wypełniony do stałego rozmiaru 16kb, a każda z 3 zaszyfrowanych kopert jest wypełniona do stałego rozmiaru przed szyfrowaniem.

Co planujemy dodać wkrótce, aby jeszcze bardziej poprawić prywatność i bezpieczeństwo:

- rotacja kolejki wiadomości, tak aby identyfikatory parami stały się tymczasowe, a Twoje rozmowy przenosiły się z serwera na serwer automatycznie.
- dostęp do serwerów wiadomości poprzez ukryte usługi Tor v3.
- mieszanie wiadomości - dodawanie opóźnień do dostarczania wiadomości, aby chronić przed korelacją ruchu przez czas wiadomości.
- wykorzystanie adresów usług ukrytych Tor v3 dla serwerów wiadomości.
- szyfrowanie lokalnej bazy danych oparte na frazie pasmowej.

SimpleX Chat [strona README](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/README.md#prywatność-szczegóły-techniczne-i-ograniczenia) ma więcej szczegółów na ten temat.

## Prosimy o pomoc w opłaceniu audytu bezpieczeństwa przeprowadzanego przez firmę trzecią

Przejdę od razu do rzeczy: proszę o wsparcie SimpleX Chat poprzez darowizny.

Priorytetem jest dla nas prywatność i bezpieczeństwo użytkowników - byłoby to niemożliwe bez Waszego wsparcia, które do tej pory mieliśmy szczęście otrzymać.

Planujemy przeprowadzenie audytu bezpieczeństwa aplikacji przez stronę trzecią i byłoby to dla nas bardzo pomocne, gdyby część z tych ponad 20000$ wydatków mogła być pokryta z darowizn.

Naszym zobowiązaniem wobec użytkowników jest to, że protokoły SimpleX są i pozostaną otwarte, i w domenie publicznej, - więc każdy może budować przyszłe implementacje dla klientów i serwerów. Budujemy platformę SimpleX w oparciu o te same zasady, co w przypadku poczty elektronicznej i sieci, ale znacznie bardziej prywatną i bezpieczną.

Jeśli już korzystasz z SimpleX Chat lub planujesz korzystać z niego w przyszłości, gdy będzie miał więcej funkcji, rozważ przekazanie darowizny - pomoże nam to zebrać więcej funduszy. Przekazanie dowolnej kwoty, nawet ceny filiżanki kawy, zrobiłoby dla nas ogromną różnicę.

Możliwe jest [przekazanie darowizny przez GitHub](https://github.com/sponsors/simplex-chat), nie pobiera prowizji od nas, lub [przez OpenCollective](https://opencollective.com/simplex-chat), który również przyjmuje darowizny w kryptowalutach, ale pobiera prowizję.

Dziękuję,

Evgeny

Założyciel SimpleX Chat

