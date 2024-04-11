---
layout: layouts/article.html
title: "SimpleX File Transfer Protocol - a new protocol for sending large files efficiently, privately and securely."
date: 2023-03-01
preview: CLI and relays implementing the new XFTP protocol are released - you can use them now!
image: images/20230301-xftp.jpg
permalink: "/blog/20230301-simplex-file-transfer-protocol.html"
---

# SimpleX File Transfer Protocol - nowy protokół do sprawnego, prywatnego i bezpiecznego przesyłania dużych plików.

**Opublikowano:** 1 marca, 2023 r.

- [szybki start: jak wysłać plik za pomocą XFTP CLI](#⚡️-szybki-start-wysyłanie-pliku-za-pomocą-xftp-cli)
- [co jest problemem](#whats-the-problem)
- [dlaczego nie użyliśmy jakiegoś istniejącego rozwiązania?](#dlaczego-nie-użyliśmy-jakiegoś-istniejącego-rozwiązania)
- [czym jest XFTP i jak działa?](#czym-jest-xftp-i-jak-działa)
- [co dalej?](#co-dalej)

## ⚡️ Szybki start: wysyłanie pliku za pomocą XFTP CLI w 3 prostych krokach

Pobierz binarne XFTP dla Linuksa z [wydania](https://github.com/simplex-chat/simplexmq/releases/tag/v5.0.0-beta.3) - potrzebujesz pliku `xftp-ubuntu-20_04-x86-64` - zmień jego nazwę na `xftp`.

**Krok 1**: Aby wysłać plik:

```bash
xftp send filename.ext
```

Możesz również wysłać plik, który może być odebrany przez wielu odbiorców używając opcji `-n`:

```bash
xftp send filename.ext -n 10
```

**Krok 2**: Przekaż opis(y) pliku (pliki `rcvN.xftp`) do odbiorcy(ów) w sposób bezpieczny, np. wyślij go jako plik przez SimpleX Chat.

**Krok 3**: Odebranie pliku:

```bash
xftp recv rcvN.xftp
```

Nadawca usuwa również wszystkie fragmenty plików z przekaźników przed ich wygaśnięciem w ciągu 48 godzin za pomocą tego polecenia:

```bash
xftp del ./filename.ext/snd.xftp.private
```

<br>

## Co jest problemem?

Jeśli używasz aplikacji SimpleX Chat wiesz, że wsparcie wysyłania plików i obrazów nie jest zbyt dobre, a wysyłanie filmów i dużych plików jest po prostu niemożliwe. Obecnie występują takie problemy:

- nadawca musi być online, aby transfer pliku został zakończony, po potwierdzeniu go przez odbiorcę.
- kiedy plik zostanie wysłany do grupy, nadawca będzie musiał przesłać go osobno do każdego członka, tworząc dużo ruchu.
- transfer plików jest powolny, ponieważ są one wysyłane w małych kawałkach - około 16kb na wiadomość.

W rezultacie ograniczyliśmy obsługiwany rozmiar plików w aplikacji do 8mb. Nawet dla obsługiwanych plików, jest to dość nieefektywne dla wysyłania jakichkolwiek plików do dużych grup.

## Dlaczego nie użyliśmy jakiegoś istniejącego rozwiązania?

Naprawdę mieliśmy nadzieję znaleźć jakieś istniejące rozwiązanie open-source, które moglibyśmy zintegrować z SimpleX Chat.

Zdecydowaliśmy się nie używać torrentów ani żadnych innych rozwiązań P2P ze względu na ich brak prywatności, kwestionowanie legalności w niektórych jurysdykcjach oraz w wielu przypadkach ze względu na ich nieefektywność w grupach.

Przejrzeliśmy kilka rozwiązań kompatybilnych z S3 (np. [minio](https://github.com/minio/minio), [garage](https://git.deuxfleurs.fr/Deuxfleurs/garage), [SeaweedFS](https://github.com/seaweedfs/seaweedfs)), ale wszystkie one wymagają opracowania oddzielnej warstwy usług, co czyni je bezużytecznymi jako samodzielne usługi i trudniejszymi do wdrożenia dla użytkowników, którzy chcą samodzielnie hostować usługę transferu plików. Jako uwaga boczna, rozwiązanie, które opracowaliśmy, może nadal być skomponowane z pamięcią masową zgodną z S3 dla serwerów o większej pojemności z pewnymi kompromisami w zakresie prywatności / wydajności.

Przyjrzeliśmy się również kilku niezależnym implementacjom udostępniania plików, z pewnymi protokołami ad-hoc (np. [ceph](https://github.com/ceph/ceph) i [lufi](https://framagit.org/fiat-tux/hat-softwares/lufi)), ale żadna z nich nie wydawała się wystarczająco dojrzała, a także nie tak prywatna, jak byśmy chcieli.

Tak więc po wielu poszukiwaniach zdecydowaliśmy się zaprojektować i zaimplementować nowy protokół do przesyłania plików, który zarówno rozwiązywałby powyższe problemy, jak i zapewniał wyższy poziom prywatności metadanych niż jakikolwiek inny protokół do przesyłania plików.

## Czym jest XFTP i jak działa?

```
           Nadawca                     Internet             Przekaźniki XFTP            Internet           Odbiorca
----------------------------   |   -----------------   |   -------------------    |   ------------   |   ------------
                               |                       |   (może być samodziel-   |                  |
                               |                       |    nie hostowany)        |                  |
                               |                       |       +------------+     |                  |
                  fragment 1  ------  HTTP2 po TLS ------      | Przekaźnik |    ---- HTTP2 / TLS -----   fragment 1
                |---> SimpleX File Transfer Protocol (XFTP) -> |    XFTP    | ->         XFTP          -------------->|
                |             ---------------------------      +-----------+     ----------------------               |
                |              |                       |                          |                  |                |
                |              |                       |                          |                  |                v
          +----------+         |                       |       +------------+     |                  |         +-------------+
          | Sending  | ch. 2  ------- HTTP2 / TLS -------      | Przekaźnik |    ---- HTTP2 / TLS -----  ch. 2 |   Klient    |
plik ---> |   XFTP   | ------>           XFTP            ----> |    XFTP    | --->        XFTP         ------> |  odbiorcy   | ---> plik
          |  Client  |        ---------------------------      +------------+    ----------------------        |    XFTP     |
          +----------+         |                       |                          |                  |         +-------------+
                |              |                       |                          |                  |                ^
                |              |                       |       +------------+     |                  |                |
                |             ------- HTTP2 / TLS -------      | Przekaźnik |    ---- HTTP2 / TLS -----               |
                |------------->           XFTP           ----> |    XFTP    | --->         XFTP        -------------->|
                  fragment N  ---------------------------      +------------+    ----------------------   fragment N
                               |                       |      (przechowuje        |                  |
                               |                       |      fragmenty plików)   |                  |
                               |                       |                          |                  |
```

XFTP to skrót od SimpleX File Transfer Protocol. Jego konstrukcja opiera się na tych samych ideach i ma niektóre cechy SimpleX Messaging Protocol, który jest używany w SimpleX Chat:

- odbiorca nie widzi adresu IP nadawcy, ponieważ fragmenty pliku (kawałki) są tymczasowo przechowywane na wielu przekaźnikach XFTP.
- plik może być wysyłany asynchronicznie, bez konieczności bycia online przez nadawcę, aby plik został odebrany.
- nie ma sieci peerów, którzy mogliby obserwować ten transfer - nadawca sam wybiera, które przekaźniki XFTP użyć, może też samodzielnie prowadzić własne.
- Przekaźniki XFTP nie mają żadnych metadanych pliku - widzą tylko poszczególne kawałki, a dostęp do każdego z nich jest autoryzowany anonimowymi danymi uwierzytelniającymi (przy użyciu podpisu kryptograficznego Edwards curve), które są losowe dla każdego kawałka.
- kawałki mają jeden z rozmiarów dozwolonych przez serwery - obecnie dopuszczamy kawałki 256kb, 1mb i 4mb, więc jeśli wyślesz, powiedzmy 1gb plik, do przekaźników XFTP będzie to wyglądało nie do odróżnienia od wysłania wielu małych plików, a oni będą wiedzieli, że kawałki są wysyłane przez tego samego użytkownika tylko poprzez informacje o transporcie, ale żaden z przekaźników nie zobaczy wszystkich kawałków. Ponadto, gdy ta funkcja będzie dostępna w aplikacjach mobilnych, możesz użyć izolacji transportowej na chunk, gdy każdy fragment pliku zostanie przesłany przez oddzielne połączenie TCP (i obwód Tor, jeśli używasz Tor) - CLI, który wydaliśmy, nie obsługuje jeszcze izolacji transportowej dla każdego fragmentu.
- każdy fragment może być pobrany przez wielu odbiorców, ale każdy odbiorca używa własnego klucza i identyfikatora fragmentu do autoryzacji dostępu, a fragment jest szyfrowany innym kluczem uzgodnionym przez efemeryczne klucze DH (NaCl crypto_box (Schemat szyfrowania uwierzytelnionego SalsaX20Poly1305) z współdzielonej tajemnicy pochodzącej z wymiany klucza Curve25519) w drodze z serwera do każdego odbiorcy. Protokół XFTP w rezultacie ma tę samą jakość co protokół SMP - nie ma wspólnych identyfikatorów i szyfrogramów pomiędzy wysyłanym i odbieranym ruchem wewnątrz połączenia TLS, więc nawet jeśli bezpieczeństwo protokółu TLS zostanie naruszone, komplikuje to ataki korelacji ruchu.
- Protokół XFTP wspiera również nadmiarowość - każdy fragment pliku może być wysłany przez wiele przekaźników, a odbiorca może wybrać ten, który jest dostępny. Wydany CLI nie obsługuje jednak nadmiarowości.
- plik jako całość jest szyfrowany losowym kluczem symetrycznym przy użyciu NaCl secret_box.

Skąd więc odbiorca miałby wiedzieć, skąd wziąć te wszystkie fragmenty pliku i jak złożyć je z powrotem w oryginalny plik? Zazwyczaj, gdy wysyłasz plik za pośrednictwem dowolnego serwisu wymiany plików, dostarcza on link, który możesz przekazać odbiorcy. Link pozwala na pobranie oryginalnego pliku, ale również dostarcza serwerowi wiele metadanych o pliku, które często zawierają nazwę pliku i dokładny rozmiar, a w wielu przypadkach serwer ma również dostęp do zawartości pliku.

Zamiast używania linku, protokół XFTP zawiera specjalny format "opisu pliku" - jest to mały plik tekstowy zawierający lokalizacje, klucze dostępu i skróty dla wszystkich fragmentów pliku, a także klucz szyfrowania i skrót (SHA512) dla całego pliku. Ten opis pliku nie zawiera oryginalnej nazwy pliku ani dokładnego rozmiaru pliku, więc jeśli jest używany po wygaśnięciu fragmentów pliku lub usunięciu ich z przekaźników XFTP, informacje te nie są dostępne.

CLI generuje osobny opis pliku dla każdego zamierzonego odbiorcy - musisz określić, ile osób ma mieć możliwość otrzymania tego pliku. Możesz określić większą liczbę odbiorców, aby uniknąć ujawnienia rzeczywistej liczby odbiorców z przekaźników XFTP. Aplikacje mobilne, gdy ten protokół jest zintegrowany, zrobią to automatycznie, wybierając dużą losową liczbę możliwych odbiorców, więc podczas gdy przekaźniki będą w stanie zaobserwować, ile osób w danym czasie pobrało plik, nie będą wiedzieć, ilu zamierzonych odbiorców miałeś - wysyłanie do grupy 10 osób i do 1 odbiorcy może wyglądać tak samo dla przekaźników.

Opis pliku jest plikiem wrażliwym na bezpieczeństwo, zawierającym klucze prywatne i adresy fragmentów niezbędne do odbioru całego pliku, a także klucz symetryczny do odszyfrowania pliku. Dlatego do wysłania opisu pliku należy użyć bezpiecznego kanału - np. można go wysłać przez SimpleX Chat. Jednak po pobraniu pliku przez odbiorcę, CLI unieważnia adresy fragmentów pliku na przekaźnikach i ten sam opis pliku nie może być ponownie użyty do pobrania pliku.

## Co dalej?

Wydaliśmy i wdrożyliśmy kilka przekaźników XFTP, z którymi można eksperymentować (są one hardcodowane w XFTP CLI), a także można wdrożyć własne przekaźniki albo z [binarki do pobrania](https://github.com/simplex-chat/simplexmq/releases/tag/v5.0.0-beta.3) albo kompilując [kod źródłowy](https://github.com/simplex-chat/simplexmq). Wydaliśmy również XFTP CLI - jest on dostępny w tym samym wydaniu.

Obecnie integrujemy wsparcie dla wysyłania dużych plików za pomocą protokołu XFTP w klientach SimpleX Chat. SimpleX Chat v5.0 będzie miał wsparcie dla odbierania plików wysyłanych za pomocą protokołu XFTP (będzie można wysłać opis pliku za pomocą aplikacji SimpleX Chat CLI, dzięki czemu aplikacje mobilne będą mogły odbierać je jak normalne pliki, tylko znacznie szybciej), a v5.1 będzie w pełni wspierać wysyłanie dużych plików (do 1gb) w aplikacjach mobilnych.

Opublikujemy również formalną specyfikację protokołu XFTP oraz przegląd jego właściwości bezpieczeństwa i modelu zagrożeń. Na razie możesz dowiedzieć się więcej o projekcie protokołu i motywach jego działania z tego wewnętrznego [Protokół XFTP RFC](https://github.com/simplex-chat/simplexmq/blob/stable/rfcs/2022-12-26-simplex-file-transfer.md).

Używanie i wysyłanie plików za pomocą dostępnego XFTP CLI bardzo pomoże nam ustabilizować zarówno protokół jak i implementacje. To, co naprawdę podoba nam się w tym projekcie, to fakt, że jest on całkowicie niezależny od SimpleX Chat - możesz go używać samodzielnie, wysyłając pliki i przekazując opisy plików do swoich kontaktów przez dowolny inny komunikator - np. przez Signal, - bez możliwości zaobserwowania przez ten komunikator, że w rzeczywistości wysyłasz duży plik.

Nie zdecydowaliśmy jeszcze, czy zrobimy osobny audyt bezpieczeństwa implementacji XFTP, czy połączymy go z kolejnym audytem bezpieczeństwa SimpleX Chat. Ta druga opcja wydaje się bardziej prawdopodobna, ponieważ XFTP wykorzystuje te same prymitywy kryptograficzne, które zostały zweryfikowane podczas [oceny bezpieczeństwa SimpleX Chat przez Trail of Bits](./20221108-simplex-chat-v4.2-security-audit-new-website.md) w listopadzie 2022 roku.

## Platforma SimpleX

Kilka linków odpowiadających na najczęstsze pytania:

[Jak SimpleX może dostarczać wiadomości bez identyfikatorów użytkowników](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/blog/20220511-simplex-chat-v2-images-files.md#pierwsza-platforma-komunikacyjna-która-nie-posiada-żadnych-identyfikatorów-użytkowników).

[Jakie są zagrożenia związane z posiadaniem identyfikatorów przypisanych do użytkowników](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/blog/20220711-simplex-chat-v3-released-ios-notifications-audio-video-calls-database-export-import-protocol-improvements.md#dlaczego-posiadanie-identyfikatorów-użytkowników-jest-złe-dla-użytkowników).

[Szczegóły techniczne i ograniczenia](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/blog/20220723-simplex-chat-v3.1-tor-groups-efficiency.md#prywatność-szczegóły-techniczne-i-ograniczenia).

[Jak SimpleX różni się od Session, Matrix, Signal, etc.](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/README.md#najczęściej-zadawane-pytania).

Zapraszamy również na naszą [stronę internetową](https://simplex.chat/pl).

## Wspomóż nas darowiznami

Ogromne podziękowania dla wszystkich, którzy przekazali darowiznę na rzecz SimpleX Chat!

Stawiamy na pierwszym miejscu prywatność i bezpieczeństwo użytkowników - bez Waszego wsparcia byłoby to niemożliwe.

Zobowiązujemy się, że protokoły SimpleX są i pozostaną otwarte, w domenie publicznej, więc każdy może budować przyszłe implementacje klientów i serwerów. Budujemy platformę SimpleX w oparciu o te same zasady, co w przypadku poczty elektronicznej i Internetu, ale znacznie bardziej prywatną i bezpieczną.

Wasze datki pomagają nam zebrać więcej funduszy - każda kwota, nawet cena filiżanki kawy, robi dla nas wielką różnicę.

Zobacz [ten dział](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/README.md#pomóż-w-tłumaczeniu-simplex-chat), aby poznać sposoby przekazywania darowizn.

Dziękuję,

Evgeny

Założyciel SimpleX Chat
