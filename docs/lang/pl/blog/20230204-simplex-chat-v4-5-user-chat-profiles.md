---
layout: layouts/article.html
title: "SimpleX Chat v4.5 released – with multiple chat profiles, message draft, transport isolation and Italian language!"
date: 2023-02-04
image: images/20230204-profiles2.png
imageBottom: true
previewBody: blog_previews/20230204.html
permalink: "/blog/20230204-simplex-chat-v4-5-user-chat-profiles.html"
---

# SimpleX Chat v4.5 wydany - z wieloma profilami użytkowników, szkicami wiadomości, izolacją transportu i włoskim interfejsem!

**Opublikowano:** 4 luty, 2023

## Co nowego w v4.5

- [wiele profili czatu](#wiele-profili-czatu).
- [projekt wiadomości](#projekt-wiadomości).
- [izolacja transportu](#izolacja-transportu).
- [zmniejszone zużycie baterii](#zmniejszone-zużycie-baterii).
- [prywatne nazwy plików](#prywatne-nazwy-plików).

Ponadto dodaliśmy [włoski interfejs](#włoski-interfejs), dzięki społeczności użytkowników i firmie Weblate!

### Wiele profili czatu

<img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20230204-profiles1.png" width="288"> &nbsp;&nbsp; <img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20230204-profiles2.png" width="288">

Tryb Incognito dodany w v3.2 pozwala na udostępnienie nowym kontaktom losowej nazwy profilu. Jest to dość popularne rozwiązanie - korzysta z niego ponad połowa użytkowników łączących się z naszym profilem pomocniczym. W przypadku, gdy chcesz tylko wyrzucić nazwę profilu, która nie dzieli żadnych informacji o Tobie, jest to wygodniejsze niż ręczne tworzenie nowego profilu dla każdego kontaktu.

Ale jest wiele przypadków, kiedy jednorazowy profil nie jest wystarczająco dobry i chcesz stworzyć osobne profile - jeden dla rodziny, inny do pracy, jeszcze jeden dla kontaktów online, które znają tylko Twój pseudonim, a nie prawdziwe imię i nazwisko.

Poprzednio było to możliwe tylko poprzez użycie oddzielnych baz danych czatu. Ta wersja dodaje menedżera profili czatu, który pozwala na tworzenie nieograniczonej liczby profili i bardzo szybkie ich przełączanie. Wszystkie te profile są połączone w tym samym czasie.

Gdy usuwasz niepotrzebny Ci już profil masz dwie możliwości - albo usunąć zarówno dane profilu na urządzeniu jak i wszelkie kolejki wiadomości, które ten profil utworzył na serwerach SMP (np. gdy ważniejsze jest pozostawienie jak najmniejszej ilości metadanych na serwerach), albo usunąć tylko dane profilu bez wykonywania żadnych żądań sieciowych (gdy ważniejsze jest szybkie usunięcie wszystkich danych z urządzenia).

Podczas gdy większość ustawień aplikacji dotyczy wszystkich profili, kilka ustawień jest specyficznych dla danego profilu.

Możesz oddzielnie ustawić, które serwery SMP są używane do tworzenia nowych kontaktów w każdym profilu (za pośrednictwem ustawień sieci). Uwaga: zmiana serwerów SMP w jednym profilu nie będzie miała wpływu na inne profile, a wszystkie nowe profile są tworzone ze wstępnie ustawionymi serwerami.

Możesz również osobno ustawić czas przechowywania wiadomości w każdym profilu (poprzez Ustawienia bazy danych).

Ponadto w każdym profilu można ustawić inny adres kontaktowy i inne preferencje dotyczące czatu.

Wszystkie inne ustawienia są używane dla wszystkich profili. Chociaż możesz chcieć mieć różne ustawienia dla różnych poziomów bezpieczeństwa, w tym przypadku znacznie lepiej jest używać różnych urządzeń.

### Projekt wiadomości

<img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20230204-draft1.png" width="288"> &nbsp;&nbsp; <img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20230204-draft2.png" width="288">

Wcześniej, jeśli zamknąłeś konwersację z niewysłaną wiadomością, ta wiadomość znikała. Chociaż może to być lepsze dla prywatności, istnieje kilka scenariuszy, w których jest to bardzo niewygodne:

- musisz skomponować wiadomość z kilku części, które kopiujesz z innych rozmów.
- przychodzą jakieś wiadomości, na które trzeba pilnie odpowiedzieć, więc można zostawić niedokończoną wiadomość, aby wrócić do niej później.

Oba te rozwiązania są teraz możliwe - napisana przez Ciebie wiadomość, wraz z wszelkimi załącznikami, a nawet nagraną przez Ciebie wiadomością głosową, pozostanie dostępna jako wersja robocza, dopóki nie zamkniesz aplikacji lub nie pozostawisz niedokończonej innej wiadomości - wprawdzie możesz wysłać wiadomość na innym czacie bez utraty aktualnej wersji roboczej, ale w danym momencie może być tylko jedna wersja robocza.

### Izolacja transportu

<img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20230204-transport.png" width="288">

Nawet jeśli utworzysz różne profile czatu, nadal łączysz się ze swoimi kontaktami za pośrednictwem tego samego urządzenia. Izolacja transportu domyślnie sprawia, że połączenia należące do tego samego profilu używają różnych sesji TCP, więc choć serwer widzi ten sam adres IP, nie widzi go jako tego samego połączenia klienckiego. Jeśli łączysz się przez Tor używając SOCKS proxy (np. aplikacja Orbot na Androida) nie tylko aplikacja będzie używała różnych sesji TCP, ale także będzie używała oddzielnych obwodów Tor dla połączeń z różnych profili, co uniemożliwi serwerom i obserwatorom sieci widzenie tego jako ruchu pochodzącego z tego samego urządzenia.

Istnieje dodatkowa opcja BETA, aby użyć osobnego połączenia transportowego dla każdego kontaktu i połączenia członków grupy, które masz - jest dostępna w ustawieniach sieci, jeśli włączysz narzędzia deweloperskie. W przypadku, gdy masz dużą liczbę kontaktów lub uczestniczysz w dużych grupach we wszystkich profilach w aplikacji, nie powinieneś używać tej opcji, ponieważ może ona tworzyć dużo ruchu, a także może przekroczyć limit dla gniazd TCP. Również tworzenie wielu oddzielnych obwodów Tor może być powolne. Będziemy testować przy jakiej liczbie połączeń ta opcja zaczyna szwankować i do czasu gdy będzie dostępna bez narzędzi dev dodamy jakieś limity.

### Zmniejszone zużycie baterii

Zużycie baterii jest jedną z największych skarg użytkowników SimpleX Chat. Główną przyczyną nadmiernego ruchu są nieefektywne strategie ponawiania operacji sieciowych w tych przypadkach:

- pojemność kolejki wiadomości została przekroczona - aplikacja będzie ponawiać próby wysłania wiadomości, aż pojawi się pojemność.
- serwer nie jest dostępny, np. jeśli łączysz się z kimś przez jego serwer, a on później go wyłącza. Ten przypadek powoduje znacznie mniejszy ruch niż pierwszy.

Ta wersja sprawia, że retries z powodu przekroczenia pojemności kolejki są 10x rzadsze i dodaje rozszerzenie protokołu SMP, które pozwoli jeszcze bardziej zmniejszyć retries w następnej wersji. Działa to w następujący sposób:
- gdy nadawca napotka błąd "przekroczenie limitu kolejki", przestaje próbować wysłać wiadomość.
- serwer również odnotowuje ten błąd.
- gdy odbiorca otrzyma wszystkie wiadomości z tej kolejki, serwer powiadomi odbiorcę, że nadawca miał błąd "przekroczenie limitu kolejki".
- odbiorca wyśle wtedy specjalną wiadomość do klienta nadawcy, aby poinstruować go, że może wznowić dostarczanie wiadomości.
- Nadawca nadal może okazjonalnie próbować wysłać wiadomość, np. raz na godzinę, ale nie będzie musiał ponawiać próby co kilka minut, jak to ma miejsce obecnie.

Zmniejszenie zużycia baterii jest naszym dużym priorytetem - oczekujemy, że w ciągu najbliższych kilku miesięcy zmniejszymy je co najmniej 2-3x.

### Prywatne nazwy plików

Kiedy wysyłasz obraz lub wiadomość głosową, są one wysyłane jako pliki z nazwami plików zawierającymi znacznik czasu. Nie widzieliśmy tego jako problemu, ponieważ pliki są wysyłane razem z wiadomością, a wiadomości i tak mają znaczniki czasu po stronie serwera. Ale użytkownicy zwrócili uwagę, że znacznik czasu, którego używaliśmy, był w lokalnej strefie czasowej, a zatem przeciekał kraj (lub kontynent), w którym znajduje się użytkownik. Prostym obejściem było zresetowanie strefy czasowej do UTC, i to jest to, co większość użytkowników, którzy muszą chronić swoją lokalizację, i tak robi. Ale ta wersja rozwiązuje to - te nazwy plików zawierają teraz strefę czasową UTC - żadne obejścia nie są potrzebne.

### Włoski interfejs

Dzięki społeczności naszych użytkowników oraz firmie Weblate udostępniającej darmowy plan hostingowy dla tłumaczeń SimpleX Chat możemy teraz obsługiwać więcej języków w interfejsie - ta wersja dodaje włoski, a wiele innych jest w trakcie realizacji - chiński, japoński, czeski, holenderski itd.

Możesz też [przyczynić się do przetłumaczenia](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/README.md#pomóż-w-tłumaczeniu-simplex-chat) aplikacji na swój język!

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
