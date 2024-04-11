---
layout: layouts/article.html
title: "Security assessment by Trail of Bits, the new website and v4.2 released"
date: 2022-11-08
image: images/20221108-trail-of-bits.jpg
previewBody: blog_previews/20221108.html
permalink: "/blog/20221108-simplex-chat-v4.2-security-audit-new-website.html"
---

# Ocena bezpieczeństwa przeprowadzona przez Trail of Bits, nowa strona internetowa i wydanie v4.2

**Opublikowano:** 8 listopada, 2022

# Ocena bezpieczeństwa przez Trail of Bits

<img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20221108-trail-of-bits.jpg" width=240>

Kiedy po raz pierwszy uruchomiliśmy aplikację w marcu, odpowiedź na Reddicie brzmiała: _"Czy zostałeś skontrolowany, czy powinniśmy cię po prostu zignorować?"_.

Mamy rosnącą liczbę entuzjastów korzystających z SimpleX Chat, którzy mogą zaakceptować ryzyko związane z bezpieczeństwem nieaudytowanego systemu, ale użytkownicy, którzy zależą od ich bezpieczeństwa, cierpliwie czekali, aż niektórzy niezależni eksperci przejrzą naszą bazę kodową.

[Trail of Bits](https://www.trailofbits.com/about), amerykańska firma konsultingowa zajmująca się bezpieczeństwem i technologią, której klientami są duże firmy technologiczne, agencje rządowe i duże projekty blockchain, zleciła 2 inżynierom przeglądanie SimpleX Chat przez tydzień kalendarzowy, a w szczególności [biblioteki simplexmq](https://github.com/simplex-chat/simplexmq), która jest odpowiedzialna za całą kryptografię i sieć platformy SimpleX.

Zidentyfikowano 2 błędy o średniej i 2 niskiej ważności, z których wszystkie wymagają ataku o wysokiej trudności do wykorzystania - atakujący musiałby mieć uprzywilejowany dostęp do systemu, może potrzebować znajomości skomplikowanych szczegółów technicznych lub musi odkryć inne słabości, aby je wykorzystać. 3 z tych błędów zostały już poprawione w wersji 4.2.

W trakcie audytu, Trail of Bits ocenił dojrzałość [biblioteki simplexmq](https://github.com/simplex-chat/simplexmq) w ośmiu kategoriach i uznał pięć z nich za silne lub zadowalające.

Poniżej wyjaśniamy nasze rozumienie problemów, jak również poprawki wdrożone przez zespół SimpleX Chat po zakończeniu projektu. Pełny przegląd bezpieczeństwa jest dostępny na stronie [Trail of Bits publications](https://github.com/trailofbits/publications/blob/master/reviews/SimpleXChat.pdf).

Jesteśmy ogromnie wdzięczni Trail of Bits i ich inżynierom za pracę, którą wykonali, pomagając nam zidentyfikować te błędy i wspierając trwające wysiłki, aby uczynić Simple Chat bardziej bezpiecznym.

### Problemy średniej wagi

#### Wymiana klucza X3DH dla protokołu double ratchet

Popełniliśmy błąd w implementacji wymiany klucza X3DH - funkcja wyprowadzania klucza nie została zastosowana do wyniku konkatenacji trzech operacji DH. Atak wykorzystujący ten błąd jest bardzo trudny, ponieważ wymagałby skompromitowania jednego z kluczy prywatnych wygenerowanych przez klientów, a ponadto wpływałby na tajemnicę transmisji tylko do momentu, gdy dojdzie do włamania (po wysłaniu przez obie strony kilku wiadomości).

Należy pamiętać, że SimpleX nie wykonuje X3DH z długoterminowymi kluczami tożsamości, ponieważ protokół SimpleX nie polega na długoterminowych kluczach do identyfikacji urządzeń klienckich. Dlatego wpływ kompromitacji klucza będzie mniej poważny, ponieważ wpłynie tylko na tajność połączenia, w którym klucz został naruszony.

Ten problem został naprawiony w wersji 4.2 w [tym PR](https://github.com/simplex-chat/simplexmq/pull/548/files) i jeśli obaj klienci zostaną zaktualizowani, wymiana klucza nie będzie miała tej luki. Ponadto wcześniej utworzone połączenia powinny być bezpieczne, dopóki obie strony wysyłają wiadomości, ale jeśli uważasz, że twój klucz prywatny (klucze) mógł zostać naruszony (na przykład, jeśli używałeś SimpleX Chat, zanim dodaliśmy szyfrowanie bazy danych), zalecamy utworzenie nowych połączeń z twoimi kontaktami, przynajmniej z tymi krytycznymi dla bezpieczeństwa. Zwykłe obracanie kolejki połączeń (ręczne obracanie kolejki zostało dodane w wersji 4.2) nie będzie wystarczające, ponieważ to obracanie nie reinicjalizuje ratchets - jest to coś, co dodamy w przyszłości.

#### Klucze są przechowywane w niepodpiętej pamięci i nie są czyszczone po zakończeniu ich życia

Problem polega na tym, że pamięć z kluczami kryptograficznymi może zostać zamieniona na pamięć masową i potencjalnie dostępna dla napastnika, który ma dostęp do urządzenia na poziomie root (lub poziom dostępu wymagany do dostępu do pliku wymiany aplikacji). Jeśli więc uruchamiasz SimpleX Chat na desktopie, możesz zwiększyć jego bezpieczeństwo, uruchamiając go w odizolowanym kontenerze.

Naszym zdaniem, na mobilnych systemach operacyjnych jest to mniej poważne, ponieważ każda aplikacja działa już w swoim własnym kontenerze, a aplikacje nie dzielą się dostępem do swoich obszarów wymiany (np. w Androidzie swap to [skompresowany obszar w pamięci RAM](https://developer.android.com/topic/performance/memory-management) niedostępny dla innych aplikacji).

Aby wykorzystać ten problem, napastnik musi mieć uprzywilejowany dostęp do urządzenia. Ponadto, uważamy, że [Haskell generational garbage collection](https://www.microsoft.com/en-us/research/wp-content/uploads/1993/01/gen-gc-for-haskell.pdf) sprawia, że czas życia nieużywanej pamięci jest niższy niż w innych językach.

Zajmiemy się tym problemem w najbliższej przyszłości, prawdopodobnie poprzez użycie biblioteki [secure-memory](https://hackage.haskell.org/package/secure-memory-0.0.0.2) stworzonej przez Kirilla Elagina, inżyniera z Serokell, lub innego podobnego podejścia.

### Problemy o niskiej dotkliwości

#### Funkcja wypełniania ciągów znaków jest niepoprawna dla długich wiadomości, a funkcja usuwania wypełniania ciągów znaków rzuca wyjątek dla krótkich wiadomości

Oba te problemy zostały naprawione w 4.2 w [tym PR](https://github.com/simplex-chat/simplexmq/pull/547/files), wraz z dodatkowymi testami jednostkowymi, a także zwalidowaliśmy, że nawet przed poprawką ciągi, które spowodowałyby taki wyjątek, nigdy nie były przekazywane do tej funkcji - Zespół SimpleX Chat nie mógł znaleźć możliwości ataku, który zakończyłby się sukcesem z powodu tego problemu.

### Co dalej

Istnieją obszary SimpleX Chat, które były poza zakresem tego przeglądu, a konkretnie:

- implementacja protokołu czatu i mobilne interfejsy użytkownika, ponieważ nie zawierają one kryptografii sieci (z wyjątkiem aplikacji na Androida przechowującej zaszyfrowane frazy hasła bazy danych i wymiany/szyfrowania klucza dla połączeń WebRTC).
- serwer powiadomień push, który jest używany przez klientów iOS.

Umówimy się na osobny przegląd tych obszarów.

## Nowa strona internetowa

Nasza [poprzednia strona](https://old-website.simplex.chat) powstała 2 lata temu, aby zaprezentować ideę SimpleX, nie było wtedy SimpleX Chat - mieliśmy wtedy tylko prototypową implementację serwera SimpleX Messaging Protocol.

Wiele osób powiedziało nam, że nasza strona nie wyjaśniała wystarczająco dobrze, dla kogo przeznaczony jest SimpleX Chat, jakie problemy rozwiązuje i czym różni się od alternatyw. Tak więc, chociaż uwielbiamy skupiać się na aplikacji czatu, postanowiliśmy zrobić nową.

Mamy nadzieję, że nasza [nowa strona](https://simplex.chat/pl) lepiej odpowiada na te pytania. Jeśli uważasz, że coś powinno zostać dodane/usunięte/zmienione - prosimy daj nam znać. Dziękujemy!

## SimpleX Chat v4.2 wydany!

Nowości w tym wydaniu:

- poprawiono 3 błędy z audytu bezpieczeństwa!
- linki do grup - administratorzy grup mogą tworzyć linki dla nowych członków, aby się do nich przyłączyć
- automatyczne akceptowanie próśb o kontakt + konfiguracja czy akceptować incognito i wiadomość powitalną
- drobne rzeczy: zmiana roli członka grupy, oznaczanie czatu jako nieprzeczytanego, wysyłanie naklejek i GIF-ów z klawiatur Android.

Funkcje beta (włącz narzędzia deweloperskie, aby je wypróbować):

- ręcznie przełączyć kontakt lub członka na inny adres / serwer (musi być obsługiwany przez obu klientów, aby działał)
- odbieraj pliki szybciej (włącz to w ustawieniach Prywatność i bezpieczeństwo)

### Linki do grup

<img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20221108-group1.png" width="288"> &nbsp;&nbsp; <img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20221108-group2.png" width="288">

Wielu użytkowników prosiło o możliwość dołączenia do grupy poprzez link. Ponieważ grupy SimpleX Chat są w pełni zdecentralizowane i nie ma stanu po stronie serwera, dołączenie za pośrednictwem tych linków wymaga udziału twórcy linku, który musi być online, aby zaakceptować żądanie dołączenia do grupy.

Sposób działania pod maską jest podobny do tego, jak działają adresy kontaktowe:

1. Administrator lub właściciel grupy tworzy długoterminowy adres, który jest technicznie taki sam jak adres użytkownika, ale jest związany z konkretną grupą.
2. Użytkownik, który dołącza do grupy, może zidentyfikować, że ten link należy do jakiejś grupy przez dodatkowy fragment danych w linku - `{"type": "group", "groupLinkId": "some random string"}`. ID w tym linku nie reprezentuje tożsamości grupy, za każdym razem, gdy dowolny użytkownik stworzy nowy link dla tej samej grupy, to ID będzie inne. Ten identyfikator jest używany przez klienta dołączającego do grupy, aby zidentyfikować grupę i automatycznie zaakceptować zaproszenie, gdy zostanie odebrane.
3. Kiedy administrator otrzymuje żądanie połączenia, automatycznie akceptuje je i wysyła link zaproszenia do dołączenia do grupy.
4. Dołączający użytkownik porównuje ID w zaproszeniu z ID w linku i jeśli się zgadzają - automatycznie akceptuje zaproszenie.

Następnie działa to tak samo jak w przypadku dołączenia poprzez ręczne zaproszenie - dołączający użytkownik będzie nawiązywał połączenie ze wszystkimi istniejącymi członkami, aby móc wysyłać wiadomości do grupy.

Połączenie może być utworzone poprzez stronę grupy, jak pokazano na obrazku.

Mamy kilka grup, do których możesz dołączyć, aby zadać jakiekolwiek pytania lub po prostu przetestować aplikację:

- [#SimpleX-Group](https://simplex.chat/pl/contact#/?v=1-2&smp=smp%3A%2F%2Fu2dS9sG8nMNURyZwqASV4yROM28Er0luVTx5X1CsMrU%3D%40smp4.simplex.im%2FWHV0YU1sYlU7NqiEHkHDB6gxO1ofTync%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEAWbebOqVYuBXaiqHcXYjEHCpYi6VzDlu6CVaijDTmsQU%253D%26srv%3Do5vmywmrnaxalvz6wi3zicyftgio6psuvyniis6gco6bp6ekl4cqj4id.onion&data=%7B%22type%22%3A%22group%22%2C%22groupLinkId%22%3A%22mL-7Divb94GGmGmRBef5Dg%3D%3D%22%7D): grupa ogólna z ponad 100 członkami, gdzie można zadawać wszelkie pytania.

- Kilka grup według krajów/języków: [\#SimpleX-DE](https://simplex.chat/pl/contact#/?v=1-2&smp=smp%3A%2F%2Fhpq7_4gGJiilmz5Rf-CswuU5kZGkm_zOIooSw6yALRg%3D%40smp5.simplex.im%2FmIorjTDPG24jdLKXwutS6o9hdQQRZwfQ%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEA9N0BZaECrAw3we3S1Wq4QO7NERBuPt9447immrB50wo%253D%26srv%3Djjbyvoemxysm7qxap7m5d5m35jzv5qq6gnlv7s4rsn7tdwwmuqciwpid.onion&data=%7B%22type%22%3A%22group%22%2C%22groupLinkId%22%3A%22S8aISlOgkTMytSox9gAM2Q%3D%3D%22%7D) (Niemiecki), [\#SimpleX-US](https://simplex.chat/pl/contact#/?v=1-2&smp=smp%3A%2F%2Fu2dS9sG8nMNURyZwqASV4yROM28Er0luVTx5X1CsMrU%3D%40smp4.simplex.im%2FlTWmQplLEaoJyHnEL1-B3f2PtDsikcTs%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEA-hMBlsQjNxK2vaVhqW_UyAVtuoYqgYTigK4B9dJ9CGc%253D%26srv%3Do5vmywmrnaxalvz6wi3zicyftgio6psuvyniis6gco6bp6ekl4cqj4id.onion&data=%7B%22type%22%3A%22group%22%2C%22groupLinkId%22%3A%22G0UtRHIn0TmPoo08h_cbTA%3D%3D%22%7D) (US/Angielski), [\#SimpleX-France](https://simplex.chat/pl/contact#/?v=1-2&smp=smp%3A%2F%2Fu2dS9sG8nMNURyZwqASV4yROM28Er0luVTx5X1CsMrU%3D%40smp4.simplex.im%2F11r6XyjwVMj0WDIUMbmNDXO996M_EN_1%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEAXDmc2Lrj9WQOjEcWa0DeQHF3HcYOp9b68s8M_BJ7gEk%253D%26srv%3Do5vmywmrnaxalvz6wi3zicyftgio6psuvyniis6gco6bp6ekl4cqj4id.onion&data=%7B%22type%22%3A%22group%22%2C%22groupLinkId%22%3A%22EZCeSYpeIBkaQwCcpcF00w%3D%3D%22%7D) (Francuski), [\#SimpleX-RU](https://simplex.chat/pl/contact#/?v=1-2&smp=smp%3A%2F%2Fhpq7_4gGJiilmz5Rf-CswuU5kZGkm_zOIooSw6yALRg%3D%40smp5.simplex.im%2FZSYM278L5WoZiApx3925EAjSXcsAVNVu%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEA7RJ2wfT8zdfOLyE5OtWLEAPowj-q6F2HB0ExbATw8Gk%253D%26srv%3Djjbyvoemxysm7qxap7m5d5m35jzv5qq6gnlv7s4rsn7tdwwmuqciwpid.onion&data=%7B%22type%22%3A%22group%22%2C%22groupLinkId%22%3A%22fsVoklNGptt7n-droqJYUQ%3D%3D%22%7D) (Rosyjski), [#SimpleX-NL](https://simplex.chat/pl/contact#/?v=1-2&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2FmP0LbswSbfxoVkkxiWE2NYnBCgZ9Snvj%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEAVwZuSsw4Mf52EaBNdNI3RebsLm0jg65ZIkcmH9E5uy8%253D%26srv%3Dbylepyau3ty4czmn77q4fglvperknl4bi2eb2fdy2bh4jxtf32kf73yd.onion&data=%7B%22type%22%3A%22group%22%2C%22groupLinkId%22%3A%22M9xIULUNZx51Wsa5Kdb0Sg%3D%3D%22%7D) (Niderlandy/Holenderski), [#SimpleX-IT](https://simplex.chat/pl/contact#/?v=1-2&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2FaZ_wjh6QAYHB-LjyGtp8bllkzoq880u-%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEA-_Wulzc3j16i7t77XJ5wgwxeW8_Ea8GxetMo7K4MgjI%253D%26srv%3Dbylepyau3ty4czmn77q4fglvperknl4bi2eb2fdy2bh4jxtf32kf73yd.onion&data=%7B%22type%22%3A%22group%22%2C%22groupLinkId%22%3A%22QWmXdrFzIeMd2OoEPMFkBQ%3D%3D%22%7D) (Włoski).

Możesz dołączyć do tych grup albo otwierając te linki w aplikacji, albo otwierając je w przeglądarce na pulpicie i skanując kod QR.

Daj mi znać, jeśli chciałbyś dodać jakieś inne kraje do listy. Dołącz do nich za pośrednictwem aplikacji, aby dzielić się tym, co się dzieje i zadawać pytania!

### Automatyczne przyjmowanie próśb o kontakt

<img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20221108-address1.png" width="288"> &nbsp;&nbsp; <img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20221108-address2.png" width="288">.

Kiedy ktoś łączy się z Tobą poprzez Twój adres długoterminowy musisz ręcznie zaakceptować prośbę o połączenie (pokazuje się ona niebieskim kolorem na liście czatów). Funkcja, którą dodaliśmy w tym wydaniu, pozwala skonfigurować aplikację do automatycznego akceptowania próśb o kontakt, a także wybrać, czy ten kontakt powinien otrzymać twój główny profil lub losowy profil incognito (niezależnie od bieżących ustawień aplikacji), a także dodać opcjonalną wiadomość z automatyczną odpowiedzią.

Ta funkcja jest przydatna, jeśli publikujesz swój adres na swojej stronie internetowej lub profilu społecznościowym i nie chcesz przesiewać osób, które chcą się z Tobą połączyć. Możesz chcieć wysłać standardową wiadomość powitalną, na przykład jeśli jest to sklep internetowy i musisz podzielić się wszelkimi informacjami z każdym, kto się z Tobą skontaktuje.

Nasze konto @simplex, z którym łączysz się, gdy wybierzesz "Czat z deweloperami" w aplikacji, używało tej funkcji przez długi czas, a teraz jest dostępna dla użytkowników aplikacji mobilnej.

### Kilka małych rzeczy.

1. Zmiana roli członka grupy to bardzo podstawowa funkcja, ale została dodana dopiero w tym wydaniu.

2. Możesz teraz oznaczyć konwersację jako nieprzeczytaną, na przykład jeśli przypadkowo oznaczyłeś wszystkie wiadomości jako przeczytane i chcesz przejrzeć je później.

3. Wysyłaj naklejki i GIF-y z klawiatur Androida, a na koniec rozwiązano też błąd z przyciskiem backspace.

### Przełącz adres odbioru (BETA)

<img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20221108-switch-address.png" width="288">

Aby ręcznie przełączyć dowolny z Twoich kontaktów (lub członka grupy na nowy adres serwera), włącz narzędzia dla programistów i wybierz "Przełącz adres odbioru" na stronie kontaktu. Tak długo, jak uruchamiają nową wersję aplikacji i są online, przełączenie powinno zająć tylko kilka sekund.

Jest to duża poprawa prywatności metadanych protokołów SimpleX, ponieważ wcześniej, o ile nie mieliśmy identyfikatorów użytkowników, to identyfikatory parami kolejek wiadomości używanych do dostarczania wiadomości były używane tak długo, jak długo istniał kontakt. Teraz te identyfikatory są tymczasowe, a w niedalekiej przyszłości dodamy automatyczną rotację tych adresów doręczeń.

Jest to również przydatne, gdy chcesz przenieść dostarczanie wiadomości na inny serwer, na przykład, jeśli używałeś domyślnych serwerów SimpleX Chat, a teraz chcesz samodzielnie hostować własne. A może trzeba zmienić adres swojego serwera. Wcześniej wymagałoby to tworzenia nowych kontaktów i utraty historii rozmów, a teraz wystarczy zmienić konfigurację serwera w aplikacji, a gdy zmiana adresu zostanie wywołana (obecnie tylko ręcznie, a w najbliższej przyszłości - automatycznie), Twoje kontakty zostaną zmigrowane na nowy serwer, bez robienia czegokolwiek - wymaga to jedynie wysłania przez każdą ze stron 2 wiadomości w celu wynegocjowania ponownego połączenia, a także rotacji kluczy szyfrujących używanych do zewnętrznej warstwy szyfrowania E2E.

### Szybsze odbieranie obrazów i małych plików (BETA)

<img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20221108-faster-images.png" width="288">

Od wersji 4.2 wszystkie pliki mniejsze niż ~92kb (równe 6 blokom wiadomości) będą wysyłane w tym samym połączeniu, w którym prowadzisz czat, a pliki mniejsze niż ~231kb (limit rozmiaru obrazu) mogą być również opcjonalnie odbierane przez to samo połączenie - to ostatnie wymaga włączenia opcji "Transferuj obrazy szybciej" w ustawieniach prywatności i bezpieczeństwa (będzie ona dostępna po włączeniu narzędzi dla deweloperów). Są dwa powody, dla których nie jest to jeszcze domyślnie włączone: 1) chcieliśmy się upewnić, że jest to stabilne; 2) istnieje niewielki wpływ na prywatność metadanych, gdy ruch w tym samym połączeniu, w którym prowadzisz główną rozmowę, jest zbyt duży.

Ta funkcjonalność została stworzona dla przyszłych wiadomości głosowych, ponieważ muszą one być wysyłane bez akceptacji, aby odbiorcy mogli ich słuchać, nawet gdy nadawca jest offline.

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
- Portfel Monero: 8568eeVjaJ1RQ65ZUn9PRQ8ENtqeX9VVhcCYYhnVLxhV4JtBqw42so2VEUDQZNkFfsH5sXCuV7FN8VhRQ21DkNibTZP57Qt
- Portfel Bitcoin: 1bpefFkzuRoMY3ZuBbZNZxycbg7NYPYTG
- proszę dać nam znać, za pośrednictwem wydania GitHub lub czatu, jeśli chcesz przekazać darowiznę w jakiejś innej kryptowalucie - dodamy adres do listy.

Dziękujemy,

Evgeny

Założyciel SimpleX Chat
