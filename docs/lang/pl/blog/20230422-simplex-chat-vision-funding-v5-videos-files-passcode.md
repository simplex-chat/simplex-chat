---
layout: layouts/article.html
title: "SimpleX Chat: vision and funding, v5.0 released with videos and files up to 1gb"
date: 2023-04-22
image: images/20230422-video.png
imageBottom: true
previewBody: blog_previews/20230422.html
permalink: "/blog/20230422-simplex-chat-vision-funding-v5-videos-files-passcode.html"
---

# SimpleX Chat: wizja i finansowanie, v5.0 wydany z filmami i plikami do 1gb.

**Opublikowano:** 22 kwietnia 2023 r.

SimpleX Chat wizja i finansowanie:
- [dlaczego jest to firma komercyjna?](#dlaczego-jest-to-firma-komercyjna)
- [jak jest finansowana i jaki jest model biznesowy?](#jak-jest-finansowana-i-jaki-jest-model-biznesowy)
- [co dalej?](#co-dalej)

Co nowego w v5.0:
- [wysyłaj filmy i pliki do 1gb](#wysyłaj-filmy-i-pliki-do-1gb)
- [hasło aplikacji niezależne od uwierzytelniania systemu](#haslo-aplikacji-niezależne-od-uwierzytelniania-systemu)
- [usprawnienia sieci](#usprawnienia-sieci)

Dodaliśmy również polski język interfejsu, dzięki [społeczności użytkowników i firmie Weblate](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/README.md#pomóż-w-tłumaczeniu-simplex-chat).

Aplikacje SimpleX Chat są teraz dostępne w 10 językach!

## Wizja i finansowanie SimpleX Chat

### Dlaczego jest to firma komercyjna?

To była wielka decyzja: czy SimpleX Chat powinien być projektem non-profit, czy projektem open-source prowadzonym przez firmę komercyjną.

W ciągu ostatnich 25 lat istnienia Internetu firmy komercyjne wykazały znacznie większą zdolność do innowacji niż organizacje non-profit. Jednym z najbardziej inspirujących przykładów jest NetScape, który stworzył sieć, jaką znamy - nie tylko jako miejsce dostępu do informacji, ale także jako platformę aplikacyjną, wymyślając ciasteczka, SSL i JavaScript, które do dziś pozostają głównymi elementami składowymi wszystkich aplikacji internetowych.

Chociaż SimpleX Chat Ltd jest firmą komercyjną, oprogramowanie SimpleX Chat jest i pozostanie open-source. Wierzymy, że w ten sposób stworzymy znacznie większą wartość zarówno dla użytkowników końcowych, jak i dla akcjonariuszy i pracowników firmy.

Wiele dużych firm technologicznych przedkładających pozyskiwanie wartości nad jej tworzenie zapracowało na złą reputację wszystkich firm, szczególnie w społecznościach ceniących decentralizację i prywatność. Ale cele komercyjne nie muszą skutkować wyzyskiem. Naszym celem jest zbudowanie nowego rodzaju sieci komunikacyjnej, a także platformy aplikacyjnej, która jest prywatna z założenia, w pełni zdecentralizowana i nie jest własnością żadnego pojedynczego podmiotu, gdzie SimpleX Chat Ltd jest jedną z wielu organizacji obsługujących sieć. Swoimi przemyśleniami na temat tego jak może ewoluować Internet i prywatność podzieliłem się w wywiadzie w [Podcascie Opt Out](https://optoutpod.com/episodes/s3e02-simplexchat/).

### Jak to jest finansowane i jaki jest model biznesowy?

Zaczęliśmy pracować nad projektem w pełnym wymiarze godzin w 2021 roku, kiedy to [Portman Wills](https://www.linkedin.com/in/portmanwills/) i [Peter Briffett](https://www.linkedin.com/in/peterbriffett/) (założyciele [Wagestream](https://wagestream.com/en/), gdzie kierowałem zespołem inżynierów) wsparli firmę bardzo wcześnie, a kilku innych inwestorów-aniołów dołączyło później. W lipcu 2022 roku SimpleX Chat pozyskał wstępne finansowanie od funduszu VC [Village Global](https://www.villageglobal.vc) - jego współzałożyciel [Ben Casnocha](https://casnocha.com) był bardzo podekscytowany naszą wizją prywatnej, w pełni zdecentralizowanej platformy komunikacyjnej i społecznościowej, zarówno dla indywidualnych użytkowników, jak i dla firm, niezależnej od jakichkolwiek kryptowalut, która mogłaby zastąpić duże scentralizowane platformy, takie jak WhatsApp, Telegram i Signal.

Ogólnie rzecz biorąc, zebraliśmy od naszych inwestorów około 370 000 USD za mały udział w firmie, aby umożliwić zespołowi projektowemu pracującemu w pełnym wymiarze czasu przez prawie dwa lata, finansując projekt i rozwój produktu, infrastrukturę, a także [ocenę bezpieczeństwa przez Trail of Bits](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/blog/20221108-simplex-chat-v4.2-security-audit-new-website.html). Duża część tych pieniędzy nie została jeszcze wydana.

Projekt był również ogromnie wspierany przez użytkowników - wspólnie [przekazaliście](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/README.md#wspomóż-nas-darowiznami) ponad 25 000 dolarów. Bez tych darowizn zebrana przez nas inwestycja nie byłaby możliwa, ponieważ wierzymy, że dobrowolne datki użytkowników mogą podtrzymać projekt w dłuższej perspektywie - już teraz pokrywają one wszystkie koszty infrastruktury. Są tylko dwa sposoby na istnienie usługi internetowej - albo użytkownicy za nią płacą, albo dane użytkowników stają się produktem dla prawdziwych klientów, jak to miało miejsce w przypadku wielu dużych firm internetowych. W tym drugim przypadku użytkownicy tracą znacznie więcej pieniędzy niż oszczędzają oddając swoją prywatność i prawa do treści, które tworzą na scentralizowanych platformach.

W przyszłości planujemy utrzymać podstawowe korzystanie z platformy za darmo, a jednocześnie będziemy dostarczać korzyści sponsorom projektu. Na przykład, pojawią się dodatkowe ikony aplikacji i odznaki profilu użytkownika. Będą też wyższe limity transferu plików - obecnie nie limitujemy go w ogóle, ograniczając jedynie rozmiar pliku, ale to raczej nie do utrzymania. W każdym razie, aplikacja pozostanie wysoce użyteczna dla każdego za darmo i w pełni open-source. Kilka innych aplikacji jest już rozwijanych w oparciu o nasz rdzeń aplikacji, co prowadzi do w pełni zdecentralizowanej sieci.

### Co dalej?

Nasze cele na następne 1-2 lata to uczynienie sieci przesyłania wiadomości:
- bardziej niezawodnej i odpornej, poprzez dodanie nadmiarowości do dostarczania wiadomości i potwierdzeń doręczenia,
- bardziej prywatna, poprzez zautomatyzowanie rotacji serwerów używanych do dostarczania wiadomości i poprzez dodanie przekaźników dostarczania, aby lepiej chronić adresy IP użytkowników,
- bardziej użyteczne poprzez dodanie i ulepszenie funkcji, których użytkownicy oczekują w komunikatorach, a także dodanie kilku unikalnych funkcji, tak jak to zrobiliśmy z [trybem incognito] (https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/blog/20220901-simplex-chat-v3.2-incognito-mode.md#tryb-incognito), [wiadomości na żywo](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/blog/20230103-simplex-chat-v4.4-disappearing-messages.md#wiadomości-na-żywo) i [ukryte profile](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/blog/20230328-simplex-chat-v4-6-hidden-profiles.md#ukryte-profile-czatu).

Jedną z głównych inicjatyw, którą rozpoczniemy w tym roku jest wsparcie dla dużych, w pełni zdecentralizowanych i prywatnych społeczności i grup, które nie są hostowane na żadnych serwerach - coś, czego żadna platforma internetowa nie osiągnęła do tej pory w tak prywatny i wydajny sposób, jak planujemy to zbudować.

Aby przyspieszyć rozwój produktu i wzrost, będziemy w tym roku zbierać fundusze zalążkowe, zarówno od VC, jak i od inwestorów-aniołów, a także możemy zaoferować naszym użytkownikom możliwość udziału w rundzie crowdfundingowej na takich samych warunkach jak inni inwestorzy, pozwalając zarówno wspierać projekt, jak i czerpać korzyści z jego przyszłego wzrostu. [Subskrybuj nasze aktualizacje](https://simplex.chat/pl/#join-simplex), aby ich nie przegapić, połącz się z zespołem w SimpleX Chat i [dołącz do grup użytkowników](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/README.md#dołącz-do-grup-użytkowników).

## Co nowego w v5.0.

### Wysyłaj filmy i pliki do 1gb!

<img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20230422-video.png" width="288">

Na początku marca [udostępniliśmy serwery i narzędzie wiersza poleceń do wysyłania i odbierania plików za pomocą protokołu XFTP](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/blog/20230301-simplex-file-transfer-protocol.md) - bardzo prywatnego i bezpiecznego protokołu, który wysyła end-to-end zaszyfrowane pliki w kawałkach, chroniąc metadane lepiej niż jakiekolwiek znane nam alternatywy.

Teraz protokół ten jest w pełni zintegrowany z SimpleX Chat, a wszystkie pliki oprócz małych wiadomości głosowych są wysyłane za pomocą tego protokołu (małe wiadomości głosowe są wysyłane jako zwykłe wiadomości). Nie tylko jest on znacznie szybszy niż wcześniej - przy szybkim łączu internetowym mogę wysłać plik 25Mb w 3 sekundy, a plik 1gb w 2 minuty (dla większości użytkowników jest to ograniczone dostępną przepustowością Internetu), ale ma też dwie inne ważne zalety czyniące go bardziej użytecznym:

- plik wysyłany do grupy musi być przesłany tylko raz, niezależnie od wielkości grupy.
- po przesłaniu pliku (będzie on miał zaznaczenie) nie trzeba już być online, aby kontakt (lub członkowie grupy) mógł go odebrać.

Co do prywatności metadanych, to ma ona podobną gwarancję jak SimpleX Messaging Protocol. Pliki są przesyłane za pomocą protokołu TLS 1.2/1.3, z taką samą weryfikacją tożsamości serwera, wiązaniem kanałów TLS i autoryzacją wysyłania, jak w przypadku serwerów SMP. Metadane plików są jednak chronione nawet w przypadku naruszenia TLS, ponieważ nie ma wspólnych identyfikatorów i szyfrogramów w ruchu odbieranym i wysyłanym przez serwer, co pozwala na korelację tylko według czasu wysyłania i odbierania. Korelacja według czasu staje się mniej wydajna wraz ze wzrostem ruchu na serwerze.

Dodaliśmy również możliwość wysyłania filmów na czacie, dzięki czemu można je odtwarzać bezpośrednio w rozmowie lub na pełnym ekranie bez wychodzenia z aplikacji - dzięki sprawnemu i szybkiemu transferowi plików są one bardzo użyteczne.

Planujemy również dodać wsparcie dla dłuższych i wyższej jakości wiadomości głosowych, a także dla wysyłania obrazów w pełnej rozdzielczości w kolejnych wersjach.

### Hasło do aplikacji

<img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20230422-passcode1.png" width="288"> &nbsp;&nbsp; <img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20230422-passcode2.png" width="288">

Chociaż uważamy, że hasło aplikacji nie zwiększa zbytnio bezpieczeństwa w porównaniu z kodem urządzenia lub ochroną biometryczną, istnieją przypadki, w których może być preferowane, a także wielu użytkowników naprawdę chciało, aby zostało dodane.

Teraz możesz wybrać, czy użyć szybszego i wygodniejszego uwierzytelniania biometrycznego systemu, czy użyć oddzielnego hasła dostępu do aplikacji. Możesz wybrać, który z nich ma być używany, gdy jest po raz pierwszy oferowany, lub przełączyć później w ustawieniach.

### Ulepszenia dotyczące sieci.

<img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20230422-socks.png" width="288">

W tej wersji dodano dwa małe usprawnienia możliwości sieciowych aplikacji.

Po pierwsze, możesz teraz udostępnić swoje serwery z własnym hostingiem na adresach IPv6, a aplikacja będzie je obsługiwać. Należy pamiętać, że starsze wersje  klientów nie będą w stanie połączyć się z Tobą, jeśli użyjesz adresu IPv6, a także, że niektórzy dostawcy usług internetowych nie udostępniają adresów IPv6 swoim użytkownikom, w którym to przypadku również nie będą mogli się połączyć, jeśli adres IPv6 jest używany. Aby umożliwić połączenia w takich przypadkach należy udostępnić swoje serwery na jakiejś nazwie domeny, która rozwiązuje zarówno adresy IPv4 jak i IPv6, i użyć tej nazwy domeny w adresie serwera.

Po drugie, klient Androida obsługuje teraz konfigurację hosta i portu SOCKS proxy, co pozwala na korzystanie z aplikacji innych niż Orbot, a także na uruchomienie SOCKS proxy w sieci lokalnej, aby oszczędzać baterię urządzenia mobilnego.

## Platforma SimpleX

Kilka linków odpowiadających na najczęstsze pytania:

[Ocena bezpieczeństwa SimpleX Chat](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/blog/20221108-simplex-chat-v4.2-security-audit-new-website.md).

[Jak SimpleX może dostarczać wiadomości bez identyfikatorów użytkowników](https://simplex.chat/pl/#how-simplex-works).

  [Jakie są zagrożenia związane z posiadaniem identyfikatorów przypisanych do użytkowników](https://simplex.chat/pl/#why-ids-bad-for-privacy).

[Szczegóły techniczne i ograniczenia](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/README.md#prywatność-szczegóły-techniczne-i-ograniczenia).

[Czym SimpleX różni się od Session, Matrix, Signal itp.](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/README.md#najczęściej-zadawane-pytania).

Odwiedź naszą [stronę internetową](https://simplex.chat/pl), aby dowiedzieć się więcej.

## Wspomóż nas darowiznami

Ogromne podziękowania dla wszystkich, którzy przekazali darowiznę na rzecz SimpleX Chat!

Stawiamy na pierwszym miejscu prywatność i bezpieczeństwo użytkowników - bez Waszego wsparcia byłoby to niemożliwe.

Zobowiązujemy się, że protokoły SimpleX są i pozostaną otwarte, w domenie publicznej, więc każdy może budować przyszłe implementacje klientów i serwerów. Budujemy platformę SimpleX w oparciu o te same zasady, co w przypadku poczty elektronicznej i Internetu, ale znacznie bardziej prywatną i bezpieczną.

Twoje darowizny pomagają nam zebrać więcej funduszy - każda kwota, nawet cena filiżanki kawy, robi dla nas wielką różnicę.

Zobacz [ten dział](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/README.md#wspomóż-nas-dotacjami), aby poznać sposoby przekazywania darowizn.

Dziękuję,

Evgeny

Założyciel SimpleX Chat
