---
layout: layouts/article.html
title: "SimpleX Chat v4.6 – with hidden profiles, community moderation, improved audio/video calls and reduced battery usage."
date: 2023-03-28
image: images/20230328-hidden-profiles2.png
imageBottom: true
previewBody: blog_previews/20230328.html
permalink: "/blog/20230328-simplex-chat-v4-6-hidden-profiles.html"
---

# SimpleX Chat v4.6 - z ukrytymi profilami, moderacją społeczności, ulepszonymi połączeniami audio/wideo i zmniejszonym zużyciem baterii.

**Opublikowano:** 28 marca, 2023

## Co nowego w wersji 4.6 v4.6

- [wsparcie dla ARMv7a i Androida 8+](#wsparcie-dla-armv7a-i-androida-8)
- [ukryte profile czatu](#ukryte-profile-czatu)
- [moderacja grupy / społeczności](#moderacja-grupy-społeczności)
- [wiadomość powitalna grupy](#wiadomość-powitalna-grupy)
- [ulepszone połączenia audio/wideo](#ulepszone-połączenia-audio-wideo)
- [zmniejszone zużycie baterii](#zmniejszone-zużycie-baterii).
- [monitorowanie serwera SMP: bot statusu i strona](#monitorowanie-serwera-smp)

Dodaliśmy również [języki chińskiego i hiszpańskiego interfejsu](#chiński-i-hiszpański-interfejs---możesz-wybrać-język-za-pośrednictwem-aplikacji), dzięki społeczności użytkowników i firmie Weblate!

### Wsparcie dla ARMv7a i Androida 8+ !

To dwukrotnie zwiększa liczbę obsługiwanych urządzeń z systemem Android - teraz większość Twoich znajomych powinna móc zainstalować SimpleX Chat. SimpleX Chat nadal nie wspiera Androida 7 i wersji starszych.

Jeśli zainstalujesz aplikację z GitHuba lub F-Droid musisz wybrać odpowiedni APK, Sklep Play dostarczy go automatycznie.

### Ukryte profile czatu

<img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20230328-hidden-profiles1.png" width="288"> &nbsp;&nbsp; <img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20230328-hidden-profiles2.png" width="288"> &nbsp;&nbsp; <img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20230328-hidden-profiles3.png" width="288">

Przez długi czas głównym sposobem ochrony aplikacji SimpleX Chat przed osobami mającymi dostęp do telefonu było uwierzytelnianie urządzenia - jest ono wymagane podczas otwierania aplikacji (i niektórych jej wrażliwych funkcji).

Wielu użytkowników poprosiło o umożliwienie ustawienia hasła do aplikacji lub kodu PIN niezależnie od kodu PIN urządzenia, jak to robią niektóre inne aplikacje. Jednak nie wydawało się to wystarczająco dobrym rozwiązaniem - jeśli ktoś ma Twój PIN urządzenia, to w większości przypadków może również poprosić Cię o podanie PINu aplikacji.

Więc zamiast mieć hasło do aplikacji, które zmniejsza wygodę i nie poprawia zbytnio bezpieczeństwa, zrobiliśmy to, co naszym zdaniem może być lepsze. Możesz teraz tworzyć ukryte profile czatu, które nie są widoczne nigdzie w aplikacji i nie pokazują żadnych powiadomień, dopóki nie wprowadzisz poprawnego hasła. Jeśli wiele profili są ukryte z tym samym hasłem, wszystkie będą widoczne na liście po wprowadzeniu go.

Ważne jest, aby pamiętać, że te ukryte profile są nadal przechowywane lokalnie na urządzeniu, więc jeśli ktoś ma dostęp do bazy danych czatu (musi znać hasło do bazy danych, która jest niezależna od PIN urządzenia lub hasła profilu) lub do konsoli czatu w aplikacji, będzie w stanie uzyskać dostęp do danych tych profili i zresetować ich hasła. Zastanawiamy się jak lepiej zabezpieczyć konsolę czatu - np. wymagając osobnego hasła lub dając opcję usunięcia jej z UI na stałe - powiedz nam co myślisz.

### Moderacja grupy / społeczności

Początkowo nie projektowaliśmy SimpleX Chat do obsługi społeczności - naszym celem zawsze była maksymalna prywatność i bezpieczeństwo.

SimpleX Chat obsługuje małe i w pełni zdecentralizowane grupy, które nie są nigdzie hostowane. Jednak wielu użytkowników chce uczestniczyć w społecznościach i odkrywać je. Odkąd dodaliśmy obsługę linków grupowych, grupy, które stworzyliśmy, aby umożliwić użytkownikom testowanie aplikacji, zaczęły się rozrastać, a także pojawiło się wiele innych społeczności liczących ponad 100 osób.

Obserwujemy już pewne mniej przyjazne wiadomości i niepożądane treści, które nie są mile widziane w niektórych społecznościach. Dlatego w tej wersji dodano funkcje pozwalające na moderowanie grup.

Po pierwsze, administratorzy i właściciele grup mogą odebrać członkom prawa do wysyłania wiadomości do grupy poprzez przypisanie im roli "obserwatora", a także uczynić tę rolę domyślną dla użytkowników dołączających poprzez link do grupy.

Po drugie, administratorzy grupy mogą teraz usuwać wiadomości wysłane przez innych członków (z wyłączeniem wiadomości wysłanych przez właścicieli grupy). "Moderuj" akcja w menu wiadomości będzie oznaczać wiadomość jako usuniętą lub usunie ją nieodwracalnie dla wszystkich członków, jak ustawiono w preferencjach grupy przez właścicieli.

Te funkcje pozwolą właścicielom grup decydować o własnych zasadach. Bardziej rozbudowane narzędzia moderacyjne pojawią się później, gdy zbudujemy wsparcie dla dużych społeczności.

### Wiadomość powitalna grupy

Właściciele grup mogą teraz ustawić wiadomość powitalną, która będzie pokazywana nowym członkom, gdy dołączą do grupy poprzez łącze grupowe.

### Ulepszone połączenia audio/wideo

<img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20230328-call2.png" width="288">

Przed tą wersją - połączenia audio i wideo w aplikacji iOS były bardzo ograniczone - działały tylko wtedy, gdy aplikacja była na pierwszym planie. Ta wersja w pełni ponownie zaimplementowała połączenia audio/wideo w iOS - teraz używa natywnej biblioteki WebRTC zamiast widoku sieciowego. Połączenia te są nadal szyfrowane end-to-end i zgodne z połączeniami w poprzednich wersjach aplikacji, zarówno na platformach iOS, jak i Android.

Tam, gdzie pozwala na to polityka App Store, połączenia na iOS korzystają teraz z natywnego interfejsu Apple dla połączeń CallKit, który pozwala na przyjmowanie połączeń z ekranu blokady, zapobiega przerywaniu rozmów przez przychodzące połączenia telefoniczne i opcjonalnie pozwala na uwzględnienie połączeń w historii połączeń telefonicznych - ostatnią opcję trzeba włączyć osobno.

Usprawniono również połączenia na Androidzie - obsługują teraz słuchawki bluetooth, pozwalają na zmianę głośności w połączeniach wideo oraz obsługują czujnik zbliżeniowy podczas rozmowy audio, aby zapobiec przypadkowemu przerwaniu połączenia, gdy trzymamy telefon blisko ucha.

### Zmniejszone zużycie baterii

Wiemy, że zużycie baterii w SimpleX Chat jest nieoptymalne i zobowiązujemy się do jego zmniejszenia. Niestety, nie ma prostej zmiany, którą moglibyśmy wprowadzić, aby rozwiązać ten problem, wymaga to wielu systematycznych ulepszeń i poprawek.

Jednym z większych problemów, szczególnie w dużych grupach, była nieefektywna strategia ponawiania prób wysyłania wiadomości w przypadkach, gdy kolejka wiadomości odbierających (skrzynka pocztowa) nie miała pojemności.

Ta wersja zwiększa maksymalny okres ponawiania próby dla scenariusza "out-of-capacity" do 1 godziny, a także zachowuje ten okres w bazie danych. Tak więc, jeśli wcześniej przed wygaśnięciem wiadomości w ciągu 48 godzin było do ~2800 prób dostarczenia i do ~45Mb zmarnowanego ruchu na odbiorcę (w zależności od tego, jak często aplikacja była restartowana), teraz będzie tylko ~50 ponownych prób, co spowoduje nie więcej niż 0.8Mb ruchu - do 56x zmniejszenie ruchu podczas wysyłania wiadomości do dużych grup.

Ten problem mógł Cię w ogóle nie dotyczyć, a także rozwiązanie go nie zmniejszy ogólnego zużycia ruchu/baterii o ten współczynnik - są inne nieefektywności, którymi będziemy się zajmować. Ale jeśli aktywnie wysyłałeś wiadomości do dużych grup, powinieneś zaobserwować znaczne zmniejszenie zużycia baterii i ruchu.

Proszę podzielić się swoimi doświadczeniami. Jeśli zużycie baterii jest nadal nieoptymalne, prosimy o podzielenie się swoimi statystykami użytkowania - można je uzyskać w konsoli czatu za pomocą komendy `/get stats` - zwróci ona zagregowaną liczbę operacji sieciowych, dla każdego serwera, od momentu uruchomienia aplikacji. Zwróć uwagę, że te statystyki zawierają adresy serwerów, z którymi się łączysz, więc jeśli chcesz zachować je w tajemnicy, proszę je zredagować. Możesz również zresetować statystyki użycia za pomocą polecenia `/reset stats`.

### Monitorowanie serwera SMP

Jeśli używasz wstępnie ustawionych serwerów w SimpleX Chat, możesz teraz zobaczyć, kiedy przeprowadzamy konserwację lub kiedy serwer jest wyłączony albo przez [połączenie z botem statusowym przez aplikację](https://simplex.chat/pl/contact#/?v=1-2&smp=smp%3A%2F%2Fu2dS9sG8nMNURyZwqASV4yROM28Er0luVTx5X1CsMrU%3D%40smp4.simplex.im%2FShQuD-rPokbDvkyotKx5NwM8P3oUXHxA%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEA6fSx1k9zrOmF0BJpCaTarZvnZpMTAVQhd3RkDQ35KT0%253D%26srv%3Do5vmywmrnaxalvz6wi3zicyftgio6psuvyniis6gco6bp6ekl4cqj4id.onion) lub odwiedzając [stronę statusu](https://status.simplex.chat). Status bot zawsze wysyła automatyczne wiadomości przed ponownym uruchomieniem serwera w celu konserwacji, ale w przypadku przestoju, jeśli ten sam serwer jest wyłączony, którego używasz do odbierania wiadomości od bota, możesz je przegapić - sprawdź stronę statusu w tym przypadku.

### Chiński i hiszpański interfejs - możesz wybrać język za pośrednictwem aplikacji!

Dzięki społeczności naszych użytkowników i firmie Weblate zapewniającej bezpłatny plan hostingowy dla tłumaczeń SimpleX Chat możemy teraz obsługiwać więcej języków w interfejsie - ta wersja dodaje chiński i hiszpański, a kolejne są w toku.

Możesz też [przyczynić się do przetłumaczenia](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/README.md#pomóż-w-tłumaczeniu-simplex-chat) aplikacji na swój język!

Aplikacja obsługuje teraz 8 języków oprócz angielskiego - czeski, niemiecki, hiszpański, francuski, włoski, holenderski, rosyjski i chiński. Możesz teraz wybrać język poprzez ustawienia aplikacji (strona Wygląd), niezależnie od ustawień systemowych.

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
