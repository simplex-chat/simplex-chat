---
layout: layouts/article.html
title: "SimpleX Chat v3.2 is released &mdash; meet T, unique to Simplex Chat"
date: 2022-09-01
image: images/20220901-incognito1.png
imageBottom: true
previewBody: blog_previews/20220901.html
permalink: "/blog/20220901-simplex-chat-v3.2-incognito-mode.html"
---

# SimpleX Chat v3.2 został wydany - poznaj tryb Incognito, unikalny dla Simplex Chat

**Opublikowano:** 1 września, 2022

## Co nowego

- [tryb incognito](#tryb-incognito)
- [przypisywanie nazw swoim kontaktom](#przypisywanie-nazw-swoim-kontaktom)
- [używanie adresów serwerów .onion z Tor](#używanie-adresów-serwerów-onion-z-tor)
- [niekończące się przewijanie i wyszukiwanie w czatach](#niekończące-się-przewijanie-i-wyszukiwanie-w-czatach)
- [wybieranie koloru akcentu i tryb ciemny](#wybieranie-koloru-akcentu-i-tryb-ciemny)
- wyłączenie powiadomień na kontakt / grupę
- na Androidzie:
  - przeciągnij, aby odpowiedzieć
  - zmniejszony rozmiar APK do bezpośredniego pobrania i w repozytorium F-Droid z 200 do 50Mb!

[Audyt wdrożeniowy jest umówiony na październik](#prosimy-o-pomoc-w-opłaceniu-audytu-bezpieczeństwa-przeprowadzanego-przez-trzecią-stronę)!

### Tryb incognito

<img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20220901-incognito1.png" width="330"> <img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20220901-incognito2.png" width="330"> <img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20220901-incognito3.png" width="330">

_SimpleX jest już prywatny, więc dlaczego potrzebujemy trybu incognito_, możesz zapytać.

Rzeczywiście możesz wybrać pseudonim jako swoją główną nazwę profilu, ale jest kilka problemów:

- wielu użytkowników chce mieć swoje prawdziwe imię i nazwisko jako główny profil, aby znajomi ich rozpoznali. Celem SimpleX jest zapewnienie anonimowości przed operatorami sieci, ale niekoniecznie przed Twoimi kontaktami.
- Nawet jeśli wybierzesz pseudonim, będzie on używany dla wszystkich twoich kontaktów. A jeśli dwa z nich spotkają się, nie mogąc udowodnić, że rozmawiają z tą samą osobą, ponieważ używają różnych adresów w sieci SimpleX do wysyłania wiadomości, mogą to _podejrzewać_.
- Każdy pseudonim, który wybierzesz ręcznie, wycieka pewne informacje o tobie, ponieważ nie jest on tak naprawdę losowy.

Możesz również używać wielu profili czatu - obecnie możesz przełączać się między nimi tylko poprzez eksport/import, wkrótce to ułatwimy! Ale są też problemy z wieloma profilami:

- jeśli nawiążesz wiele anonimowych połączeń, każde w swoim własnym profilu użytkownika, skończysz mając zbyt wiele profili - jest to bardzo niewygodne w zarządzaniu.
- Czasami, gdy Twoja relacja z kontaktem ewoluuje, możesz chcieć dzielić z nim swój główny profil i mieć go wśród swoich znajomych - wiele profili tego nie umożliwia.

Dlatego nowy tryb Incognito umożliwia nadanie każdemu nowemu kontaktowi nowej, losowej nazwy, przy czym wszystkie one znajdują się w tym samym profilu użytkownika i nie trzeba nimi zarządzać ręcznie. To jak tryb prywatny w przeglądarkach, gdzie możesz go tymczasowo włączyć, gdy łączysz się z kimś, komu nie ufasz, a następnie wyłączyć go podczas łączenia się z przyjacielem, który cię zna. Można go włączyć poprzez ustawienia aplikacji - patrz zdjęcia.

Nie znam żadnego innego komunikatora z tą funkcją, a zawsze chciałem mieć ten tryb, więc naprawdę czekamy na Wasze opinie na jego temat!

### Przypisywanie nazw swoim kontaktom

Możesz teraz zmienić nazwę, pod którą Twoje kontakty pojawiają się na czatach. Jest to szczególnie przydatne, gdy ktoś połączył się z Tobą używając losowej nazwy - możesz ją zmienić tak, aby była związana z kontekstem połączenia.

### Używanie adresów serwerów .onion z Tor

<img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20220901-onion1.png" width="330"> &nbsp; <img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20220901-onion2.png" width="330">

Udostępniliśmy wsparcie dla używania SOCKS proxy, aby uzyskać dostęp do serwerów wiadomości przez Tor, ale wcześniej serwery były nadal dostępne przez ich publiczne adresy internetowe. Oznacza to, że podczas gdy twój adres IP był chroniony przed serwerem, cały obwód Tor mógł być obserwowany przez niektórych aktorów, a dla niektórych scenariuszy komunikacyjnych nie jest to pożądane.

To wydanie dodaje wsparcie dla serwerów z wieloma nazwami hostów - wszystkie serwery dostarczone przez SimpleX Chat mają teraz podwójne adresy (jeden publiczny i jeden .onion), a ty możesz mieć własne serwery dostępne również poprzez dwa adresy - wszystko co musisz zrobić to zainstalować klienta Tor na swoim serwerze i zarejestrować jego adres w Tor. Jeśli serwer posiada zarówno adres publiczny jak i .onion, nie jest tak naprawdę ukryty, dlatego należy włączyć tryb HiddenServiceSingleHopMode, aby zmniejszyć opóźnienie połączenia - chroni on anonimowość osób łączących się z serwerem, ale nie samego serwera. Adres serwera zawierałby zarówno jego adres publiczny, jak i cebulowy, co widać w adresach serwerów w aplikacji (na stronach kontaktów) - powinieneś użyć tego samego formatu dla adresów swoich serwerów.

Zarówno aplikacja na Androida jak i iOS pozwala zarządzać tym, czy adresy .onion są używane, a także można wymusić używanie adresów .onion - w tym przypadku aplikacja nie połączy się z serwerem, jeśli jedna z jego nazw hostów nie jest adresem .onion. W systemie Android adresy .onion są używane domyślnie, gdy włączone jest proxy SOCKS.

### Niekończące się przewijanie i wyszukiwanie w czatach

Teraz możesz uzyskać dostęp do pełnej historii czatu za pośrednictwem aplikacji - to zawstydzające, jak długo zajęło nam dodanie tego! Można też przeszukiwać wiadomości.

### Wybierz kolor akcentu i tryb ciemny

Wielu z Was mówiło, że niebieski to najgorszy możliwy kolor, więc teraz możecie sprawić, że przyciski i linki aplikacji będą wyglądały tak, jak chcecie! Moje ulubione kolory to zielony i pomarańczowy.

A tryb ciemny lub jasny możecie wybrać niezależnie od ustawień systemowych.

## Platforma SimpleX

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
