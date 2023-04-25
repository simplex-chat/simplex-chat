---
layout: layouts/article.html
title: "SimpleX Chat v4.4 released – with disappearing messages, live messages, connection security verification and French language!"
date: 2023-01-03
image: images/20230103-disappearing1.png
imageBottom: true
previewBody: blog_previews/20230103.html
permalink: "/blog/20230103-simplex-chat-v4.4-disappearing-messages.html"
---

# SimpleX Chat v4.4 wydany - ze znikającymi wiadomościami, wiadomościami na żywo, weryfikacją bezpieczeństwa połączenia i językiem francuskim!

**Opublikowano:** 3 stycznia, 2023

## Co nowego w v4.4

- [znikające wiadomości](#znikające-wiadomości).
- [wiadomości "na żywo"](#wiadomości-na-żywo).
- [sprawdzanie kodu bezpieczeństwa połączenia](#sprawdzanie-kodu-bezpieczenstwa-polaczenia).
- [animowane obrazki i naklejki](#animowane-obrazki-i-naklejki) - teraz także na iOS.

Dodaliśmy też [interfejs w języku francuskim](#french-language-interface), dzięki społeczności użytkowników i firmie Weblate!

### Znikające wiadomości

<img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20230103-disappearing1.png" width="288"> &nbsp;&nbsp; <img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20230103-disappearing2.png" width="288">

Teraz możliwe jest wysyłanie wiadomości, które zostaną usunięte zarówno z urządzenia nadawcy, jak i odbiorcy po ustalonym czasie - dla nadawcy od momentu ich wysłania, a dla odbiorcy - od momentu ich przeczytania.

W przeciwieństwie do większości innych komunikatorów, wymaga to zgody obu stron, a nie tylko decyzji nadawcy. Napisałem [wcześniej](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/blog/20221206-simplex-chat-v4.3-voice-messages.md#nieodwracalne-usuwanie-wiadomości) dlaczego uważamy, że pozwolenie nadawcom na usuwanie ich wiadomości bez zgody odbiorcy jest złe i ta sama logika ma zastosowanie tutaj - jeśli chcesz wysłać wiadomość, która zniknie po jakimś czasie, twój kontakt też powinien się na to zgodzić.

W rozmowach grupowych znikające wiadomości mogą być włączone przez właścicieli grupy, domyślnie są wyłączone.

### Wiadomości "Na żywo"

<img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20230103-live.png" width="288">

Naciśnięcie przycisku "błyskawicy" przed rozpoczęciem pisania wiadomości spowoduje rozpoczęcie wiadomości "na żywo". Teraz, w miarę wpisywania, będzie ona aktualizowana dla wszystkich odbiorców co kilka sekund, uwzględniając tylko pełne słowa. Aby zakończyć wiadomość musisz nacisnąć przycisk "fajki".

Możesz również rozpocząć wiadomość na żywo po rozpoczęciu pisania lub po wybraniu obrazu - naciśnij długo przycisk wysyłania, a następnie naciśnij "Wyślij wiadomość na żywo".

### Sprawdzanie kodu bezpieczeństwa połączenia

<img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20230103-verification.png" width="288">

Projekt SimpleX Chat zapobiega możliwości podmienienia klucza przez serwery wiadomości podczas pierwszego połączenia (atak man-in-the-middle), wymagając, aby łącze zaproszenia było przekazywane innym kanałem. Napisałem więcej o tym, jak działa atak MITM w [tym poście](https://www.poberezkin.com/posts/2022-12-07-why-privacy-needs-to-be-redefined.html). Ale ten inny kanał, jakkolwiek mało prawdopodobny, nadal mógł zostać skompromitowany przez atakującego, aby zastąpić link zaproszenia, który wysłałeś. To jest powód, dla którego zalecamy udostępnianie kodu QR w rozmowie wideo - jest to bardzo skomplikowane dla atakującego, aby zastąpić go w tym przypadku.

Ta nowa funkcja pozwala zweryfikować, poprzez jeszcze jeden kanał, że połączenie jest bezpieczne, a klucze nie zostały podmienione. Możesz zeskanować kod bezpieczeństwa z aplikacji swojego kontaktu, albo porównać kody wizualnie, a nawet odczytać go w rozmowie głosowej - jeśli aplikacja Twoja i Twojego kontaktu mają dla siebie ten sam kod bezpieczeństwa to połączenie jest bezpieczne.

Jeśli wysyłasz bezpośrednie wiadomości do niektórych członków grupy, to również może być ważne, aby zweryfikować bezpieczeństwo tych połączeń, ponieważ w tym przypadku zaproszenia były wymieniane za pośrednictwem członka, który dodał Ciebie lub innego członka, a jeśli klient tego członka został zmodyfikowany, mógł on podmienić klucze i adresy, i przechwycić całą rozmowę.

Niezależnie od sposobu nawiązania połączenia, weryfikacja połączenia dowodzi jego bezpieczeństwa. Technicznie rzecz biorąc, ten kod bezpieczeństwa to hash powiązanych danych używanych w szyfrowaniu end-to-end, który z kolei jest pobierany przez połączenie kluczy publicznych z początkowej wymiany kluczy.

### Animowane obrazki i naklejki

<img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20230103-stickers1.png" width="288"> &nbsp;&nbsp; <img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20230103-stickers2.png" width="303">

Aplikacja na Androida wspierała GIF-y i naklejki przez jakiś czas, teraz można je wyświetlać i wysyłać również z aplikacji na iOS, np. za pomocą klawiatury GIPHY - nie trzeba już wybierać między prywatnością a naklejkami. Pamiętajcie tylko, że klawiatury firm trzecich mogą być niezbyt bezpieczne, więc nie powinniście ich używać do wpisywania wrażliwych informacji.

### Interfejs w języku francuskim

Dzięki społeczności naszych użytkowników i firmie [Weblate](https://weblate.org/pl), która udostępniła darmowy hosting dla tłumaczeń SimpleX Chat, możemy teraz obsługiwać więcej języków w interfejsie - w tej wersji dodano język francuski.

Prosimy o kontakt, jeśli chcesz przetłumaczyć interfejs na swój język!

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

Wasze darowizny pomagają nam zebrać więcej funduszy - każda kwota, nawet cena filiżanki kawy, robi dla nas wielką różnicę.

Możliwe jest przekazanie darowizny poprzez:

- [GitHub](https://github.com/sponsors/simplex-chat) - nie pobiera prowizji od nas.
- [OpenCollective](https://opencollective.com/simplex-chat) - pobiera prowizję, przyjmuje również darowizny w kryptowalutach.
- Adres Monero: 8568eeVjaJ1RQ65ZUn9PRQ8ENtqeX9VhcCYYhnVLxhV4JtBqw42so2VEUDQZNkFfsH5sXCuV7FN8VhRQ21DkNibTZP57Qt - Adres Bitcoin: 1bpefFkzuRoMY3ZuBbZNZxycbg7NYPYTG
- Adres BCH: 1bpefFkzuRoMY3ZuBbZNZxycbg7NYPYTG
- Adres Ethereum: 0x83fd788f7241a2be61780ea9dc72d2151e6843e2
- proszę dać nam znać, za pośrednictwem wydania GitHub lub czatu, jeśli chcesz utworzyć darowiznę w jakiejś innej kryptowalucie - dodamy adres do listy.

Dziękuję,

Evgeny

Założyciel SimpleX Chat
