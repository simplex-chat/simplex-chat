---
layout: layouts/article.html
title: "Announcing SimpleX Chat Prototype!"
date: 2021-05-12
preview: Prototype chat app for the terminal (console).
permalink: "/blog/20210512-simplex-chat-terminal-ui.html"
---

# Zapowiedź prototypu SimpleX Chat!!!!!!

**Opublikowano:** 12 maja 2021 r.

Przez ostatnie sześć miesięcy [ja](https://github.com/epoberezkin) i mój syn [Efim](https://github.com/efim-poberezkin) pracowaliśmy nad stworzeniem działającego prototypu SimpleX Chat. Z przyjemnością ogłaszamy, że klient terminalowy SimpleX Chat jest już dostępny [tutaj](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/README.md) na Linuksa, Windowsa i Maca (można go zbudować ze źródła lub pobrać binarkę dla Linuksa, Windowsa lub Maca z najnowszego wydania).

Używamy klienta terminalowego między nami i kilkoma innymi osobami od kilku miesięcy, jedząc nasze własne "psie jedzenie", i rozwinęliśmy go do wersji 0.3.1, z większością funkcji protokołu komunikacyjnego, które pierwotnie planowaliśmy

## Cechy

- Szyfrowanie end-to-end z ochroną przed atakiem man in the middle. Zaproszenie do połączenia musi być przekazane poza pasmem (zobacz [jak używać SimpleX Chat](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/README.md) w repo).
- Brak globalnej tożsamości lub jakichkolwiek nazw użytkowników widocznych dla serwera (serwerów), zapewniając pełną prywatność kontaktów i rozmów.
- Podpisywanie i weryfikacja wiadomości za pomocą automatycznie generowanych kluczy RSA, przy czym klucze są unikalne dla każdego połączenia.
- Autoryzacja każdego polecenia/wiadomości przez serwery za pomocą automatycznie generowanych par kluczy RSA, również unikalnych dla każdego połączenia.
- Walidacja integralności wiadomości (poprzez przekazywanie digestów poprzednich wiadomości).
- Szyfrowany transport TCP, niezależny od certyfikatów.
- Możesz wdrożyć swój własny serwer, ale nie musisz - demonstracyjny serwer SMP do przekazywania wiadomości jest dostępny pod adresem smp1.simplex.im:5223 (wstępnie skonfigurowany w kliencie).

## Potrzebujemy Twojej pomocy!

Budujemy nowy rodzaj sieci czatowej - jedyną sieć, która pozwala Ci kontrolować Twój czat. Będziemy bardzo wdzięczni za opinie, krytykę i wsparcie - gwiazdka na repo github, zapisanie się na listę mailingową lub jakikolwiek wkład w projekt pomoże. Jest tak wiele więcej do zrobienia!

Pierwotnie opublikowane na [https://www.reddit.com/r/haskell/comments/naw6lz/simplex_chat_prototype_terminal_ui_made_in_haskell/](https://www.reddit.com/r/haskell/comments/naw6lz/simplex_chat_prototype_terminal_ui_made_in_haskell/)
