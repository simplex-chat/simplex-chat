---
layout: layouts/article.html
title: "SimpleX announces SimpleX Chat public beta for iOS"
date: 2022-02-14
preview: Our first prototype of mobile UI for iOS is available!
permalink: "/blog/20220214-simplex-chat-ios-public-beta.html"
---

# SimpleX ogłasza publiczną betę SimpleX Chat dla iOS

**Opublikowano:** 14 lutego 2022 r.

## Prywatna i bezpieczna platforma czatu i aplikacji - [publiczna beta jest już dostępna](https://testflight.apple.com/join/DWuT2LQu) dla iPhone'ów z systemem iOS 15.

Nasza nowa aplikacja na iPhone'a jest bardzo podstawowa - w tej chwili obsługuje tylko wiadomości tekstowe i emojis.

Mimo że aplikacja jest nowa, wykorzystuje ten sam rdzeniowy kod, co nasza aplikacja terminalowa, który był używany i ustabilizowany przez długi czas, i zapewnia ten sam poziom prywatności i bezpieczeństwa, który jest dostępny od wydania v1 miesiąc temu:

- [double-ratchet](https://www.signal.org/docs/specifications/doubleratchet/) szyfrowanie E2E.
- osobne klucze dla każdego kontaktu.
- dodatkowa warstwa szyfrowania E2E w każdej kolejce wiadomości (aby zapobiec korelacji ruchu, gdy wiele kolejek jest używanych w konwersacji - coś, co planujemy jeszcze w tym roku).
- dodatkowe szyfrowanie wiadomości dostarczanych z serwerów do odbiorców (również w celu zapobiegania korelacji ruchu).

Możesz przeczytać więcej szczegółów w naszym ostatnim [ogłoszeniu v1](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/blog/20220112-simplex-chat-v1-released.md).

## Dołącz do naszej publicznej bety!

Zainstaluj aplikację [poprzez TestFlight](https://testflight.apple.com/join/DWuT2LQu), połącz się z nami (poprzez link **Połącz się z zespołem SimpleX** w aplikacji) oraz z kilkoma znajomymi, do których zwykle wysyłasz wiadomości - i proszę daj nam znać, co myślisz!

Będziemy naprawdę wdzięczni za wszelkie opinie, aby ulepszyć aplikację i zdecydować, które dodatkowe funkcje powinny być zawarte w naszym publicznym wydaniu w marcu.

Czy powinny to być:

- obrazy,
- podglądy linków,
- czy może coś innego, co nie przyszło nam do głowy.

Prosimy o głosowanie na funkcje, które uważasz za najbardziej potrzebne w naszej [mapa drogowa aplikacji](https://app.loopedin.io/simplex).

## Czym jest SimpleX?

Budujemy nową platformę dla rozproszonych aplikacji internetowych, gdzie prywatność wiadomości i sieci ma znaczenie.

Naszym celem jest zapewnienie najlepszej możliwej ochrony wiadomości i metadanych. Obecnie nie ma aplikacji do przesyłania wiadomości, która działa bez globalnej tożsamości użytkowników, więc wierzymy, że zapewniamy lepszą prywatność metadanych niż alternatywne rozwiązania. SimpleX został zaprojektowany jako prawdziwie rozproszony, bez centralnego serwera i bez globalnych tożsamości użytkowników. Pozwala to na wysoką skalowalność przy niskich kosztach, a także praktycznie uniemożliwia szpiegowanie grafu sieciowego.

Pierwszą aplikacją zbudowaną na platformie jest Simplex Chat, która jest dostępna dla terminala (wiersz poleceń w Windows/Mac/Linux) i jako publiczna beta iOS - aplikacja na Androida pojawi się w ciągu kilku tygodni. Platforma może łatwo obsługiwać prywatny kanał sieci społecznościowej i wiele innych usług, które mogą być opracowane przez zespół Simplex lub deweloperów zewnętrznych.

SimpleX pozwala również ludziom na hostowanie własnych serwerów, aby mieć kontrolę nad swoimi danymi czatu. Serwery SimpleX są wyjątkowo lekkie i wymagają pojedynczego procesu z początkowym śladem pamięci poniżej 20 Mb, który rośnie, gdy serwer dodaje kolejki w pamięci (nawet z 10 000 kolejek używa mniej niż 50Mb, nie licząc wiadomości). Należy jednak wziąć pod uwagę, że podczas gdy samodzielne hostowanie serwerów zapewnia większą kontrolę, może to zmniejszyć prywatność metadanych, ponieważ łatwiej jest skorelować ruch na serwerach z małą liczbą przychodzących wiadomości.

Dalsze szczegóły dotyczące celów platformy i projektu technicznego są dostępne [w przeglądzie platformy SimpleX](https://github.com/simplex-chat/simplexmq/blob/master/protocol/overview-tjr.md).
