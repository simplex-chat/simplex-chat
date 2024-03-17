---
layout: layouts/article.html
title: "SimpleX announces SimpleX Chat v0.5"
date: 2021-12-08
preview: Support for long-term user addresses in terminal app.
permalink: "/blog/20211208-simplex-chat-v0.5-released.html"
---

# SimpleX ogłasza SimpleX Chat v0.5

**Opublikowano:** 8 grudnia, 2021

## Simplex Chat to pierwsza platforma czatowa, która jest w 100% prywatna z założenia - SimpleX nie ma dostępu do twojego wykresu połączeń.

Budujemy nową platformę dla rozproszonych aplikacji internetowych, gdzie prywatność wiadomości i sieci ma znaczenie. [SimpleX Chat](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/README.md) to nasza pierwsza aplikacja, czat zbudowany na platformie SimpleX, który służy jako przykład mocy platformy i jako aplikacja referencyjna.

## Czym jest SimpleX?

Uznaliśmy, że obecnie nie ma aplikacji do przesyłania wiadomości, która szanowałaby prywatność użytkownika i gwarantowała prywatność metadanych - innymi słowy, wiadomości mogą być prywatne, ale strona trzecia zawsze może zobaczyć, kto z kim się komunikuje, badając centralną usługę i wykres połączeń. SimpleX, w swej istocie, jest zaprojektowany jako prawdziwie rozproszony, bez centralnego serwera. Pozwala to na ogromną skalowalność przy niskich kosztach, a także praktycznie uniemożliwia szpiegowanie grafu sieciowego.

Pierwszą aplikacją zbudowaną na platformie jest Simplex Chat, który na razie działa w oparciu o terminal (wiersz poleceń), a w przygotowaniu są aplikacje mobilne. Platforma może z łatwością obsługiwać prywatny kanał społecznościowy i wiele innych usług, które mogą być opracowane przez zespół Simplex lub deweloperów zewnętrznych.

## Co nowego w v0.5?

### Długoterminowe adresy czatu

Użytkownicy mogą teraz tworzyć długoterminowe adresy czatu, które mogą udostępniać wielu osobom (np. w podpisie e-mail lub online), dzięki czemu każdy użytkownik czatu może wysłać do nich prośbę o połączenie.

Jest to funkcja ALPHA i nie dodaliśmy jeszcze żadnej ochrony przed spamowymi prośbami o kontakt. Jednakże, jeśli adres, który utworzyłeś zacznie otrzymywać spamowe prośby o połączenie, możesz po prostu go usunąć bez utraty jakichkolwiek zaakceptowanych połączeń i utworzyć inny adres - tyle razy ile chcesz!

## Potrzebujemy Twojej pomocy!

Naprawdę docenimy twoje komentarze, krytykę i wsparcie - gwiazdka na repozytorium GitHub, pobranie i przetestowanie czatu lub jakikolwiek wkład w projekt bardzo pomoże - dziękujemy za całe wsparcie!

**Zwróć uwagę:** SimpleX Chat jest we wczesnej fazie rozwoju: nadal udoskonalamy protokoły, poprawiamy prywatność i bezpieczeństwo, więc jeśli masz scenariusze komunikacyjne wymagające wysokiego bezpieczeństwa, powinieneś rozważyć inne opcje na razie.

Naszym celem jest stworzenie nowego rodzaju platformy czatu, która pozwala kontrolować swój czat!

Pierwotnie opublikowane na [https://www.reddit.com/r/haskell/comments/rc0xkn/simplex_chat_the_first_chat_platform_that_is_100/](https://www.reddit.com/r/haskell/comments/rc0xkn/simplex_chat_the_first_chat_platform_that_is_100/)
