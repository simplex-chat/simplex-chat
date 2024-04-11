---
layout: layouts/article.html
title: "SimpleX announces SimpleX Chat v0.4"
date: 2021-09-14
preview: Terminal app now supports groups and file transfers.
permalink: "/blog/20210914-simplex-chat-v0.4-released.html"
---

# SimpleX ogłasza SimpleX Chat v0.4

**Opublikowano:** 14 września 2021 r.

## Zdecentralizowany czat open-source wykorzystujący protokół trasowania wiadomości zachowujący prywatność

Budujemy nową platformę dla rozproszonych aplikacji internetowych, gdzie prywatność wiadomości i sieci ma znaczenie. [SimpleX Chat](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/README.md) to nasza pierwsza aplikacja, czat zbudowany na platformie SimpleX, który służy jako przykład mocy platformy i jako aplikacja referencyjna.

## Czym jest SimpleX?

Uznaliśmy, że obecnie nie ma aplikacji do przesyłania wiadomości, która szanowałaby prywatność użytkownika i gwarantowała prywatność metadanych - innymi słowy, wiadomości mogą być prywatne, ale strona trzecia zawsze może zobaczyć, kto z kim się komunikuje, badając centralną usługę i wykres połączeń. SimpleX, w swej istocie, jest zaprojektowany jako prawdziwie rozproszony, bez centralnego serwera. Pozwala to na ogromną skalowalność przy niskich kosztach, a także praktycznie uniemożliwia szpiegowanie grafu sieciowego.

Pierwszą aplikacją zbudowaną na platformie jest Simplex Chat, który na razie działa w oparciu o terminal (wiersz poleceń), a w przygotowaniu są aplikacje mobilne. Platforma może z łatwością obsługiwać prywatny kanał społecznościowy i wiele innych usług, które mogą być opracowane przez zespół Simplex lub deweloperów zewnętrznych.

## Co nowego w v0.5?

Z radością ogłaszamy, że SimpleX Chat obsługuje teraz czat grupowy i przesyłanie plików!

### Grupy czatu

Aby stworzyć grupę użyj komendy `/g <group>`. Następnie możesz zaprosić kontakty do grupy wpisując komendę `/a <group> <name>`. Twój kontakt (kontakty) będzie musiał użyć komendy `/j accept`, aby zaakceptować zaproszenie do grupy. Aby wysłać wiadomości do grupy, po prostu wpisz `#<group> <message>`.

**Zwróć uwagę:** Grupy nie są przechowywane na żadnym serwerze; są one utrzymywane jako lista członków w bazie danych aplikacji. Wysłanie wiadomości do grupy powoduje wysłanie wiadomości do każdego członka grupy.

![simplex-chat](https://github.com/simplex-chat/simplex-chat/blob/stable/images/groups.gif)

### Transfer plików

Udostępnianie plików jest proste! Aby wysłać plik do kontaktu, należy użyć polecenia `/f @<contact> <file_path>`. Odbiorca będzie musiał zaakceptować, zanim plik zostanie wysłany.

![simplex-chat](https://github.com/simplex-chat/simplex-chat/blob/stable/images/files.gif)

## Zawsze szukamy pomocy!

Będziemy naprawdę wdzięczni za komentarze, krytykę i wsparcie - gwiazdka na repozytorium GitHub, pobranie i przetestowanie czatu lub jakikolwiek wkład w projekt bardzo pomoże - dziękujemy za wszelkie wsparcie!

**Zwróć uwagę:** SimpleX Chat jest we wczesnej fazie rozwoju: nadal udoskonalamy protokoły, poprawiamy prywatność i bezpieczeństwo, więc jeśli masz scenariusze komunikacyjne wymagające wysokiego bezpieczeństwa, powinieneś rozważyć inne opcje na razie.

Naszym celem jest stworzenie nowego rodzaju platformy czatu, która pozwala kontrolować swój czat!

Pierwotnie opublikowane na [https://www.reddit.com/r/selfhosted/comments/poal79/simplex_chat_an_opensource_decentralized_chat/](https://www.reddit.com/r/selfhosted/comments/poal79/simplex_chat_an_opensource_decentralized_chat/)
