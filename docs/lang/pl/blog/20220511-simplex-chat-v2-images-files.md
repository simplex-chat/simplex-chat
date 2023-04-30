---
layout: layouts/article.html
title: "SimpleX Chat v2.0 - sending images and files in mobile apps"
date: 2022-05-11
image: images/20220511-images-files.png
preview: Read how SimpleX delivers messages without having user profile identifiers of any kind.
permalink: "/blog/20220511-simplex-chat-v2-images-files.html"
---

# SimpleX Chat v2.0 - wysyłanie obrazów i plików w aplikacjach mobilnych

**Opublikowano:** 11 maja 2022 r.

## Nowość w wersji 2.0 - wysyłanie obrazów i plików prywatnie

Do wysyłania obrazu i plików SimpleX Chat używa komponentów systemowych chroniących prywatność, zarówno w aplikacjach na iOS, jak i na Androida. Nie pytamy o zgodę na dostęp do wielu lub wybranych plików, tak jak robią to np. Signal i Telegram - naraża to albo prywatność, albo wygodę.

Jak to działa? Dostęp do galerii i plików odbywa się z dostarczonego przez system dialogu, który działa w osobnym procesie i zapewnia tymczasowy URI, aby uzyskać dostęp do tylko jednego pliku wybranego przez użytkownika, tylko do czasu ponownego uruchomienia aplikacji.

Aby plik i obrazy działały w aplikacjach mobilnych, dokonaliśmy zmiany przełomowej w rdzeniu SimpleX Chat. Obecna wersja może wymieniać pliki z poprzednią wersją 1.6 aplikacji terminalowej, ale nie z wersją przed 1.6.

W aplikacji mobilnej, aby wysyłać i odbierać pliki oba urządzenia muszą mieć zainstalowaną wersję 2.0 - prosimy więc o sprawdzenie tego w swoich kontaktach. Odbieranie zdjęć działa w poprzedniej wersji, więc nawet jeśli Twoje kontakty nie zaktualizowały jeszcze aplikacji, powinny móc odbierać zdjęcia.

## Pierwsza platforma komunikacyjna, która nie posiada żadnych identyfikatorów użytkowników

Aby chronić tożsamość użytkowników i ich połączeń, SimpleX Chat nie posiada identyfikatorów użytkowników widocznych w sieci - w przeciwieństwie do każdej innej platformy komunikacyjnej.

Wiele osób pytało: _jeśli SimpleX nie ma identyfikatorów użytkowników, jak może dostarczać wiadomości?_

Aby dostarczyć wiadomości, zamiast identyfikatorów użytkowników używanych przez wszystkie inne platformy, SimpleX posiada identyfikatory dla kolejek wiadomości, oddzielne dla każdego z Twoich kontaktów. W obecnej wersji protokołu każda kolejka jest używana do momentu usunięcia kontaktu. Jeszcze w tym roku planujemy dodać rotację kolejek do protokołu klienta, aby nawet rozmowy nie miały długoterminowych identyfikatorów widocznych w sieci. Taka konstrukcja zapobiega wyciekowi jakichkolwiek metadanych użytkowników na poziomie aplikacji.

Ty definiujesz, którego serwera (serwerów) użyć **do odbierania** wiadomości, Twoje kontakty - serwery, których używasz **do wysyłania** wiadomości do nich. Oznacza to, że każda konwersacja będzie prawdopodobnie korzystać z dwóch różnych serwerów - po jednym dla każdego kierunku wiadomości.

Tylko urządzenia klienckie przechowują profile użytkowników, kontakty, grupy i wiadomości wysyłane z **2-warstwowym szyfrowaniem end-to-end**.

Więcej w [SimpleX whitepaper](https://github.com/simplex-chat/simplexmq/blob/master/protocol/overview-tjr.md).

## Jak połączyć się ze swoimi kontaktami w SimpleX Chat

Po zainstalowaniu aplikacji, możesz połączyć się z każdym:

1. Utwórz swój lokalny profil czatu - nie jest on udostępniany serwerom SimpleX. Jest on lokalny dla Twoich urządzeń i będzie udostępniany Twoim kontaktom dopiero w momencie połączenia.
2. Aby nawiązać prywatne połączenie, musisz stworzyć jednorazowe łącze do połączenia lub kod QR za pośrednictwem aplikacji. Możesz pokazać kod QR swojemu kontaktowi osobiście lub za pośrednictwem połączenia wideo - jest to najbezpieczniejszy sposób utworzenia połączenia - lub możesz udostępnić łącze za pośrednictwem dowolnego innego kanału. Tylko jeden użytkownik może połączyć się za pośrednictwem tego łącza.
3. Gdy inny użytkownik zeskanuje kod QR lub otworzy aplikację za pośrednictwem linku, połączenie zostanie utworzone i będziesz mógł wysyłać prywatnie zaszyfrowane wiadomości typu end-to-end, bez wiedzy kogokolwiek o tym, że jesteś połączony.

<img src="https://github.com/simplex-chat/simplex-chat/blob/stable/images/conversation.png" alt="Nawiąż prywatne połączenie" width="594" height="360">
