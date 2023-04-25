---
layout: layouts/article.html
title: "SimpleX announces SimpleX Chat mobile apps for iOS and Android"
date: 2022-03-08
preview: Brand new mobile apps with battle-tested Haskell core.
permalink: "/blog/20220308-simplex-chat-mobile-apps.html"
---

# SimpleX zapowiada aplikacje mobilne SimpleX Chat dla iOS i Androida

**Opublikowano:** 8 marca 2022 r.

## SimpleX Chat to pierwsza platforma czatowa, która z założenia jest w 100% prywatna - nie ma dostępu do Twojego wykresu połączeń.

Wydaliśmy już aplikacje na iPhone'a i Androida w [Apple AppStore](https://apps.apple.com/us/app/simplex-chat/id1605771084) i [Google Play Store](https://play.google.com/store/apps/details?id=chat.simplex.app), [APK dla Androida](https://github.com/simplex-chat/simplex-chat/releases/latest/download/simplex.apk) jest również dostępny do bezpośredniego pobrania.

**Zwróć uwagę**: obecna wersja jest obsługiwana tylko przez iPhone'a 8+ i Androida 10+ - planujemy dodać wsparcie dla iPada i starszych urządzeń bardzo szybko, i ogłosimy to na naszych kanałach [Reddit](https://www.reddit.com/r/SimpleXChat/) i [Twitter](https://twitter.com/SimpleXChat) - prosimy o subskrypcję, aby śledzić tam nasze aktualizacje.

## Czym jest SimpleX

Budujemy nową platformę dla rozproszonych aplikacji internetowych, gdzie prywatność wiadomości i sieci ma znaczenie.

Naszym celem jest zapewnienie najlepszej możliwej ochrony wiadomości i metadanych. Obecnie nie ma aplikacji do przesyłania wiadomości, która działa bez globalnej tożsamości użytkowników, więc wierzymy, że zapewniamy lepszą ochronę metadanych niż alternatywne rozwiązania. SimpleX został zaprojektowany jako prawdziwie rozproszony, bez centralnego serwera i bez globalnych tożsamości użytkowników. Pozwala to na wysoką skalowalność przy niskich kosztach, a także praktycznie uniemożliwia szpiegowanie grafu sieciowego.

Pierwszą aplikacją zbudowaną na platformie jest Simplex Chat. Platforma może z łatwością obsługiwać prywatny kanał sieci społecznościowej i wiele innych usług, które mogą być opracowane przez zespół Simplex lub deweloperów zewnętrznych.

Dalsze szczegóły dotyczące celów platformy i projektu technicznego są dostępne [w przeglądzie platformy SimpleX](https://github.com/simplex-chat/simplexmq/blob/master/protocol/overview-tjr.md).

## Dlaczego ją budujemy

Evgeny (założyciel SimpleX Chat): Pracowałem nad tą platformą przez długi czas, aby zapewnić miejsce, w którym wszyscy ludzie mogą swobodnie komunikować się ze sobą, bez obawy przed prześladowaniem z powodu tego, co powiedzieli i z kim są połączeni. Nie dzielenie się informacjami o swoich powiązaniach jest bardzo ważne, szczególnie dla ludzi żyjących w opresyjnych reżimach. Z powodu strasznego konfliktu między Rosją a Ukrainą, mieszkańcy obu krajów - mam tam przyjaciół i rodzinę - mogą być zagrożeni, gdy dzielą się swoimi opiniami lub po prostu z powodu bycia połączonym z osobami, które były ścigane. Każda aplikacja do obsługi komunikatorów, która wie, kim jesteś, może w końcu udostępnić wszystkie twoje połączenia niepożądanym osobom trzecim, czy to w wyniku nakazu sądowego, czy w wyniku ataku - więc nawet Signal, który ma silne szyfrowanie, nie może chronić twojego wykresu połączeń. Mam nadzieję, że nasz komunikator może pomóc ludziom żyjącym w opresyjnych reżimach w wyrażaniu swoich opinii bez obaw i ryzyka ścigania.

## Ogromne podziękowania dla naszych testerów!

Wielkie dzięki dla wszystkich, którzy pomogli w testowaniu i ulepszaniu aplikacji!

Jeśli masz zainstalowaną wersję [TestFlight](https://testflight.apple.com/join/DWuT2LQu) możesz dalej z niej korzystać.

Planujemy utrzymać ją tak stabilną, jak to tylko możliwe, a dzięki niej uzyskasz dostęp do wszystkich nowych funkcji 1-2 tygodnie wcześniej - jest ona ograniczona do 10 000 użytkowników, więc możesz ją pobrać, póki jest dostępna. Nadal możesz komunikować się z osobami, które używają wersji publicznej - jesteśmy zobowiązani do zachowania kompatybilności wstecznej.

Zawsze możesz zmigrować z publicznej wersji App Store do wersji TestFlight. Przeciwna migracja - z TestFlight do wersji publicznej - jest możliwa tylko wtedy, gdy mamy wydane te same wersje aplikacji, ponieważ zwykle istnieją pewne migracje baz danych, których nie można odwrócić. Aby zmigrować do wersji publicznej należy wyłączyć automatyczne aktualizacje na TestFlight, poczekać aż wersja publiczna dogoni i dopiero wtedy zainstalować ją z App Store. W każdym razie instalacja wersji publicznej jest bezpieczna, ale może się zawiesić, jeśli mamy nowszą wersję z TestFlight - w tym przypadku wystarczy przeinstalować aplikację z TestFlight i zainstalować wersję z App Store nieco później - nie stracilibyśmy żadnych danych.

## To nie wszystkie nowości - nasz kod rdzeniowy jest używany od dawna przez kilka tysięcy osób w naszej aplikacji terminalowej.

Aplikacje używają tego samego kodu rdzeniowego co nasza aplikacja terminalowa, który był używany i ustabilizowany przez długi czas, i zapewnia ten sam poziom prywatności i bezpieczeństwa, który jest dostępny od wydania v1 na początku tego roku:

- [double-rachet](https://www.signal.org/docs/specifications/doubleratchet/) Szyfrowanie E2E.
- oddzielne klucze dla każdego kontaktu.
- dodatkowa warstwa szyfrowania E2E w każdej kolejce wiadomości (aby zapobiec korelacji ruchu, gdy wiele kolejek jest używanych w konwersacji - coś, co planujemy jeszcze w tym roku).
- dodatkowe szyfrowanie wiadomości dostarczanych z serwerów do odbiorców (również w celu zapobiegania korelacji ruchu).

Więcej szczegółów technicznych można przeczytać w naszym ostatnim [ogłoszeniu v1](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/blog/20220112-simplex-chat-v1-released.md).

Wielkie podziękowania dla [@angerman](https://github.com/angerman) za umożliwienie skompilowania naszego kodu Haskella na platformy mobilne i zatwierdzenie go w sklepach z aplikacjami - to był nietrywialny projekt i wciąż trwa.

## Zainstaluj aplikacje i nawiąż prywatne połączenie!

Po zainstalowaniu aplikacji możesz połączyć się z każdym:

1. Utwórz swój lokalny profil czatu - nie jest on udostępniany serwerom SimpleX, jest lokalny dla Twoich urządzeń i zostanie udostępniony Twoim kontaktom w momencie połączenia.
2. Aby nawiązać prywatne połączenie, musisz stworzyć jednorazowy link do połączenia / kod QR za pomocą przycisku "Dodaj kontakt" w aplikacji. Możesz pokazać kod QR swojemu kontaktowi osobiście lub za pośrednictwem połączenia wideo - jest to najbezpieczniejszy sposób utworzenia połączenia - lub możesz udostępnić link za pośrednictwem dowolnego innego kanału - tylko jeden użytkownik może połączyć się za pośrednictwem tego linku.
3. Gdy inny użytkownik zeskanuje kod QR lub otworzy aplikację poprzez link (powinien również najpierw stworzyć swój profil), połączenie zostanie utworzone i będziesz mógł wysyłać zaszyfrowane wiadomości e2e prywatnie, bez niczyjej wiedzy, że jesteś połączony.

## Nowe funkcje i ulepszenia, które pojawią się wkrótce

- serwer powiadomień push. Obecnie aplikacje ładują wiadomości w tle cyklicznie, co na iOS może być dość rzadkie, jeśli nie otwierasz aplikacji regularnie. Dzięki powiadomieniom push wiedziałbyś o nowych wiadomościach natychmiast.
- e2e szyfrowane połączenia audio i wideo przez WebRTC.
- eksport i import bazy danych czatu.
- "odpowiedź na wiadomość" - funkcja pozwalająca zacytować wiadomość, na którą odpowiadasz.
- lokalizacja - damy Ci znać, gdy będziesz mógł wnieść tłumaczenia na swoje języki.
- konfigurowanie serwerów w aplikacjach - zostanie to wydane w tym tygodniu, zarówno dla iOS jak i Androida. Domyślnie aplikacje używają serwerów SimpleX Chat, ale będziecie mogli skonfigurować własne i nadal być połączeni z innymi użytkownikami, którzy używają naszej aplikacji z naszymi serwerami.
- zdjęcia profilowe użytkowników.
- wysyłanie obrazów i plików - podgląd obrazu będzie wysyłany przez serwery, więc może być asynchroniczny, a duże pliki / obrazy w pełnej rozdzielczości przez WebRTC, więc oba urządzenia będą musiały być online.

Daj nam znać, co jeszcze uważasz za ważne i jeśli znajdziesz jakieś błędy.
