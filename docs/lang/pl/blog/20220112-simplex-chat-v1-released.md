---
layout: layouts/article.html
title: "SimpleX announces SimpleX Chat v1"
date: 2022-01-12
preview: Major protocol changes address all design mistakes identified during concept review by an independent expert.
permalink: "/blog/20220112-simplex-chat-v1-released.html"
---

# SimpleX zapowiada SimpleX Chat v1.

**Opublikowano:** 12 stycznia, 2022

## Najbardziej prywatna i bezpieczna platforma czatu i aplikacji

Budujemy nową platformę dla rozproszonych aplikacji internetowych, gdzie prywatność wiadomości i sieci w ogóle ma znaczenie. [SimpleX Chat](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/README.md) to nasza pierwsza aplikacja, aplikacja do przesyłania wiadomości zbudowana na platformie SimpleX.

## Czym jest SimpleX?

Obecnie nie ma aplikacji do przesyłania wiadomości, która szanowałaby prywatność użytkownika i gwarantowała prywatność metadanych - innymi słowy, wiadomości mogą być prywatne, ale osoba trzecia zawsze może zobaczyć, kto z kim się komunikuje, badając centralną usługę i graf połączeń. SimpleX, w swej istocie, jest zaprojektowany jako prawdziwie rozproszony, bez centralnego serwera. Pozwala to na ogromną skalowalność przy niskich kosztach, a także praktycznie uniemożliwia szpiegowanie grafu sieciowego.

Pierwszą aplikacją zbudowaną na platformie jest Simplex Chat, który na razie działa w oparciu o terminal (wiersz poleceń), a w przygotowaniu są aplikacje mobilne. Platforma może z łatwością obsługiwać prywatny kanał społecznościowy i wiele innych usług, które mogą być opracowane przez zespół Simplex lub deweloperów zewnętrznych.

## Co nowego w v1?

### Stabilna implementacja protokołu

Wszystkie wydania od v1 będą kompatybilne wstecz i do przodu.

### Szyfrowanie wiadomości zostało całkowicie przeprojektowane, aby zapewnić tajemnicę w przód i odzyskanie po włamaniu.

SimpleX Chat v1 używa teraz:

- [double-ratchet](https://www.signal.org/docs/specifications/doubleratchet/) Szyfrowanie E2E przy użyciu szyfru AES-256-GCM z [X3DH key agreement](https://www.signal.org/docs/specifications/x3dh/) przy użyciu 2 efemerycznych kluczy Curve448 w celu uzyskania sekretów dla inicjalizacji ratchet. Te klucze i sekrety są oddzielne dla każdego kontaktu, członkostwa w grupie i transferu plików.
- oprócz podwójnego ratchet, istnieje osobne szyfrowanie E2E w każdej kolejce wiadomości z wymianą klucza DH przy użyciu Curve25519 i [NaCl crypto-box](https://nacl.cr.yp.to/index.html) - osobne szyfrowanie E2E zostało dodane, aby uniknąć posiadania jakiegokolwiek wspólnego tekstu szyfrującego pomiędzy kolejkami wiadomości jednego kontaktu (aby zapobiec korelacji ruchu).
- dodatkowe szyfrowanie wiadomości dostarczanych z serwerów do odbiorców, również z wykorzystaniem wymiany DH Curve25519 i NaCl crypto-box - aby uniknąć wspólnego szyfrogramu w ruchu wysyłanym i odbieranym (również aby zapobiec korelacji ruchu).

### Ulepszone uwierzytelnianie i transport użytkowników i serwerów

SimpleX używa teraz efemerycznych kluczy Ed448 do podpisywania i weryfikacji poleceń klientów do serwerów. Podobnie jak wcześniej, klucze te są różne dla każdej kolejki wiadomości i nie reprezentują tożsamości użytkownika.

Zamiast szyfrowanego transportu ad-hoc używamy teraz TLS 1.2+ ograniczonego do najbardziej wydajnego i bezpiecznego szyfru z zabezpieczeniem przed przeciążeniem (ECDHE-ECDSA-CHACHA20POLY1305-SHA256), grup Curve448 i kluczy Ed448.

Tożsamość serwera jest walidowana w ramach TLS handshake - odcisk palca certyfikatu serwera offline jest używany jako stała tożsamość serwera, która jest zawarta w adresie serwera, w celu ochrony przed atakami MITM pomiędzy klientami i serwerami.

SimpleX używa również [tls-unique channel binding](https://datatracker.ietf.org/doc/html/rfc5929#section-3) w każdym podpisanym poleceniu klienta do serwera, aby chronić przed atakami replay.

### Zmiany w kodowaniu protokołu

Przeszliśmy z nieefektywnego tekstowego kodowania protokołu niskiego poziomu, które uprościło wczesny rozwój, na wydajne pod względem przestrzeni i wydajności kodowanie binarne, zmniejszając narzut protokołu z około 15% do 3,7% wielkości przesyłanych wiadomości aplikacji.

## Dowiedz się więcej o Simplex

Dalsze szczegóły dotyczące celów platformy i projektu technicznego są dostępne [tutaj](https://github.com/simplex-chat/simplexmq/blob/master/protocol/overview-tjr.md).

Klient SimpleX Chat może być używany w terminalu na wszystkich głównych platformach desktopowych (Windows/Mac/Linux), a także na urządzeniach z systemem Android dzięki [Termux](https://github.com/termux).

SimpleX pozwala również na hostowanie własnych serwerów i posiadanie własnych danych czatu. Serwery SimpleX są wyjątkowo lekkie i wymagają pojedynczego procesu z początkowym śladem pamięci poniżej 20 Mb, który rośnie wraz z dodawaniem przez serwer kolejek w pamięci (nawet z 10,000 kolejek używa mniej niż 50Mb, nie licząc wiadomości).

## Nie możemy się doczekać, aż go użyjesz!

Czekamy na Wasze opinie i sugestie - poprzez problemy na GitHubie lub poprzez SimpleX Chat - możecie połączyć się z zespołem za pomocą komendy `/simplex` po uruchomieniu czatu.
