# Prywatność i bezpieczeństwo

Domyślna konfiguracja SimpleX Chat ma na celu zrównoważenie prywatności, bezpieczeństwa i wygody. Możesz chcieć zmienić opcje domyślne.

Na tej stronie wymieniono wszystkie funkcje i opcje, które wpływają na prywatność i bezpieczeństwo.

## Ustawienia prywatności i bezpieczeństwa

Te ustawienia są dostępne w [Ustawieniach prywatności i bezpieczeństwa](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/docs/guide/app-settings.md#privacy-and-security).

## Weryfikacja kodu bezpieczeństwa

<img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20230103-verification.png" width="288">

Choć SimpleX Chat zawsze nawiązuje połączenie poprzez link przekazywany niezależnym kanałem, więc jest już bardziej zabezpieczony niż inne aplikacje, to istnieją scenariusze, w których linki zaproszenia mogą zostać podmienione w tranzycie (atak MITM). Aby zabezpieczyć się przed takimi atakami, należy zweryfikować kod bezpieczeństwa z kontaktami:

- otworzyć rozmowę z kontaktem
- stuknij nazwę kontaktu w górnej części rozmowy
- stuknij "Zweryfikuj kod bezpieczeństwa"
- poproś swój kontakt o zrobienie tego samego
- połączenie jest bezpieczne, jeśli Ty i Twój kontakt macie ten sam kod bezpieczeństwa

Można to sprawdzić w jeden z następujących sposobów:

- jedno z Was może zeskanować kod bezpieczeństwa ze swojego urządzenia, jeśli kody się zgadzają kontakt zostanie oznaczony jako zweryfikowany na urządzeniu, które zeskanowało kod.
- wystarczy dotknąć opcji zaznacz zweryfikowane, jeśli ufasz potwierdzeniu od kontaktu, że kod jest zweryfikowany.
- możesz również odczytać kontakt przez połączenie głosowe.

Czytaj więcej w [tym poście](
https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/blog/20230103-simplex-chat-v4.4-disappearing-messages.md#connection-security-verification)

## Hasło bazy danych

Po zainstalowaniu aplikacja generuje losowe hasło dla bazy danych czatu i przechowuje je bezpiecznie w KeyChain (iOS) lub za pomocą KeyStore (Android, moduł TPM jest używany, gdy jest dostępny). Możesz ustawić własne hasło, a także usunąć je z urządzenia, w którym to przypadku będziesz musiał je wprowadzić przy każdym uruchomieniu aplikacji, a powiadomienia mogą być ograniczone, w ustawieniach [Hasło do bazy danych i eksport](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/docs/guide/managing-data.md#database-passphrase).

## Tryb incognito

Ta funkcja generuje losową nazwę profilu dla każdego nowego kontaktu. Przeczytaj więcej w [Tryb incognito](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/docs/guide/app-settings.md#incognito).

## Ukryte profile

Ta funkcja pozwala ukryć niektóre profile czatu za pomocą hasła. Przeczytaj więcej w [Ukrywanie i wyciszanie profili czatu](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/docs/guide/chat-profiles.md#hiding-and-muting-chat-profiles).

## Ustawienia sieci.

[Izolacja transportu (BETA)](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/docs/guide/app-settings.md#transport-isolation-beta) pozwala na izolowanie ruchu z każdym kontaktem w innym połączeniu TCP (i obwodzie Tor).

## Używanie Tor

<img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20220808-tor1.png" width="330"> &nbsp; <img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20220808-tor2.png" width="330">

Aby połączyć się z przekaźnikami SMP (serwerami wiadomości) poprzez Tor musisz zainstalować aplikację Orbot.

Android: użyj aplikacji Orbot jako SOCKS proxy na porcie 9050 (domyślnie) i włącz [Użyj proxy SOCKS](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/docs/guide/app-settings.md#use-socks-proxy-android-only).

iOS: użyj aplikacji Orbot jako dostawcy VPN i włącz VPN.

Możesz również zmienić, które adresy serwerów są używane dzięki opcji [Użyj hostów .onion](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/docs/guide/app-settings.md#use-onion-hosts).
