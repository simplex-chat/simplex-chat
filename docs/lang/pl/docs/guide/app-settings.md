# Ustawienia aplikacji

## Otwieranie ustawień aplikacji

Aby otworzyć ustawienia aplikacji:

- Otwórz aplikację.
- Stuknij obraz swojego profilu użytkownika w prawym górnym rogu ekranu.
- Jeśli masz więcej niż jeden profil, stuknij ponownie w bieżący profil lub wybierz Ustawienia.

## Ustawienia Twojego profilu

Ta sekcja jest oznaczona **"Ty"** w ustawieniach aplikacji.

### Twój obecny profil

Stuknij swój awatar/nazwę, aby zaktualizować bieżące nazwy profilu i awatara.

Nazwa wyświetlana nie może mieć żadnych spacji i zaleca się używanie znaków łacińskich i cyfr, aby ułatwić wpisywanie tych nazw użytkownikom, którzy używają [SimpleX Chat dla terminalu](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/docs//CLI.md) (CLI).

**Zwróć uwagę**: Gdy zapiszesz swój profil, aktualizacja zostanie wysłana do wszystkich Twoich kontaktów (z wyłączeniem kontaktów, z którymi były udostępnione Twoje profile incognito). Jeśli masz dużą liczbę kontaktów może to potrwać kilka sekund.

### Twoje profile czatu

Ta strona umożliwia dodawanie i konfigurowanie profili czatu. Więcej szczegółów można znaleźć w [Twoje profile czatu](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/docs/guide/chat-profiles.md).

### Incognito

Ta funkcja jest unikalna dla SimpleX Chat - jest niezależna od profili czatu.

Kiedy "Incognito" jest włączone, Twoja aktualna nazwa profilu i obraz NIE są udostępniane nowym kontaktom. Pozwala to na połączenia z innymi osobami bez udostępniania danych - gdy nawiązujesz nowe połączenia lub dołączasz do grup za pośrednictwem łącza, nowa losowa nazwa profilu zostanie wygenerowana dla każdego kontaktu lub grupy.

Przeczytaj więcej w [tym poście](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/blog/20220901-simplex-chat-v3.2-incognito-mode.md#incognito-mode).

### Twój adres kontaktowy SimpleX

<img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20221108-address1.png" width="288"> &nbsp;&nbsp; <img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20221108-address2.png" width="288">

Ta strona pozwala na stworzenie długoterminowego adresu, który może być używany przez inne osoby, aby się z Tobą połączyć. W przeciwieństwie do jednorazowych linków zapraszających, adresy te mogą być używane wielokrotnie, co czyni je dobrymi do udostępniania w sieci, np. na innych platformach mediów społecznościowych.

Gdy ludzie połączą się z Tobą za pośrednictwem tego adresu, otrzymasz prośbę o połączenie, którą możesz zaakceptować lub odrzucić. Możesz skonfigurować automatyczną akceptację prośby o połączenie i automatyczną wiadomość powitalną, która zostanie wysłana do nowych kontaktów.

Jeśli zaczniesz otrzymywać zbyt wiele próśb o połączenie za pośrednictwem tego adresu, zawsze bezpiecznie jest go usunąć - wszystkie połączenia, które utworzyłeś za pośrednictwem tego adresu pozostaną aktywne, ponieważ ten adres nie jest używany do dostarczania wiadomości.

Czytaj więcej w [tym poście](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/blog/20221108-simplex-chat-v4.2-security-audit-new-website.md#auto-accept-contact-requests).

### Preferencje czatu

Ta strona pozwala skonfigurować preferencje czatu, które miałyby zastosowanie do wszystkich kontaktów - można je zmienić osobno dla każdego kontaktu:

- _znikające wiadomości_ - aby zezwolić na znikające wiadomości w rozmowach z Tobą, tylko jeśli Twoje kontakty na to pozwalają.
- _usuń dla wszystkich_ - aby zezwolić na nieodwracalne usuwanie wiadomości (domyślnie wiadomości są oznaczone jako usunięte, nie są w pełni usunięte). "Tak" ustawienie pozwalałoby na to tylko wtedy, gdy pozwalają na to Tobie, a "Zawsze" - nawet jeśli nie pozwalają.
- _wiadomości głosowe_ - aby zezwolić na wysyłanie wiadomości głosowych.

Aby ustawić preferencje czatu w każdym kontakcie, stuknij w nazwę kontaktu na górze rozmowy, a następnie wybierz "Preferencje kontaktu".

Właściciele grup mogą ustawić podobne preferencje dla swoich grup, gdy grupa jest tworzona lub później: stuknij nazwę grupy na górze rozmowy, a następnie wybierz "Preferencje grupy".

## Ustawienia aplikacji

Ta sekcja jest oznaczona jako **"Ustawienia"** w ustawieniach aplikacji.

### Powiadomienia

Ta strona pozwala skonfigurować tryb powiadomień: natychmiastowy, okresowy lub tylko wtedy, gdy aplikacja jest uruchomiona. Istnieją pewne różnice w działaniu tych opcji na iOS i Androidzie. Przeczytaj więcej w [tym poście](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/blog/20220404-simplex-chat-instant-notifications.md) o tym, jak działają powiadomienia.

Możesz również skonfigurować, czy podgląd wiadomości jest wyświetlany po jej nadejściu.

**Zwróć uwagę**: Aby powiadomienia natychmiastowe i okresowe działały w systemie Android, musisz wyłączyć optymalizację zasilania, gdy zostaniesz o to poproszony przez aplikację lub później za pośrednictwem ustawień. Ponadto niektóre warianty systemu Android wymagają dodatkowych ustawień, np. system MIU na telefonach Xiaomi wymaga włączenia "Auto start" dla aplikacji, aby usługa powiadomień działała. Proszę przeczytać [Nie zabijaj mojej aplikacji](https://dontkillmyapp.com/) przewodnik dla wszelkich ustawień, które mogą być wymagane na Twoim urządzeniu.

Ponadto, obecnie powiadomienia natychmiastowe mają najwyższe zużycie baterii - pracujemy nad zmniejszeniem go, aby było mniejsze lub takie samo jak w przypadku powiadomień okresowych.

### Sieć i serwery

Ta strona pozwala na skonfigurowanie własnych przekaźników SMP i zmianę innych ustawień sieciowych.

<img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20230204-transport.png" width="288">

#### Serwery SMP

<img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20221206-server1.png" width="288"> &nbsp;&nbsp; <img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20221206-server2.png" width="288"> &nbsp;&nbsp; <img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20221206-server3.png" width="288">

Domyślnie aplikacja ma skonfigurowane wstępnie ustawione przekaźniki - można je zmienić na własne.

Na tej stronie można również przetestować połączenie z serwerami.

Więcej w [tym poście](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/blog/20221206-simplex-chat-v4.3-voice-messages.md#smp-servers-configuration-and-password).

#### Użyj proxy SOCKS (tylko na Android)

Ta opcja sprawia, że aplikacja łączy się za pośrednictwem proxy SOCKS, które powinno być zapewnione przez inną aplikację działającą na Twoim urządzeniu.

Najbardziej typowym zastosowaniem tej opcji jest uruchomienie aplikacji Orbot, która zapewnia proxy SOCKS do łączenia się przez sieć Tor, ale może to być również inna aplikacja i może ona pośredniczyć w połączeniach przez inną sieć nakładkową.

#### Użyj hostów .onion

##### Android

Opcja **Użyj hostów .onion** jest dostępna tylko wtedy, gdy włączona jest opcja **Użyj proxy SOCKS**. Można wybrać:

- _nie_: nigdy nie używaj hostów .onion. Wybierz tę opcję, jeśli twoje SOCKS proxy nie łączy się przez sieć Tor.
- _gdy dostępny_ (domyślnie): gdy SOCKS proxy jest włączone, aplikacja zakłada, że zapewnia połączenia przez sieć Tor i używa adresów hostów .onion, gdy przekaźniki SMP uwzględniają je w swojej konfiguracji.
- _wymagane_: zawsze używaj hostów .onion. Wybierz tę opcję, jeśli twój SOCKS proxy łączy się przez sieć Tor i chcesz uniknąć połączeń bez sieci Tor. W tym przypadku, jeśli adres przekaźnika SMP nie zawiera .onion host, połączenie nie powiedzie się.

##### iOS

<img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20220901-onion1.png" width="330"> &nbsp; <img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20220901-onion2.png" width="330">

Podczas gdy iOS nie obsługuje proxy SOCKS, można zainstalować aplikację Orbot, która działa jako dostawca VPN. Możesz wybrać:

- _nie_ (domyślnie): nie używaj hostów .onion. Wybierz tę opcję, jeśli nie używasz Orbota lub jeśli używasz VPN, który proksuje połączenia przez inną sieć nakładkową.
- _gdy dostępny_: używaj adresów hostów .onion, gdy przekaźniki SMP zawierają je w swojej konfiguracji. Orbot VPN musi być włączony, aby ta opcja działała.
- _wymagane_: zawsze używaj hostów .onion. Wybierz tę opcję, jeśli używasz Orbot VPN i chcesz uniknąć połączeń bez Tora. W tym przypadku, jeśli adres przekaźnika SMP nie zawiera .onion host, połączenie nie powiedzie się. Jeśli używasz tej opcji, możesz włączyć opcję "Disable Orbot for non-onion traffic" w ustawieniach Orbota sprawiając, że reszta ruchu w Twoim urządzeniu nie będzie korzystać z Tora.

**Zwróć uwagę** VPN na iOS może wysyłać część ruchu do zwykłej sieci, jeśli np. aplikacja VPN ulegnie awarii. Można skonfigurować włączenie zawsze włączonego trybu VPN na zarządzanych urządzeniach iOS, ale nie ma to zastosowania do większości pojedynczych urządzeń.

#### Izolacja transportu (BETA)

Ta opcja jest dostępna tylko wtedy, gdy masz włączone narzędzia deweloperskie.

Przeczytaj szczegóły w [tym poście](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/blog/20230204-simplex-chat-v4-5-user-chat-profiles.md#transport-isolation).

#### Zaawansowane ustawienia sieci

<img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20220808-network.png" width="330">

Jeśli Twoja sieć jest powolna, a połączenia z serwerami ciągle zawodzą (widziałbyś na swoich kontaktach obrotówkę), proszę zwiększyć timeouty TCP i protokołu na tej stronie.

### Prywatność i bezpieczeństwo

#### Blokada SimpleX

Kiedy Blokada SimpleX jest włączona, wymaga przejścia przez uwierzytelnianie urządzenia, gdy otwierasz aplikację lub używasz niektórych funkcji wrażliwych na bezpieczeństwo lub prywatność.

Zostanie Ci zaproponowane włączenie jej po kilkukrotnym otwarciu aplikacji.

Aby włączyć ją później:

- [Otwórz ustawienia aplikacji](#opening-the-app-settings).
- Stuknij w "Prywatność i bezpieczeństwo".
- Przełącz przełącznik "Blokada SimpleX".
- Potwierdź swoje dane uwierzytelniające telefonu.

Gotowe! Teraz będziesz musiał uwierzytelnić się przy uruchamianiu lub wznawianiu aplikacji po 30 sekundach w tle.

#### Chroń ekran aplikacji

<img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20221206-protect.png" width="330">

Ta opcja ukrywa ekran aplikacji w ostatnich aplikacjach - jest domyślnie włączona. Na Androidzie zapobiega również wykonywaniu zrzutów ekranu.

#### Automatyczne akceptowanie obrazów

Automatyczne akceptowanie obrazów może zmniejszyć prywatność - Twoje kontakty będą wiedziały, że jesteś online.

- [Otwórz ustawienia aplikacji](#opening-the-app-settings).
- Stuknij w "Prywatność i bezpieczeństwo".
- Przełącz przełącznik "Automatyczne akceptowanie obrazów".

#### Wysyłanie podglądów linków

Włączenie wysyłania podglądów linków może zmniejszyć prywatność - Twoja aplikacja będzie ładować podgląd linków ze strony internetowej.

- [Otwórz ustawienia aplikacji](#opening-the-app-settings).
- Stuknij w "Prywatność i bezpieczeństwo".
- Przełącz przełącznik "Wyślij podgląd linku".

#### Linki SimpleX

Ta opcja wpływa na to, jak w rozmowach pokazywane są linki do łączenia się z innymi użytkownikami SimpleX lub do dołączania do grup. Możesz wybrać pomiędzy:

- _opis_ (domyślnie): pokazywany jest tylko opis linku i nazwa hosta serwera. Link nie zostanie otwarty w przeglądarce.
- _pełny link_: pokazywany jest pełny link. Link nadal nie będzie otwierany w przeglądarce.
- _przez przeglądarkę_: pokazany jest pełny link, który zostanie otwarty w przeglądarce. W tym przypadku, jeśli domena linku jest inna niż simplex.chat, link zostanie pokazany w czerwonym kolorze, ponieważ może być złośliwy.

Przeczytaj więcej o [bezpieczeństwie linków SimpleX](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/blog/20221206-simplex-chat-v4.3-voice-messages.md#privacy-and-security-of-simplex-invitation-links).

### Wygląd

Ta strona umożliwia konfigurację:

- język interfejsu
- ikona aplikacji
- kolor akcentu

### Hasło do bazy danych i eksport

Ta strona pozwala na zmianę hasła bazy danych, eksport i import bazy danych oraz konfigurację okresu przechowywania wiadomości.

Przeczytaj więcej na stronie [Zarządzanie danymi](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/docs/guide/managing-data.md).

## Pomoc

Ta sekcja zawiera informacje o tym, jak korzystać z aplikacji i linki do połączenia z zespołem. Proszę użyć [Wyślij pytania i pomysły](https://simplex.chat/pl/contact#/?v=1&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2FK1rslx-m5bpXVIdMZg9NLUZ_8JBm8xTt%23%2F%3Fv%3D1%26dh%3DMCowBQYDK2VuAyEALDeVe-sG8mRY22LsXlPgiwTNs9dbiLrNuA7f3ZMAJ2w%253D%26srv%3Dbylepyau3ty4czmn77q4fglvperknl4bi2eb2fdy2bh4jxtf32kf73yd.onion), aby połączyć się z nami za pośrednictwem czatu, aby zadać wszelkie pytania, przekazać wszelkie sugestie i zgłosić wszelkie problemy.

## Wspieraj SimpleX Chat

- przyczyń się - link do informacji o tym, jak wnieść wkład i darowiznę do projektu.
- oceń aplikację - oceń i zrecenzuj ją w App Store lub Play Store - Twoja opinia bardzo pomaga.
- daj gwiazdkę na GitHubie - to również bardzo pomaga nam się rozwijać.

Dziękujemy za wsparcie!

## Narzędzia deweloperskie

Ta strona ma opcje, które zwykle są potrzebne tylko dla deweloperów aplikacji i mogą być używane do debugowania aplikacji w przypadku, gdy coś nie działa.

### Konsola czatu

Tutaj możesz zobaczyć i użyć komend konsoli z rdzeniem czatu. Żadna z tych informacji nie jest wysyłana przez sieć, jest to wewnętrzna komunikacja pomiędzy różnymi częściami aplikacji.

Proszę uważać - niektóre z komend mogą zakłócić działanie aplikacji, używaj ich tylko jeśli wiesz co robisz lub zostałeś poinstruowany przez zespół.

**Zwróć uwagę**: log w konsoli może zawierać coś, co wygląda jak błędy. Jeśli nie doświadczysz żadnych problemów w UI aplikacji, te błędy nie powinny być traktowane jako awaria aplikacji - są one prawdopodobnie normalnym i oczekiwanym zachowaniem.

### Potwierdź aktualizację bazy danych

Ta opcja jest domyślnie wyłączona - baza danych aplikacji jest migrowana do nowej wersji bez konieczności potwierdzania. Począwszy od v4.6.2 migracje te są odwracalne - możesz cofnąć się do poprzedniej wersji (nie wcześniej niż v4.6.1) aplikacji. Jeśli chcesz być proszony o potwierdzenie przy każdej aktualizacji bazy danych, możesz włączyć tę opcję - nie jest to zalecane, ponieważ dzieje się to prawie przy każdej aktualizacji aplikacji, a nie powinno powodować żadnych obaw.

### Pokaż opcje deweloperskie

Opcja włącza opcję [Tryb izolacji transportu](#transport-izolacji-beta) oraz włącza pokazywanie ID bazy danych w kontaktach, grupach i członkach grup, aby ułatwić debugowanie za pomocą poleceń konsoli.
