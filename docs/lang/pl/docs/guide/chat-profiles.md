# Twoje profile czatu

## Tworzenie dodatkowych profili czatu

SimpleX Chat umożliwia tworzenie dowolnej liczby profili czatu. Tak samo jak pierwszy profil, są one przechowywane tylko lokalnie na urządzeniu.


<img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20230204-profiles2.png" width="288">

Aby utworzyć dodatkowy profil SimpleX Chat:

- [Otwórz ustawienia aplikacji](./app-settings.md#opening-the-app-settings).
- Wybierz opcję "Twoje profile czatu".
- Odblokuj opcję poprzez odcisk palca lub PIN.
- Stuknij w "+ Dodaj profil".
- Utwórz nowy profil, wprowadzając swoją nazwę wyświetlaną i pełną nazwę (opcjonalnie).
- Stuknij w "Utwórz".

## Ukrywanie i wyciszanie profili czatu

W wersji 4.6 dodano możliwość wyciszania i ukrywania profili czatu.

<img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20230328-hidden-profiles1.png" width="288"> &nbsp;&nbsp; <img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20230328-hidden-profiles2.png" width="288"> &nbsp;&nbsp; <img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20230328-hidden-profiles3.png" width="288">

Te działania są dostępne poprzez długie naciśnięcie (Android) lub przeciągnięcie (iOS) na profilu na liście.

Aby odsłonić ukryte profile, wpisz pełne hasło w pasku wyszukiwania.

## Przełączanie między profilami

<img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20230204-profiles1.png" width="288">

- Dotknij obrazu swojego profilu użytkownika w prawym górnym rogu ekranu.
- Wybierz profil, którego chcesz używać.

Możesz również przełączyć profil za pomocą opcji Twoje profile czatu w ustawieniach.

## Tryb incognito

<img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20220901-incognito1.png" width="330"> <img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20220901-incognito2.png" width="330"> <img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20220901-incognito3.png" width="330">

Ta funkcja jest unikalna dla SimpleX Chat - jest niezależna od profili czatu.

Gdy "Tryb Incognito" jest włączony, aktualnie wybrana nazwa profilu i obraz są ukryte przed nowymi kontaktami. Pozwala to na anonimowe połączenia z innymi osobami bez żadnych wspólnych danych - gdy nawiązujesz nowe połączenia lub dołączasz do grup za pośrednictwem łącza, dla każdego połączenia zostanie wygenerowana nowa losowa nazwa profilu.

Aby włączyć/wyłączyć tryb incognito:

- [Otwórz ustawienia aplikacji](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/docs/guide/app-settings.md#opening-the-app-settings).
- Włącz/wyłącz tryb incognito stukając w przełącznik na "Incognito".

Przeczytaj więcej w [tym poście](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/blog/20220901-simplex-chat-v3.2-incognito-mode.md#incognito-mode).

## Edytuj swój profil

Aby edytować swój profil:

- [Otwórz ustawienia aplikacji](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/docs/guide/app-settings.md#opening-the-app-settings).
- Wybierz swój profil.
- Stuknij w "Edytuj".
- Wprowadź żądaną nazwę profilu i/lub zaktualizuj swoje pełne imię i nazwisko.
- Możesz również ustawić i zmienić zdjęcie profilowe.
- Stuknij w "Zapisz i powiadom kontakty".

## Przenoszenie profili czatu na inne urządzenie

SimpleX Chat przechowuje wszystkie dane użytkownika tylko na urządzeniach klienckich przy użyciu przenośnego formatu zaszyfrowanej bazy danych, który można wyeksportować i przenieść na dowolne obsługiwane urządzenie.

Aby wyeksportować swoje dane z SimpleX Chat:

- [Otwórz ustawienia aplikacji](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/docs/guide/app-settings.md#opening-the-app-settings).
- Wybierz "Hasło do bazy danych i eksport".
- Zatrzymaj czat, przełączając opcję "Chat is running" (dotknij "Stop" w oknie dialogowym potwierdzenia).
- Potwierdź to za pomocą odcisku palca lub kodu PIN, jeśli masz włączony [Blokada SimpleX](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/docs/guide/app-settings.md#simplex-lock).
- Jeśli nie ustawiłeś go wcześniej, [ustaw hasło](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/docs/guide/managing-data.md#database-passphrase) w "Database passphrase". Początkowo baza danych jest szyfrowana losowym hasłem, które jest przechowywane w KeyChain (iOS) lub z KeyStore (Android).
- Stuknij w "Eksportuj bazę danych" - nie pozwoli na eksport, jeśli nie ustawisz hasła.
- Zapisz plik na swoim urządzeniu lub udostępnij go za pomocą dowolnej dostępnej opcji.

Aby zaimportować swoje dane z SimpleX Chat w aplikacji na innym urządzeniu:

- Przenieś plik bazy danych na nowe urządzenie.
- Zainstaluj aplikację SimpleX Chat.
- Utwórz profil czatu o dowolnej nazwie, wykonując kroki w [Utwórz swój pierwszy profil czatu](#create-yout-first-chat-profile) - wkrótce go wymienisz.
- Otwórz ustawienia aplikacji
- Wybierz "Hasło do bazy danych i eksport".
- Zatrzymaj czat stukając w przełącznik "Czat jest uruchomiony".
- Stuknij w "Importuj bazę danych".
- Wybierz plik .zip z wyeksportowanymi danymi czatu.
- W oknie dialogowym potwierdzenia stuknij "Importuj".
- Uruchom czat za pomocą przełącznika lub zamknij i uruchom aplikację - zostaniesz poproszony o wprowadzenie hasła do bazy danych czatu.

**Zwróć uwagę**:

1. Obecnie nie można przenieść niektórych profili, można przenieść tylko całą bazę danych zawierającą wszystkie profile.

2. NIE wolno używać wyeksportowanej bazy danych na więcej niż jednym urządzeniu jednocześnie, ponieważ może to zakłócić niektóre połączenia. Musisz również zawsze używać najnowszej wersji bazy danych czatu - używanie starej wersji również może zakłócić połączenia.

3. Nie ma możliwości odzyskania utraconego hasła - upewnij się, że przechowujesz go bezpiecznie.
