# Połączenia audio i wideo

SimpleX Chat umożliwia wykonywanie szyfrowanych end-to-end połączeń audio i wideo z kontaktami za pośrednictwem WebRTC. Uwaga: Połączenia grupowe nie są w tym momencie obsługiwane.

## Wykonywanie i odbieranie połączeń

### Jak nawiązać połączenie audio.

1. Stuknij w kontakt.
2. Stuknij w ikonę telefonu po prawej stronie nazwy kontaktu w górnej części ekranu.

### Jak wykonać połączenie wideo

1. Stuknij w kontakt.
2. Stuknij w trzy pionowe kropki w prawym górnym rogu ekranu, aby uzyskać dostęp do większej liczby opcji.
3. Wybierz **Połączenie wideo**.

### Odbieranie połączeń

Gdy jest połączenie przychodzące, masz trzy opcje:

- akceptuj: aby połączyć połączenie
- odrzucić: aby odrzucić połączenie, _bez_ powiadamiania dzwoniącego.
- zignoruj: aby tymczasowo odrzucić połączenie, ale w taki sposób, aby można je było przyjąć później, jeśli rozmówca nadal czeka, poprzez wiadomość **Akceptuj połączenie** w rozmowie z tym kontaktem.

Nie ma ograniczenia czasowego, w którym zaproszenie do połączenia może pozostać aktywne - dopóki rozmówca nadal czeka, można przyjąć połączenie w dowolnym momencie później.

Połączenie można zaakceptować z ekranu blokady, zarówno na Androidzie (trzeba to włączyć w opcjach), jak i na iOS (domyślnie, korzystając z natywnego interfejsu połączeń iOS, który można wyłączyć).

### Połączenia na ekranie blokady w systemie Android

SimpleX Chat domyślnie pokazuje połączenie przychodzące na ekranie blokady Twojego urządzenia. Możesz jednak zmienić to zachowanie w menu ustawień aplikacji.

1. Otwórz menu ustawień aplikacji.
2. Dotknij **Połączenia audio i wideo**.
3. Na liście rozwijanej **Połączenia na ekranie blokady** wybierz jedną z trzech następujących opcji:
   - Wyłącz - połączenie będzie wyświetlane jako powiadomienie.
   - Pokaż - połączenie pojawi się na ekranie blokady, aby je zaakceptować, musisz odblokować urządzenie i aplikację.
   - Akceptuj - połączenie można zaakceptować i odrzucić bezpośrednio z ekranu blokady, bez otwierania aplikacji.

**Uwaga**: niektóre systemy/urządzenia z Androidem zabraniają wyświetlania pełnoekranowych widoków na ekranie blokady - w takim przypadku połączenie pokaże się jako zwykłe powiadomienie.

### Połączenia na ekranie blokady na iOS

<img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20230328-call1.png" width="288">

Domyślnie SimpleX Chat używa natywnego interfejsu połączeń iOS, gdzie jest to dozwolone, aby pokazać połączenia przychodzące na ekranie blokady. Możesz to wyłączyć:

1. Otwórz menu ustawień aplikacji.
2. Stuknij **Połączenia audio i wideo**.
3. Wyłącz przełącznik **Użyj interfejsu połączeń iOS**.

**Zwróć uwagę**: Interfejs połączeń systemu iOS umożliwia odbieranie połączeń bez odblokowywania urządzenia i aplikacji. Jeśli jest to niepożądane, wyłącz go - połączenia będą w tym przypadku pokazywane jako powiadomienia.

Więcej przeczytacie w [tym poście](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/blog/20230328-simplex-chat-v4-6-hidden-profiles.md#improved-audiovideo-calls).

## Zaawansowane ustawienia połączeń.

### Serwery WebRTC ICE


<img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20220928-ice-servers.png" width="330">

SimpleX Chat domyślnie używa wstępnie ustawionego serwera przekaźnikowego, aby ukryć Twój adres IP przed kontaktami, ale może również obserwować czas trwania połączeń. Jeśli nie chcesz tego, możesz skonfigurować i użyć swoich samodzielnie hostowanych serwerów przekaźnikowych WebRTC zamiast tego, aby uzyskać dalszą kontrolę nad połączeniami.

1. Otwórz menu ustawień aplikacji.
2. Stuknij **Połączenia audio i wideo**.
3. Stuknij **Serwery WebRTC ICE**.
4. Włącz przełącznik **Skonfiguruj serwery ICE**.
5. Wprowadź adresy serwerów ICE (po jednym w każdym wierszu).
6. Stuknij **Zapisz**.

**Zwróć uwagę**: w przeciwieństwie do przekaźników wiadomości (serwerów SMP) konfiguracja serwerów WebRTC ICE jest przechowywana w bieżącym urządzeniu, a nie w bazie danych czatu. w przypadku przeniesienia bazy danych czatu na inne urządzenie należy zaktualizować tę konfigurację.

### Zawsze używaj przekaźnika

Połączenia audio i wideo na SimpleX Chat są domyślnie kierowane przez serwer przekaźnikowy TURN. Opcjonalnie można to wyłączyć i zamiast tego używać protokołu peer-to-peer (P2P), jeśli jest on obsługiwany przez sieć. Jednak Twój adres IP będzie znany Twoim kontaktom.

1. Otwórz menu ustawień aplikacji.
2. Stuknij **Połączenia audio i wideo**.
3. Przełącz przełącznik **Zawsze używaj przekaźnika**, aby używać serwera przekaźnikowego lub wyłącz go dla P2P.

**Zwróć uwagę**: wyłączenie tej opcji pozwala na połączenia P2P, ale nie zabrania korzystania z przekaźników TURN - w przypadku, gdy dostawcy sieci zablokują połączenia P2P, połączenie nadal będzie korzystać z przekaźników, jeśli są one dostępne. Aby zabronić korzystania z przekaźników, musisz zmienić konfigurację serwera WebRTC ICE, aby uwzględnić tylko serwery STUN, na przykład:
