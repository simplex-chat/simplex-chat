---
layout: layouts/article.html
title: "SimpleX Chat v2.2 - the new privacy and security features"
date: 2022-06-04
image: images/20220604-privacy-settings.png
imageBottom: true
previewBody: blog_previews/20220604.html
permalink: "/blog/20220604-simplex-chat-new-privacy-security-settings.html"
---

# SimpleX Chat v2.2 - nowe funkcje prywatności i bezpieczeństwa

**Opublikowano:** 4 czerwca 2022 r.

Zobacz [ogłoszenie v2](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/blog/20220511-simplex-chat-v2-images-files.md), aby uzyskać więcej informacji o platformie SimpleX i o tym, jak chroni ona Twoją prywatność, unikając w swojej konstrukcji jakichkolwiek tożsamości użytkowników - SimpleX, w przeciwieństwie do każdej innej platformy do przesyłania wiadomości, nie posiada kluczy tożsamości ani żadnych numerów identyfikujących swoich użytkowników.

## Nowe ustawienia prywatności i bezpieczeństwa w wersji 2.2

<img src="https://github.com/simplex-chat/simplex-chat/blob/stable/blog/images/20220604-privacy-settings.png" width="480">

### Chroń swoje czaty

Aby chronić swoje czaty możesz włączyć Blokada SimpleX. Za każdym razem, gdy otworzysz czat po tym, jak był w tle przez 30 sekund, będziesz musiał przejść uwierzytelnienie biometryczne lub kod pin, aby użyć aplikacji (pod warunkiem, że jest ona włączona dla Twojego urządzenia).

### Oszczędzaj dane i unikaj udostępniania, że jesteś online

W przypadku, gdy chcesz zaoszczędzić dane komórkowe lub uniknąć pokazywania swoim kontaktom, że jesteś online, możesz wyłączyć automatyczne pobieranie zdjęć. Dla wielu użytkowników jest wygodniejsze, aby obrazy były pobierane automatycznie, dlatego jest to domyślnie włączone.

Podglądy obrazów o niskiej rozdzielczości nadal byłyby pokazywane, nadawcy nie mają możliwości sprawdzenia, czy je otrzymałeś, czy nie.

### Unikaj odwiedzania stron internetowych z wysyłanych linków

Kiedy otrzymujesz linki, które zawierają podglądy linków, jest to w pełni prywatne - te podglądy są generowane przez nadawcę i nie ujawniają w żaden sposób Twojego adresu IP.

Gdy wysyłasz linki, aplikacja automatycznie pobiera opis linku i obrazek ze strony internetowej danego linku. Choć jest to wygodne, naraża Twój adres IP na działanie strony internetowej. Aby tego uniknąć, możesz wyłączyć wysyłanie podglądów linków.

### Identyfikacja zagubionych wiadomości na czacie

Aplikacja śledzi integralność otrzymywanych wiadomości poprzez przeczesywanie ich numerów sekwencyjnych i sprawdzanie, czy hash poprzedniej wiadomości pasuje do hasha zawartego w wiadomości - każda rozmowa, efektywnie, to dwa blockchainy, do których dostęp masz tylko Ty i Twój kontakt.

W przypadku, gdyby część wiadomości została utracona, zobaczyłbyś to na czacie. Może to nastąpić z jednego z następujących powodów:

- wiadomości wygasły na serwerze po 30 dniach nie zostały dostarczone.
- wiadomości zostały usunięte podczas restartu serwera. Dodamy nadmiarowość serwera jeszcze w tym roku, aby uniknąć utraty wiadomości w tym przypadku, na razie, jeśli widzisz wskazanie, że niektóre wiadomości zostały utracone na czacie, możesz sprawdzić ze swoim kontaktem, co to było.
- jakiś inny błąd aplikacji. Powiadom nas za pośrednictwem czatu - zbadamy możliwe przyczyny źródłowe.
- połączenie jest zagrożone. Jest to bardzo mało prawdopodobny, ale nie niemożliwy scenariusz.

### Jest więcej

Możesz odkryć dodatkowe funkcje, które obecnie testujemy w Funkcje eksperymentalne - zostaną one ogłoszone później!

## Więcej informacji

Zobacz [ogłoszenie v1](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/blog/20220112-simplex-chat-v1-released.md), aby dowiedzieć się, jak SimpleX chroni bezpieczeństwo wiadomości.

Przeczytaj o projekcie SimpleX w [whitepaper](https://github.com/simplex-chat/simplexmq/blob/master/protocol/overview-tjr.md).
