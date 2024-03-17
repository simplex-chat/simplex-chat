| Zaktualizowano 07.02.2023 | Języki PL [EN](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/ANDROID.md), [FR](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/fr/ANDROID.md), [CZ](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/cz/ANDROID.md) |

# Dostęp do plików w aplikacji Android

SimpleX używa baz danych i przechowuje swoje preferencje wewnątrz prywatnego katalogu danych w systemie Android. Katalog ten zawiera:
- bazy danych
- pliki wysłane i odebrane
- pliki tymczasowe, które zostaną usunięte, gdy nie będą potrzebne
- preferencje użytkownika.


Jeśli chcesz zobaczyć co jest przechowywane wewnątrz katalogu danych SimpleX musisz mieć:
- system operacyjny oparty na Uniksie (lub [MinGW](https://www.mingw-w64.org/downloads/) w systemie Windows)
- narzędzie ADB (Android Debug Bridge) zainstalowane na komputerze ([pobierz je tutaj](https://developer.android.com/studio/releases/platform-tools) i zainstaluj)
- swoje urządzenie podłączone przez USB lub Wi-Fi do komputera.

## Proces:
- otwórz SimpleX, przejdź do `Hasło do bazy danych i eksport`, włącz `Kopia zapasowa danych aplikacji`. Dzięki temu inne kroki będą działać.
- _opcjonalnie_: jeśli chcesz zobaczyć zawartość bazy danych, zmień hasło do bazy danych z losowego na własne. Aby to zrobić, zatrzymaj czat na ekranie `Hasło do bazy danych i eksport`, otwórz `Hasło do bazy danych`, wprowadź nowe hasło i zatwierdź je, a następnie zaktualizuj. Nie zapomnij o tym, w przeciwnym razie stracisz wszystkie dane w przypadku, gdy podanie frazy hasła będzie wymagane następnym razem
- otwórz emulator terminala (Windows CMD/Powershell nie zadziała) i zmień katalog na ten, którego chcesz użyć do przechowywania kopii zapasowej:

```bash
cd /tmp  # just an example
```
Następnie należy uruchomić następujące:
```bash
adb -d backup -f chat.ab -noapk chat.simplex.app &&
tail -n +5 chat.ab > chat.dat &&
printf "\x1f\x8b\x08\x00\x00\x00\x00\x00" | cat - chat.dat > chat.gz &&
tar -xvzf chat.gz
```

Teraz odblokuj urządzenie i potwierdź operację tworzenia kopii zapasowej bez użycia hasła do szyfrowania, w przeciwnym razie polecenia nie będą działać.

Po tej czynności backup powinien się zakończyć. Jeśli zobaczysz błąd o treści `tar: Error is not recoverable: exiting now`, ale wcześniej wypisałeś kilka nazw plików, nie przejmuj się, jest ok.

Teraz pliki z kopii zapasowej będą znajdować się wewnątrz `./apps/chat.simplex.app/`.

Proszę zauważyć, że jeśli używasz nowoczesnej wersji SimpleX, bazy danych będą zaszyfrowane i nie będziesz mógł zobaczyć ich zawartości bez użycia aplikacji `sqlcipher` i bez znajomości hasła deszyfrującego (musisz je najpierw zmienić na swoje z losowo wygenerowanego w aplikacji).

Proszę zapoznać się z przewodnikiem [SQL.md](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/pl/docs/SQL.md), aby dowiedzieć się więcej jak odszyfrować bazy danych i jak wykonywać do nich zapytania.
