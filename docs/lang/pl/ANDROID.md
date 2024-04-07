---
title: Dostęp do plików w aplikacji Androidowej
revision: 07.02.2023
---

| 07.02.2023 | PL, [EN](/docs/ANDROID.md), [CZ](/docs/lang/cs/ANDROID.md), [FR](/docs/lang/fr/ANDROID.md) |

# Dostęp do plików w aplikacji Androidowej

SimpleX wykorzystuje bazy danych i przechowuje ustawienia w prywatnym katalogu w systemie Android. Katalog ten zawiera:

- bazy danych
- wysłane i odebrane pliki
- pliki tymczasowe, które zostaną usunięte, gdy nie są już potrzebne
- ustawienia użytkownika.

Jeśli chcesz zobaczyć, co jest przechowywane w katalogu SimpleX, musisz mieć:

- System operacyjny oparty na systemie Unix (lub [MinGW](https://www.mingw-w64.org/downloads/) na Windowsie)
- narzędzie ADB (Android Debug Bridge) zainstalowane na komputerze ([pobierz je tutaj](https://developer.android.com/studio/releases/platform-tools) i zainstaluj)
- urządzenie podłączone przez USB lub Wi-Fi do komputera.

## Proces:

- otwórz SimpleX, przejdź do `Hasło do bazy danych i eksport`, włącz `Kopia zapasowa danych aplikacji`. To sprawi, że następne kroki będą działać.
- _opcjonalnie_: jeśli chcesz wyświetlić zawartość bazy danych, zmień hasło bazy danych z losowego na swoje. Aby to zrobić, zatrzymaj czat na ekranie `Hasło do bazy danych i eksport`, otwórz `Hasło do bazy danych`, wprowadź nowe hasło i potwierdź je, a następnie zatwierdź. Nie zapomnij go, w przeciwnym razie utracisz wszystkie dane w przypadku, gdy zostaniesz ponownie poproszony o hasło.
- otwórz emulator terminala (Windows CMD/Powershell nie zadziała) i zmień katalog na ten, którego chcesz użyć do przechowywania kopii zapasowej:

```bash
cd /tmp  # to tylko przykład
```
Następne uruchom:
```bash
adb -d backup -f chat.ab -noapk chat.simplex.app && 
tail -n +5 chat.ab > chat.dat && 
printf "\x1f\x8b\x08\x00\x00\x00\x00\x00" | cat - chat.dat > chat.gz && 
tar -xvzf chat.gz
```

Teraz odblokuj urządzenie i potwierdź operację tworzenia kopii zapasowej bez użycia hasła do szyfrowania, w przeciwnym razie polecenia nie będą działać.

Po tym kopia zapasowa powinna zostać zrobiona. Jeśli pojawi się błąd `tar: Error is not recoverable: exiting now`, ale wcześniej pojawiło się kilka nazw plików, nie martw się, wszystko jest w porządku.

Teraz zapisane pliki będą w `./apps/chat.simplex.app/`.

Pamiętaj, że jeśli korzystasz z nowej wersji SimpleX, bazy danych będą zaszyfrowane i nie będziesz w stanie przeglądać ich zawartości bez użycia aplikacji `sqlcipher` oraz gdy nie znasz hasła deszyfrującego (musisz najpierw zmienić je na swoje z losowo wygenerowanego w aplikacji).

## Odszyfrowywanie baz danych

Aby wyświetlić dane bazy danych, należy je najpierw odszyfrować. Zainstaluj `sqlcipher` używając ulubionego menedżera pakietów i uruchom następujące polecenia w katalogu z bazami danych:
```bash
sqlcipher files_chat.db
pragma key="youDecryptionPassphrase";
# Upewnij się, że to działa
select * from users;
```

Jeśli zobaczysz `Parse error: no such table: users`, upewnij się, że wprowadzono prawidłowe hasło i zostało ono zmienione z losowego w aplikacji na Androida (jeśli oczywiście pobrano tę bazę danych z urządzenia z Androidem).
