| Zaktualizowano 31.01.2023 | Języki: PL, [EN](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/CONTRIBUTING.md), [FR](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/fr/CONTRIBUTING.md), [CZ](https://github.com/simplex-chat/simplex-chat/blob/stable/docs/lang/cz/CONTRIBUTING.md) |

# Przewodnik wspomagający

## Kompilacja z włączonym szyfrowaniem SQLCipher

Dodaj `cabal.project.local` do katalogu głównego projektu z lokalizacją nagłówków i bibliotek OpenSSL oraz flagą ustawiającą tryb szyfrowania:

```
cp scripts/cabal.project.local.mac cabal.project.local
# lub
# cp scripts/cabal.project.local.linux cabal.project.local
```

## OpenSSL na MacOS

MacOS posiada domyślnie LibreSSL, OpenSSL musi być zainstalowany aby skompilować SimpleX ze źródła.

OpenSSL może być zainstalowany za pomocą `brew install openssl@1.1`.

Będziesz musiał dodać `/opt/homebrew/opt/openssl@1.1/bin` do swojej PATH, aby wszystko działało poprawnie.
