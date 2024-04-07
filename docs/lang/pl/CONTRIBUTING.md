---
title: Poradnik wspierania projektu
revision: 31.01.2023
---

| Updated 31.01.2023 | Languages: EN, [FR](/docs/lang/fr/CONTRIBUTING.md), [CZ](/docs/lang/cs/CONTRIBUTING.md) |

# Poradnik wspierania projektu

## Kompilacja z włączonym szyfrowaniem SQLCipher

Dodaj `cabal.project.local` do katalogu głównego projektu z lokalizacją nagłówków i bibliotek OpenSSL oraz flagą ustawiającą tryb szyfrowania:

```
cp scripts/cabal.project.local.mac cabal.project.local
# lub
# cp scripts/cabal.project.local.linux cabal.project.local
```

## OpenSSL na MacOS

MacOS ma domyślnie zainstalowany LibreSSL, OpenSSL musi być zainstalowany, aby skompilować SimpleX z kodu źródłowego.

OpenSSL można zainstalować za pomocą `brew install openssl@1.1`

Będziesz musiał dodać `/opt/homebrew/opt/openssl@1.1/bin` do swojego PATH, aby wszystko działało poprawnie


## Branche projektu

**W repo simplex-chat**


- `stable` - stabilne wydanie aplikacji, może być używane do aktualizacji poprzedniego stabilnego wydania (GHC 9.6.3).

- `stable-android` - używane do budowania stabilnej biblioteki rdzenia Androida z Nix (GHC 8.10.7) - tylko dla Androida armv7a.

- `stable-ios` - używane do budowania stabilnej biblioteki rdzenia iOS z Nix (GHC 8.10.7) - ten branch powinien być taki sam jak `stable-android` z wyjątkiem plików konfiguracyjnych Nix. Przestarzałe.

- `master` - branch dla wydań wersji beta (GHC 9.6.3).

- `master-ghc8107` - branch dla wydań wersji beta (GHC 8.10.7). Przestarzałe.

- `master-android` - używane do budowania biblioteki rdzenia Androida w wersji beta z Nix (GHC 8.10.7) - tylko dla Androida armv7a.

- `master-ios` - służy do budowania biblioteki rdzenia beta iOS z Nix (GHC 8.10.7). Przestarzałe.

- `windows-ghc8107` - branch do kompilacji głównej biblioteki Windows (GHC 8.10.7). Przestarzałe?

Branche `master-ios` i `windows-ghc8107` powinny być takie same jak `master-ghc8107` z wyjątkiem plików konfiguracyjnych Nix.

**W repo simplexmq**

- `master` - używa GHC 9.6.3, jego commit powinien być użyty w branchu `master` repo simplex-chat.

- `master-ghc8107` - jego commit powinien być użyty w branchu `master-android` (i `master-ios`) repo simplex-chat. Przestarzałe.

## Development i proces wydawania

1. Tworzenie PR-ów do brancha `master` _tylko_ dla repozytoriów simplex-chat i simplexmq.

2. Jeśli repozytorium simplexmq zostało zmienione, aby skompilować mobilne biblioteki rdzenia należy połączyć jego branch `master` z branchem `master-ghc8107`.

3. Aby skompilować podstawowe biblioteki dla Androida, iOS i Windows:

- scal branch `master` z branchem `master-android`.

- Zaktualizuj kod, aby był kompatybilny z GHC 8.10.7 (patrz niżej).

- push do GitHuba.

4. Wszystkie biblioteki powinny być budowane z brancha `master`, Android armv7a - z brancha `master-android`.

5. Aby zbudować aplikacje Desktop i CLI, należy utworzyć tag w branchu `master`, pliki APK powinny być dołączone do wydania.

6. Po publicznym wydaniu w App Store i Play Store, scal:

- `master` do `stable`

- `master` do `master-android` (i skompiluj/zaktualizuj kod)

- `master-android` do `stable-android`.

7. Branch `master` repo simplexmq powinien zostać niezależnie scalony z branchem `stable` w wydaniach stabilnych.


## Różnice pomiędzy GHC 8.10.7 i GHC 9.6.3

1. Główna różnica związana jest z rozszerzeniem `DuplicateRecordFields`.

W GHC 9.6.3 nie jest już możliwe określenie typu podczas korzystania z selektorów, zamiast tego używane jest rozszerzenie OverloadedRecordDot i składnia, które muszą zostać usunięte w GHC 8.10.7:

```haskell
{-# LANGUAGE DuplicateRecordFields #-}
-- Użyj tego w GHC 9.6.3, gdy jest to potrzebne
{-# LANGUAGE OverloadedRecordDot #-}

-- syntax GHC 9.6.3
let x = record.field

-- syntax GHC 8.10.7 usunięty w GHC 9.6.3
let x = field (record :: Record)
```

Nadal możliwe jest określenie typu podczas korzystania ze składni aktualizacji rekordu, użyj tej reguły, aby wyłączyć ostrzeżenie kompilatora:

```haskell
-- Użyj tego w GHC 9.6.3, gdy jest to potrzebne
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}

let r' = (record :: Record) {field = value}
```

2. Większość funkcji monad musi być teraz importowana z `Control.Monad`, a nie z konkretnych modułów monad (np. `Control.Monad.Except`).

```haskell
-- Użyj tego w GHC 9.6.3, gdy jest to potrzebne
import Control.Monad
```

[Ten PR](https://github.com/simplex-chat/simplex-chat/pull/2975/files) opisuje wszystkie różnice.
