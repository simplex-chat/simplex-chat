---
title: Průvodce přispíváním
revision: 31.01.2023
---
| Aktualizováno 31.01.2023 | Jazyky: CZ, [EN](/docs/CONTRIBUTING.md), [FR](/docs/lang/fr/CONTRIBUTING.md), [PL](/docs/lang/pl/CONTRIBUTING.md) |

# Průvodce přispíváním

## Kompilace s povoleným šifrováním SQLCipher

Do kořenového adresáře projektu přidejte `cabal.project.local` s umístěním hlaviček a knihoven OpenSSL a příznakem nastavujícím režim šifrování:

```
cp scripts/cabal.project.local.mac cabal.project.local
# nebo
# cp scripts/cabal.project.local.linux cabal.project.local
```

## OpenSSL na MacOS

Systém MacOS je standardně dodáván s LibreSSL, pro kompilaci SimpleX ze zdrojových kódů je nutné nainstalovat OpenSSL.

OpenSSL lze nainstalovat pomocí `brew install openssl@1.1`.

Aby vše fungovalo správně, musíte do své cesty PATH přidat `/opt/homebrew/opt/openssl@1.1/bin`.
