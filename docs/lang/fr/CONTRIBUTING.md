---
title: Guide pour contribuer
revision: 31.01.2023
---
| 31.01.2023 | FR, [EN](/docs/CONTRIBUTING.md), [CZ](/docs/lang/cs/CONTRIBUTING.md), [PL](/docs/lang/pl/CONTRIBUTING.md) |

# Guide pour contribuer

## Compiler avec le chiffrement SQLCipher activé

Ajoutez `cabal.project.local` à la racine du projet avec les emplacements des en-têtes et des bibliothèques OpenSSL ainsi que le paramètre définissant le mode de chiffrement :

```
cp scripts/cabal.project.local.mac cabal.project.local
# or
# cp scripts/cabal.project.local.linux cabal.project.local
```

## OpenSSL sur MacOS

LibreSSL est fourni par défaut sur MacOS, OpenSSL doit être installé pour compiler SimpleX à partir de la source.

OpenSSL peut être installé avec `brew install openssl@1.1`

Vous devez ajouter `/opt/homebrew/opt/openssl@1.1/bin` à votre PATH pour que tout fonctionne correctement.
