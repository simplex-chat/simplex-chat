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


## Branches du projet

**Dans le repo simplex-chat**

- `stable` - version stable des applications, peut être utilisée pour les mises à jour de la version stable précédente (GHC 9.6.3).

- `stable-android` - utilisé pour construire la bibliothèque de base stable d'Android avec Nix (GHC 8.10.7) - seulement pour Android armv7a.

- `stable-ios` - utilisé pour construire la bibliothèque de base stable iOS avec Nix (GHC 8.10.7) - cette branche devrait être la même que `stable-android` à l'exception des fichiers de configuration Nix. Déclassé.

- `master` - branche pour les versions beta (GHC 9.6.3).

- `master-ghc8107` - branche pour les versions beta (GHC 8.10.7). Déclassé.

- `master-android` - utilisé pour construire la bibliothèque beta Android core avec Nix (GHC 8.10.7) - seulement pour Android armv7a.

- `master-ios` - utilisé pour construire la version beta de la bibliothèque iOS avec Nix (GHC 8.10.7). Déclassé.

- `windows-ghc8107` - branche pour la construction de la bibliothèque de base Windows (GHC 8.10.7). Déclassé ?

Les branches `master-ios` et `windows-ghc8107` devraient être les mêmes que `master-ghc8107` à l'exception des fichiers de configuration Nix.

**Dans le repo simplexmq**

- `master` - utilise GHC 9.6.3 son commit devrait être utilisé dans la branche `master` du repo simplex-chat.

- `master-ghc8107` - son commit doit être utilisé dans la branche `master-android` (et `master-ios`) du repo simplex-chat. Déclassé.

## Processus de développement et de publication

1. Faire des PRs à la branche `master` _uniquement_ pour les dépôts simplex-chat et simplexmq.

2. Si le dépôt simplexmq a été modifié, pour construire les bibliothèques de base mobiles, vous devez fusionner sa branche `master` dans la branche `master-ghc8107`.

3. Pour construire les bibliothèques de base pour Android, iOS et Windows :
- fusionner la branche `master` dans la branche `master-android`.
- mettre à jour le code pour être compatible avec GHC 8.10.7 (voir ci-dessous).
- pousser sur GitHub.

4. Toutes les bibliothèques doivent être construites à partir de la branche `master`, Android armv7a - à partir de la branche `master-android`.

5. Pour construire les applications Desktop et CLI, faire un tag dans la branche `master`, les fichiers APK doivent être attachés à la version.

6. Après la publication publique sur l'App Store et le Play Store, fusionnez :
- `master` vers `stable`
- `master` vers `master-android` (et compiler/mettre à jour le code)
- `master-android` vers `stable-android`

7. Indépendamment, la branche `master` du repo simplexmq devrait être fusionnée avec la branche `stable` sur les versions stables.


## Différences entre GHC 8.10.7 et GHC 9.6.3

1. La principale différence est liée à l'extension `DuplicateRecordFields`.

Il n'est plus possible dans GHC 9.6.3 de spécifier le type lors de l'utilisation de sélecteurs, à la place l'extension OverloadedRecordDot et la syntaxe sont utilisées et doivent être supprimées dans GHC 8.10.7 :

```haskell
{-# LANGUE DuplicateRecordFields #-}
-- l'utiliser dans GHC 9.6.3 quand c'est nécessaire
{-# LANGUAGE OverloadedRecordDot #-}

-- syntaxe GHC 9.6.3
let x = record.field

-- syntaxe GHC 8.10.7 supprimée dans GHC 9.6.3
let x = field (record : : Record)
```

Il est toujours possible de spécifier le type lors de l'utilisation de la syntaxe de mise à jour des enregistrements, utilisez ce pragma pour supprimer l'avertissement du compilateur :

```haskell
-- utilisez ceci dans GHC 9.6.3 lorsque c'est nécessaire
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}

let r' = (record : : Record) {field = value}
```

2. La plupart des fonctions monad doivent maintenant être importées depuis `Control.Monad`, et non depuis des modules monad spécifiques (par exemple `Control.Monad.Except`).

```haskell
-- utilise ceci dans GHC 9.6.3 quand c'est nécessaire
import Control.Monad
```

[Cet PR](https://github.com/simplex-chat/simplex-chat/pull/2975/files) contient toutes les différences.
