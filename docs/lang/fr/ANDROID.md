---
title: Accès aux fichiers dans l'application Android
revision: 07.02.2023
---
| 07.02.2023 | FR, [EN](/docs/ANDROID.md), [CZ](/docs/lang/cs/ANDROID.md) |

# Accès aux fichiers dans l'application Android

SimpleX utilise des bases de données et stocke ses préférences dans le répertoire de données privées d'Android. Ce répertoire contient
- des bases de données
- les fichiers envoyés et reçus
- les fichiers temporaires qui seront supprimés lorsqu'ils ne sont pas nécessaires
- les préférences de l'utilisateur.


Si vous voulez voir ce qui est stocké dans le répertoire de données de SimpleX, vous devez avoir :
- Un système d'exploitation Unix (ou [MinGW](https://www.mingw-w64.org/downloads/) sous Windows)
- l'outil ADB (Android Debug Bridge) installé sur un ordinateur ([téléchargez-le ici](https://developer.android.com/studio/releases/platform-tools) et installez-le)
- votre appareil connecté via USB ou Wi-Fi à l'ordinateur.

## La procédure :

- Ouvrez SimpleX, allez dans `Phrase secrète et exportation de la base de données`, activez `Sauvegarde des données de l'app`. Cela permettra aux autres étapes de fonctionner
- optionnel_ : si vous voulez voir le contenu de la base de données, changez la phrase secrète de la base de données aléatoire pour la vôtre. Pour ce faire, arrêtez le chat dans le menu "Phrase secrète et exportation de la base de données", ouvrez "Phrase secrète de la base de données", entrez la nouvelle phrase secrète et confirmez-la, puis mettez-la à jour. Ne l'oubliez pas, sinon vous perdrez toutes vos données au cas où la phrase d'authentification vous serait redemandée plus tard.
- ouvrez un émulateur de terminal (Windows CMD/Powershell ne fonctionnera pas) et changez de répertoire pour celui que vous voulez utiliser pour stocker la sauvegarde :

```bash
cd /tmp  # juste un exemple
```
Exécutez ensuite la méthode suivante :
```bash
adb -d backup -f chat.ab -noapk chat.simplex.app && 
tail -n +5 chat.ab > chat.dat && 
printf "\x1f\x8b\x08\x00\x00\x00\x00\x00" | cat - chat.dat > chat.gz && 
tar -xvzf chat.gz
```

Déverrouillez maintenant l'appareil et confirmez une opération de sauvegarde sans utiliser de mot de passe pour le chiffrement, sinon les commandes ne fonctionneront pas.

Ensuite, la sauvegarde doit être terminée. Si vous voyez une erreur disant `tar : Error is not recoverable : exiting now` mais qu'avant cela vous avez enregistré quelques noms de fichiers, ne vous inquiétez pas, tout va bien.

Maintenant les fichiers sauvegardés seront dans `./apps/chat.simplex.app/`.

Veuillez noter que si vous utilisez une version récente de SimpleX, les bases de données seront chiffrées, et vous ne pourrez pas en voir le contenu sans utiliser l'application `sqlcipher` et sans connaître la phrase secrète de déchiffrement (vous devez d'abord la changer pour la vôtre à partir de celle qui est générée aléatoirement dans l'application).

## Déchiffrer les bases de données

Afin de visualiser les données de la base de données, vous devez d'abord les déchiffrer. Installez `sqlcipher` en utilisant votre gestionnaire de paquets préféré et exécutez les commandes suivantes dans le répertoire contenant les bases de données :
```bash
sqlcipher files_chat.db
pragma key="youDecryptionPassphrase";
# S'assurer qu'il fonctionne bien
select * from users;
```

Si vous voyez `Parse error : no such table : users`, assurez-vous que vous avez entré la bonne phrase secrète, et que vous avez changé la phrase secrète au hasard dans l'application Android (si vous avez obtenu cette base de données à partir d'un appareil Android, bien sûr).
