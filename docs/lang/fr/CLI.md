---
title: Application de terminal
revision: 31.01.2023
---
| 31.01.2023 | FR, [EN](/docs/CLI.md), [CZ](/docs/lang/cs/CLI.md) |

# Application de terminal (console) SimpleX Chat pour Linux/MacOS/Windows

## Table des matières

- [Fonctionnalités du terminal de chat](#fonctionnalités-du-terminal-de-chat)
- [Installation](#🚀-installation)
  - [Télécharger le client de chat](#télécharger-le-client-de-chat)
    - [Linux et MacOS](#linux-et-macos)
    - [Windows](#windows)
  - [Compiler depuis la source](#compiler-depuis-la-source)
    - [Utiliser Docker](#utiliser-docker)
    - [Utiliser Haskell stack](#utiliser-haskell-stack)
- [Utilisation](#utilisation)
  - [Lancement du client de chat](#lancement-du-client-de-chat)
  - [Accéder aux serveurs de messagerie via Tor](#accéder-aux-serveurs-de-messagerie-via-Tor)
  - [Comment utiliser SimpleX chat](#comment-utiliser-simplex-chat)
  - [Groupes](#groupes)
  - [Envoi de fichiers](#envoi-de-fichiers)
  - [Adresses de contact d'utilisateur](#adresses-de-contact-dutilisateur)
  - [Accéder à l'historique des chats](#accéder-à-lhistorique-des-chats)

## Fonctionnalités du terminal de chat

- Chat individuel avec plusieurs personnes dans la même fenêtre de terminal.
- Messagerie de groupe.
- Envoi de fichiers aux contacts et aux groupes.
- Adresses de contact des utilisateurs - établissez des connexions via des liens de contact à usage multiple.
- Les messages sont conservés dans une base de données SQLite locale.
- Nom du destinataire auto-rempli - il suffit de taper vos messages pour répondre à l'expéditeur une fois la connexion établie.
- Des serveurs SMP de démonstration sont disponibles et préconfigurés dans l'application - ou vous pouvez [déployer votre propre serveur](https://github.com/simplex-chat/simplexmq#using-smp-server-and-smp-agent).
- Pas d'identité globale ni de noms visibles par le.s serveur.s, ce qui garantit la confidentialité totale de vos contacts et de vos conversations.
- Deux couches de chiffrement E2E (double ratchet pour les connexions duplex, utilisant un accord de clé X3DH avec des clés Curve448 éphémères, et NaCl crypto_box pour les files d'attente SMP, utilisant des clés Curve25519) et transmission hors bande des clés des destinataires (voir [Comment utiliser SimpleX chat](#comment-utiliser-simplex-chat)).
- Validation de l'intégrité du message (en incluant les résumés des messages précédents).
- Authentification de chaque commande/message par les serveurs SMP avec des clés Ed448 générées automatiquement.
- Chiffrement de transport TLS 1.3.
- Chiffrement supplémentaire des messages du serveur SMP au destinataire pour réduire la corrélation du trafic.

Les clés publiques impliquées dans l'échange de clés ne sont pas utilisées comme identité, elles sont générées aléatoirement pour chaque contact.

Voir les [primitives de chiffrement utilisées](https://github.com/simplex-chat/simplexmq/blob/master/protocol/overview-tjr.md#encryption-primitives-used) pour les détails techniques.

<a name="🚀-installation"></a>

## 🚀 Installation

### Télécharger le client de chat

#### Linux et MacOS

Pour **installer** ou **mettre à jour** `simplex-chat`, vous devez exécuter le script d'installation. Pour ce faire, utilisez la commande cURL ou Wget suivante :

```sh
curl -o- https://raw.githubusercontent.com/simplex-chat/simplex-chat/stable/install.sh | bash
```

```sh
wget -qO- https://raw.githubusercontent.com/simplex-chat/simplex-chat/stable/install.sh | bash
```

Une fois le client de chat téléchargé, vous pouvez le lancer avec la commande `simplex-chat` dans votre terminal.

Vous pouvez également télécharger manuellement une version binary de chat pour votre système à partir de la [dernière version stable] (https://github.com/simplex-chat/simplex-chat/releases) et le rendre exécutable comme indiqué ci-dessous.

```sh
chmod +x <binary>
mv <binary> ~/.local/bin/simplex-chat
```

(ou tout autre emplacement préféré sur `PATH`).

Sous MacOS, vous devez également [autoriser Gatekeeper à l'exécuter](https://support.apple.com/en-us/HT202491).

#### Windows

```sh
move <binary> %APPDATA%/local/bin/simplex-chat.exe
```

### Compiler depuis la source

> **Veuillez noter** : pour compiler l'application, utilisez le code source de la [branche stable](https://github.com/simplex-chat/simplex-chat/tree/stable).

#### Utiliser Docker

Sous Linux, vous pouvez compiler l'exécutable du chat en utilisant [docker build with custom output](https://docs.docker.com/engine/reference/commandline/build/#custom-build-outputs) :

```shell
git clone git@github.com:simplex-chat/simplex-chat.git
cd simplex-chat
git checkout stable
DOCKER_BUILDKIT=1 docker build --output ~/.local/bin .
```

> **Veuillez noter** : Si vous rencontrez l'erreur ``version `GLIBC_2.28' non trouvée``, reconstruisez-le avec l'image de base `haskell:8.10.7-stretch`(changez-la dans votre [Dockerfile](Dockerfile) local).

#### Utiliser Haskell stack

(sur n'importe quel OS)

1. Installer [Haskell GHCup](https://www.haskell.org/ghcup/), GHC 8.10.7 et cabal :

```shell
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

2. Compiler le projet :

```shell
git clone git@github.com:simplex-chat/simplex-chat.git
cd simplex-chat
git checkout stable
# sur Linux
apt-get update && apt-get install -y build-essential libgmp3-dev zlib1g-dev
cp scripts/cabal.project.local.linux cabal.project.local
# ou sur MacOS:
# brew install openssl@1.1
# cp scripts/cabal.project.local.mac cabal.project.local
# vous devrez peut-être modifier cabal.project.local pour indiquer l'emplacement réel d'openssl
cabal update
cabal install
```

## Utilisation

### Lancement du client de chat

Pour démarrer le client de chat, exécutez `simplex-chat` depuis le terminal.

Par défaut, le répertoire de données de l'application est créé dans le répertoire personnel (`~/.simplex`, ou `%APPDATA%/simplex` sous Windows), et deux fichiers de base de données SQLite `simplex_v1_chat.db` et `simplex_v1_agent.db` y sont initialisés.

Pour spécifier un préfixe de chemin de fichier différent pour les fichiers de la base de données, utilisez l'option de ligne de commande `-d` :

```shell
$ simplex-chat -d alice
```

L'exécution ci-dessus, par exemple, créera les fichiers de base de données `alice_v1_chat.db` et `alice_v1_agent.db` dans le répertoire actuel.

Trois serveurs SMP par défaut sont hébergés sur Linode - ils sont [pré-configurés dans l'application](https://github.com/simplex-chat/simplex-chat/blob/stable/src/Simplex/Chat/Options.hs#L42).

Si vous avez déployé votre propre serveur SMP, vous pouvez configurer le client via l'option `-s` :

```shell
$ simplex-chat -s smp://LcJUMfVhwD8yxjAiSaDzzGF3-kLG4Uh0Fl_ZIjrRwjI=@smp.example.com
```

La chaîne encodée en Base64url qui précède l'adresse du serveur est l'empreinte du certificat hors ligne du serveur qui est validée par le client lors du handshake TLS.

Vous pouvez toujours parler aux gens en utilisant le serveur par défaut ou tout autre serveur - cela n'affecte que l'emplacement de la file d'attente des messages lorsque vous initiez la connexion (et la file d'attente des réponses peut se trouver sur un autre serveur, comme défini par le client de l'autre coté).

Exécutez `simplex-chat -h` pour voir toutes les options disponibles.

### Accéder aux serveurs de messagerie via Tor

Installez Tor et exécutez-le en tant que proxy SOCKS5 sur le port 9050, par exemple sur Mac vous pouvez :

```
brew install tor
brew services start tor
```

Utilisez l'option `-x` pour accéder aux serveurs via Tor :

```
simplex-chat -x
```

Vous pouvez également utiliser l'option `--socks-proxy=ipv4:port` ou `--socks-proxy=:port` pour configurer l'hôte et le port de votre proxy SOCKS5, par exemple si vous l'exécutez sur un autre hôte ou port.

### Comment utiliser SimpleX chat

Une fois que vous aurez lancé le chat, vous serez invité à indiquer un "nom d'affichage" et un "nom complet" facultatif pour créer un profil de chat local. Votre nom d'affichage est un alias par lequel vos contacts peuvent se référer à vous. Il n'est pas unique et ne sert pas d'identité globale. Si certains de vos contacts ont choisi le même nom d'affichage, le client de chat ajoute un suffixe numérique à leur nom d'affichage local.

Le schéma ci-dessous montre comment connecter et envoyer un message à un contact :

<div align="center">
  <img align="center" src="/images/how-to-use-simplex.svg">
</div>

Une fois que vous avez configuré votre profil local, entrez `/c` (pour `/connect`) pour créer une nouvelle connexion et générer une invitation. Envoyez cette invitation à votre contact via n'importe quel autre canal.

Vous pouvez créer plusieurs invitations en saisissant plusieurs fois la commande `/connect` et en envoyant ces invitations aux contacts correspondants avec lesquels vous souhaitez vous connecter.

L'invitation ne peut être utilisée qu'une seule fois et même si elle est interceptée, l'attaquant ne pourra pas l'utiliser pour vous envoyer les messages via cette file d'attente une fois que votre contact aura confirmé que la connexion est établie. Voir le protocole de l'agent pour une explication du [format d'invitation](https://github.com/simplex-chat/simplexmq/blob/master/protocol/agent-protocol.md#connection-request).

Le contact qui a reçu l'invitation doit saisir `/c <invitation>` pour accepter la connexion. La connexion est alors établie, et les deux parties sont notifiées.

Ils utiliseraient alors les commandes `@<name> <message>` pour envoyer des messages. Vous pouvez aussi simplement commencer à taper un message pour l'envoyer au contact qui était le dernier.

Utilisez `/help` dans le chat pour voir la liste des commandes disponibles.

### Groupes

Pour créer un groupe, utilisez `/g <groupe>`, puis ajoutez-y des contacts avec `/a <groupe> <nom>`. Vous pouvez ensuite envoyer des messages au groupe en entrant `#<groupe> <message>`. Utilisez `/help groups` pour d'autres commandes.

![simplex-chat](/images/groups.gif)

> **Veuillez noter** : les groupes ne sont pas stockés sur un serveur, ils sont conservés comme une liste de membres dans la base de données de l'application à qui les messages seront envoyés.

### Envoi de fichiers

Vous pouvez envoyer un fichier à votre contact avec `/f @<contact> <chemin_du_fichier>` - le destinataire devra l'accepter avant qu'il ne soit envoyé. Utilisez `/help files` pour les autres commandes.

![simplex-chat](/images/files.gif)

Vous pouvez envoyer des fichiers à un groupe avec `/f #<group> <chemin_du_fichier>`.

### Adresses de contact d'utilisateur

Comme alternative aux liens d'invitation unique, vous pouvez créer une adresse à long terme avec `/ad` (pour `/address`). L'adresse créée peut ensuite être partagée via n'importe quel canal, et utilisée par d'autres utilisateurs comme lien pour faire une demande de contact avec `/c <adresse_de_contact_de_l'utilisateur>`.

Vous pouvez accepter ou rejeter les demandes entrantes avec les commandes `/ac <nom>` et `/rc <nom>`.

L'adresse de l'utilisateur est "à long terme" dans le sens où il s'agit d'un lien de connexion à usage multiple - elle peut être utilisée jusqu'à ce qu'elle soit supprimée par l'utilisateur, auquel cas toutes les connexions établies resteront actives (contrairement à ce qui se passe avec le courrier électronique, où le changement d'adresse empêche les gens de vous envoyer des messages).

Utilisez `/help address` pour les autres commandes.

![simplex-chat](/images/user-addresses.gif)
