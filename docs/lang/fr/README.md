| Updated 07.02.2023 | Languages: [EN](/README.md), FR |

<img src="/images/simplex-chat-logo.svg" alt="SimpleX logo" width="100%">

# SimpleX - la première plateforme de messagerie qui n'a pas le moindre identifiant d'utilisateur - 100% privé par définition !

[![build](https://github.com/simplex-chat/simplex-chat/actions/workflows/build.yml/badge.svg?branch=stable)](https://github.com/simplex-chat/simplex-chat/actions/workflows/build.yml)
[![GitHub downloads](https://img.shields.io/github/downloads/simplex-chat/simplex-chat/total)](https://github.com/simplex-chat/simplex-chat/releases)
[![GitHub release](https://img.shields.io/github/v/release/simplex-chat/simplex-chat)](https://github.com/simplex-chat/simplex-chat/releases)
[![Join on Reddit](https://img.shields.io/reddit/subreddit-subscribers/SimpleXChat?style=social)](https://www.reddit.com/r/SimpleXChat)
[![Follow on Mastodon](https://img.shields.io/mastodon/follow/108619463746856738?domain=https%3A%2F%2Fmastodon.social&style=social)](https://mastodon.social/@simplex)

[<img src="https://github.com/simplex-chat/.github/blob/master/profile/images/apple_store.svg" alt="iOS app" height="42">](https://apps.apple.com/us/app/simplex-chat/id1605771084)
&nbsp;
[![Android app](https://github.com/simplex-chat/.github/blob/master/profile/images/google_play.svg)](https://play.google.com/store/apps/details?id=chat.simplex.app)
&nbsp;
[<img src="https://github.com/simplex-chat/.github/blob/master/profile/images/f_droid.svg" alt="F-Droid" height="41">](https://app.simplex.chat)
&nbsp;
[<img src="https://github.com/simplex-chat/.github/blob/master/profile/images/testflight.png" alt="iOS TestFlight" height="41">](https://testflight.apple.com/join/DWuT2LQu)
&nbsp;
[<img src="https://github.com/simplex-chat/.github/blob/master/profile/images/apk_icon.png" alt="APK" height="41">](https://github.com/simplex-chat/simplex-chat/releases/latest/download/simplex.apk)

- 🖲 Protège vos messages et vos métadonnées - avec qui vous parlez et quand.
- 🔐 Chiffrement de bout en bout à double ratchet, avec couche de chiffrement supplémentaire.
- 📱 Apps mobiles pour Android ([Google Play](https://play.google.com/store/apps/details?id=chat.simplex.app), [APK](https://github.com/simplex-chat/simplex-chat/releases/latest/download/simplex.apk)) et [iOS](https://apps.apple.com/us/app/simplex-chat/id1605771084).
- 🚀 [Bêta TestFlight pour iOS](https://testflight.apple.com/join/DWuT2LQu) avec les nouvelles fonctionnalités 1 à 2 semaines plus tôt - **limitée à 10 000 utilisateurs** !
- 🖥 Disponible en tant que terminal (console) / CLI sur Linux, MacOS, Windows.

**NOUVEAU** : Audit de sécurité par [Trail of Bits](https://www.trailofbits.com/about), le [nouveau site](https://simplex.chat) et la v4.2 est disponible ! [Voir l'annonce](/blog/20221108-simplex-chat-v4.2-security-audit-new-website.md)

## Sommaire

- [Pourquoi la vie privée est importante](#pourquoi-la-vie-privée-est-importante)
- [L'approche SimpleX de la vie privée et de la sécurité](#lapproche-simplex-de-la-vie-privée-et-de-la-sécurité)
  - [Confidentialité totale](#confidentialité-totale-de-votre-identité-de-votre-profil-de-vos-contacts-et-de-vos-métadonnées)
  - [Protection contre le spam et les abus](#protection-contre-le-spam-et-les-abus)
  - [Propriété et sécurité de vos données](#propriété-contrôle-et-sécurité-totale-de-vos-données)
  - [Les utilisateurs sont maîtres de leur réseau SimpleX](#les-utilisateurs-sont-maîtres-du-réseau-simplex)
- [Foire aux questions](#foire-aux-questions)
- [Nouvelles et mises à jour](#nouvelles-et-mises-à-jour)
- [Établir une connexion privée](#établir-une-connexion-privée)
- [Installation rapide d'une application terminale](#⚡-installation-rapide-dune-application-pour-terminal)
- [Le modèle de la plateforme SimpleX](#le-modèle-de-la-plateforme-simplex)
- [Vie privée : détails techniques et limites](#vie-privée--détails-techniques-et-limites)
- [Pour les développeurs](#pour-les-développeurs)
- [Feuille de route](#feuille-de-route)
- [Rejoindre un groupe d'utilisateurs](#rejoindre-un-groupe-dutilisateurs)
- [Traduire l'application](#traduire-lapplication)
- [Contribuer](#contribuer)
- [Aidez-nous en faisant des dons](#aidez-nous-en-faisant-des-dons)
- [Avertissements, contact de sécurité, licence](#avertissements)

## Pourquoi la vie privée est importante

Tout le monde devrait se soucier de la confidentialité et de la sécurité de ses communications - des conversations anodines peuvent vous mettre en danger, même si vous n'avez rien à cacher.

L'une des histoires les plus choquantes est l'expérience de [Mohamedou Ould Salahi](https://fr.wikipedia.org/wiki/Mohamedou_Ould_Slahi) dont il a parlé dans ses Mémoires et qui est illustrée dans le film Désigné coupable (The Mauritanian). Il a été placé dans le camp de Guantanamo, sans procès, et y a été torturé pendant 15 ans après un appel téléphonique à un proche en Afghanistan, soupçonné d'être impliqué dans les attentats du 11 septembre, bien qu'il ait vécu en Allemagne pendant les 10 années précédant les attentats.

Il ne suffit pas d'utiliser une messagerie chiffrée de bout en bout, nous devrions tous utiliser des messageries qui protègent la vie privée de nos réseaux personnels, c'est-à-dire les personnes avec lesquelles nous sommes connectés.

## L'approche SimpleX de la vie privée et de la sécurité

### Confidentialité totale de votre identité, de votre profil, de vos contacts et de vos métadonnées.

**Contrairement à toutes les autres plateformes de messagerie existante, SimpleX n'a pas d'identifiant attribué aux utilisateurs.** - pas même des nombres aléatoires. Cela protège la confidentialité des personnes avec lesquelles vous communiquez, en les cachant aux serveurs de la plateforme SimpleX et à tout observateur. [En savoir plus](./SIMPLEX.md#protection-complète-de-votre-identité,-profil,-contacts-et-métadonnées).

### La meilleure protection contre le spam et les abus

Comme vous n'avez pas d'identifiant sur la plateforme SimpleX, vous ne pouvez pas être contacté, sauf si vous partagez un lien d'invitation unique ou une adresse d'utilisateur temporaire facultative. [En savoir plus](./SIMPLEX.md#la-meilleure-protection-contre-le-spam-et-les-abus).

### Propriété, contrôle et sécurité totale de vos données

SimpleX stocke toutes les données de l'utilisateur sur les appareils clients, les messages ne sont conservés que temporairement sur les serveurs relais SimpleX jusqu'à leur réception. [En savoir plus](./SIMPLEX.md#propriété,-contrôle-et-sécurité-totale-de-vos-données).

### Les utilisateurs sont maîtres du réseau SimpleX

Vous pouvez utiliser SimpleX avec vos propres serveurs et continuer à communiquer avec les personnes utilisant les serveurs préconfigurés dans les applications ou tout autre serveur SimpleX. [En savoir plus](./SIMPLEX.md#les-utilisateurs-sont-maîtres-du-réseau-simplex).

## Foire aux questions

1. _Comment SimpleX peut distribuer des messages sans aucun identifiant d'utilisateur ?_ Voir [l'annonce de la v2](/blog/20220511-simplex-chat-v2-images-files.md#the-first-messaging-platform-without-user-identifiers) explaining how SimpleX works.

2. _Pourquoi ne pas simplement utiliser Signal ?_ Signal est une plateforme centralisée qui utilise les numéros de téléphone pour identifier ses utilisateurs et leurs contacts. Cela signifie que, si le contenu de vos messages sur Signal est protégé par un chiffrement robuste de bout en bout, une importante quantité de métadonnées est visible pour Signal - avec qui vous parlez et quand.

3. _En quoi est-ce différent de Matrix, Session, Ricochet, Cwtch, etc., qui ne requièrent pas non plus d'identité d'utilisateur ?_ Bien que ces plateformes ne requièrent pas d'_identité réelle_, elles s'appuient sur des identités d'utilisateur anonymes pour transmettre les messages - il peut s'agir, par exemple, d'une clé d'identité ou d'un nombre aléatoire. L'utilisation d'une identité d'utilisateur persistante, même anonyme, crée un risque que le profil de connexion de l'utilisateur devienne connu des observateurs et/ou des fournisseurs de services, et peut conduire à la désanonymisation de certains utilisateurs. Si le même profil d'utilisateur est utilisé pour se connecter à deux personnes différentes via un messager autre que SimpleX, ces deux personnes peuvent confirmer si elles sont connectées à la même personne - elles utiliseraient le même identifiant d'utilisateur dans les messages. Avec SimpleX, il n'y a pas de métadonnées communes entre vos conversations avec différents contacts - une qualité qu'aucune autre plateforme de messagerie ne possède.

## Nouvelles et mises à jour

Mises à jour récentes :

[04 févr. 2023. Sortie de la v4.5 - avec plusieurs profils d'utilisateurs, les brouillons de messages, l'isolation du transport et l'interface en italien](/blog/20230204-simplex-chat-v4-5-user-chat-profiles.md).

[03 janv. 2023. Sortie de la v4.4 - avec les messages éphèmères, les messages "en direct" (dynamique), la vérification de sécurité de la connexion, les GIFs et les autocollants ainsi que la langue de l'interface en français](/blog/20230103-simplex-chat-v4.4-disappearing-messages.md).

[06 déc. 2022. Revues de novembre et sortie de la v4.3 - avec les messages vocaux instantanés, la suppression irréversible de messages et une configuration améliorée des serveurs](/blog/20221206-simplex-chat-v4.3-voice-messages.md).

[08 nov. 2022. Audit de sécurité par Trail of Bits, nouveau site web et sortie de la v4.2](/blog/20221108-simplex-chat-v4.2-security-audit-new-website.md).

[28 sept. 2022. v4.0 : base de données de chat local chiffrée et beaucoup d'autres changements](/blog/20220928-simplex-chat-v4-encrypted-database.md).

[Toutes les mises à jour](/blog)

## Établir une connexion privée

Vous devez partager un lien ou scanner un code QR (en personne ou pendant un appel vidéo) pour établir une connexion et commencer à envoyer des messages.

Le canal par lequel vous partagez le lien n'a pas besoin d'être sécurisé - il suffit que vous puissiez confirmer qui vous a envoyé le message et que votre connexion SimpleX soit établie.

<img src="https://github.com/simplex-chat/.github/blob/master/profile/images/app1.png" alt="Établir une connexion privée" height="360"> <img src="https://github.com/simplex-chat/.github/blob/master/profile/images/arrow.png" height="360"> <img src="https://github.com/simplex-chat/.github/blob/master/profile/images/app2.png" alt="Conversation" height="360"> <img src="https://github.com/simplex-chat/.github/blob/master/profile/images/arrow.png" height="360"> <img src="https://github.com/simplex-chat/.github/blob/master/profile/images/app3.png" alt="Appel vidéo" height="360">

## ⚡ Installation rapide d'une application pour terminal

```sh
curl -o- https://raw.githubusercontent.com/simplex-chat/simplex-chat/stable/install.sh | bash
```

Une fois le client de chat installé, exécutez simplement `simplex-chat` depuis votre terminal.

![simplex-chat](/images/connection.gif)

En savoir plus sur [l'installation et l'utilisation de l'application terminal](/docs/lang/fr/CLI.md).

## Le modèle de la plateforme SimpleX

SimpleX est un réseau client-serveur doté d'une topologie de réseau unique qui utilise des nœuds de relais de messages redondants et jetables pour transmettre de manière asynchrone des messages via des files d'attente de messages unidirectionnelles (simplex), assurant l'anonymat du destinataire et de l'expéditeur.

Contrairement aux réseaux P2P, tous les messages passent par un ou plusieurs nœuds de serveur, qui n'ont même pas besoin de persistance. En fait, la [mise en place de serveurs SMP actuelle](https://github.com/simplex-chat/simplexmq#smp-server) utilise un stockage des messages en mémoire, ne conservant que les enregistrements de la file d'attente. SimpleX offre une meilleure protection des métadonnées que les conceptions P2P, car aucun identifiant global de participant n'est utilisé pour transmettre les messages, et évite [les problèmes des réseaux P2P](/docs/lang/fr/SIMPLEX.md#comparaison-avec-dautres-protocoles).

Contrairement aux réseaux fédérés, les nœuds serveurs **ne possèdent pas d'enregistrements des utilisateurs**, **ne communiquent pas entre eux** et **ne stockent pas les messages** après leur livraison aux destinataires. Il n'existe aucun moyen de découvrir la liste complète des serveurs participant au réseau SimpleX. Cette conception permet d'éviter le problème de visibilité des métadonnées que connaissent tous les réseaux fédérés et protège mieux contre les attaques à l'échelle du réseau.

Seuls les appareils clients disposent d'informations sur les utilisateurs, leurs contacts et leurs groupes.

Voir le [livre blanc de SimpleX](https://github.com/simplex-chat/simplexmq/blob/stable/protocol/overview-tjr.md) pour plus d'informations sur les objectifs et la conception technique de la plateforme.

Voir [SimpleX Chat Protocol](/docs/protocol/simplex-chat.md) pour le format des messages envoyés entre les clients de chat sur [SimpleX Messaging Protocol](https://github.com/simplex-chat/simplexmq/blob/stable/protocol/simplex-messaging.md).

## Vie privée : détails techniques et limites

SimpleX Chat est un projet en cours - nous publions des améliorations dès qu'elles sont prêtes. Vous devez décider si l'état actuel est suffisant pour votre scénario d'utilisation.

Ce qui est déjà mis en place :

1. Au lieu des identifiants de profil utilisateur utilisés par toutes les autres plateformes, même les plus privées, SimpleX utilise des identifiants par paire et par file d'attente (2 adresses pour chaque file d'attente de messages unidirectionnelle, avec une 3ème adresse optionnelle pour les notifications push sur iOS, 2 files d'attente dans chaque connexion entre les utilisateurs). Cela rend l'observation du graphe du réseau au niveau de l'application plus difficile, car pour `n` utilisateurs il peut y avoir jusqu'à `n * (n-1)` files de messages.
2. Chiffrement de bout en bout dans chaque file d'attente de messages en utilisant [NaCl cryptobox](https://nacl.cr.yp.to/box.html). Ce système est ajouté pour permettre la redondance à l'avenir (en faisant passer chaque message par plusieurs serveurs), pour éviter d'avoir le même texte chiffré dans différentes files d'attente (qui ne serait visible pour l'attaquant que si TLS est compromis). Les clés de chiffrement utilisées pour ce chiffrement ne sont pas tournées, mais nous prévoyons de faire tourner les files d'attente. Des clés Curve25519 sont utilisées pour la négociation des clés.
3. Chiffrement [Double ratchet](https://signal.org/docs/specifications/doubleratchet/) de bout en bout de chaque conversation entre deux utilisateurs (ou membres d'un groupe). Il s'agit du même algorithme que celui utilisé dans Signal et dans de nombreuses autres applications de messagerie ; il fournit à la messagerie OTR un secret avant (chaque message est chiffré par sa propre clé éphémère), une récupération après effraction (les clés sont fréquemment renégociées dans le cadre de l'échange de messages). Deux paires de clés Curve448 sont utilisées pour l'accord de clé initial, la partie initiatrice transmet ces clés via le lien de connexion, la partie acceptante - dans l'en-tête du message de confirmation.
4. Couche supplémentaire de chiffrement utilisant la cryptobox NaCL pour les messages livrés du serveur au destinataire. Cette couche évite d'avoir tout texte chiffré en commun entre le trafic envoyé et reçu du serveur dans TLS (et il n'y a pas d'identifiants en commun non plus).
5. Plusieurs niveaux de remplissage de données pour contrer les attaques sur la taille des messages.
6. À partir de la v2 du protocole SMP (la version actuelle est la v4), toutes les métadonnées des messages, y compris l'heure de réception du message par le serveur (arrondie à la seconde), sont envoyées aux destinataires à l'intérieur d'une enveloppe chiffrée, de sorte que même si le protocole TLS est compromis, il ne peut être observé.
7. Seul TLS 1.2/1.3 est autorisé pour les connexions client-serveur, limité aux algorithmes cryptographiques : CHACHA20POLY1305_SHA256, Ed25519/Ed448, Curve25519/Curve448.
8. Pour se protéger contre les attaques par relecture, les serveurs SimpleX exigent le [tlsunique channel binding](https://www.rfc-editor.org/rfc/rfc5929.html) comme identifiant de session dans chaque commande client signée avec une clé éphémère par file d'attente.
9. Pour protéger votre adresse IP, tous les clients SimpleX Chat permettent d'accéder aux serveurs de messagerie via Tor - voir [v3.1 release announcement](/blog/20220808-simplex-chat-v3.1-chat-groups.md) pour plus de détails.
10. Chiffrement de la base de données locale avec une phrase secrète - vos contacts, groupes et tous les messages envoyés et reçus sont stockés de manière chiffrée. Si vous avez utilisé SimpleX Chat avant la version 4.0, vous devez activer le chiffrement via les paramètres de l'application.
11. Isolation du transport - des connexions TCP et des circuits Tor différents sont utilisés pour le trafic de différents profils d'utilisateurs, optionnellement - pour des connexions différentes avec des contacts et des membres de groupes.

Nous prévoyons d'ajouter bientôt :

1. Rotation automatique des files d'attente de messages. Actuellement, les files d'attente créées entre deux utilisateurs sont utilisées jusqu'à ce que la file d'attente soit modifiée manuellement par l'utilisateur ou que le contact soit supprimé. Nous prévoyons d'ajouter une rotation de file d'attente pour rendre ces identifiants temporaires et les faire tourner selon un calendrier à déterminer (par exemple, tous les X messages, ou toutes les X heures/jours).
2. Chiffrement des fichiers locaux. Actuellement, les images et les fichiers que vous envoyez et recevez sont stockés dans l'application sans être cryptés, vous pouvez les supprimer via `Paramètres / Base de données phrase secrète et exportation`.
3. Mélange de messages - ajout d'une latence à la livraison des messages, pour se protéger contre la corrélation du trafic par le temps de transmission des messages.

## Pour les développeurs

Vous pouvez :

- utiliser la bibliothèque SimpleX Chat pour intégrer des fonctionnalités de chat dans vos applications mobiles.
- créer des bots et services de chat en Haskell - voir un exemple de chat bot [simple](/apps/simplex-bot/) et un plus [avancé](/apps/simplex-bot-advanced/).
- créer des bots et des services de chat dans n'importe quelle langue en utilisant la CLI du terminal SimpleX Chat comme un serveur WebSocket local. Voir [TypeScript SimpleX Chat client](/packages/simplex-chat-client/) et un [exemple de chat bot en JavaScript](/packages/simplex-chat-client/typescript/examples/squaring-bot.js).
- exécuter [simplex-chat terminal CLI](/docs/lang/fr/CLI.md) pour exécuter des commandes de chat individuelles, par exemple pour envoyer des messages dans le cadre de l'exécution d'un script shell.

Si vous envisagez de développer avec la plateforme SimpleX, n'hésitez pas à nous contacter pour obtenir des conseils et de l'aide.

## Feuille de route

- ✅ Déploiement facile de serveur SimpleX avec stockage des messages en mémoire, sans aucune dépendance.
- ✅ Client terminal (console) avec support des groupes et des fichiers.
- ✅ Déploiement de serveur SimpleX en un clic sur Linode.
- ✅ Chiffrement de bout en bout à l'aide du protocole double ratchet avec une couche de chiffrement supplémentaire.
- ✅ Applications mobiles v1 pour Android et iOS.
- ✅ Notifications instantanées privées pour Android utilisant un service d'arrière-plan.
- ✅ Modèles de chat bot en Haskell.
- ✅ v2.0 - prise en charge des images et des fichiers dans les applications mobiles.
- ✅ Suppression manuelle de l'historique des chats.
- ✅ Appels audio et vidéo WebRTC chiffrés de bout en bout via les apps mobiles.
- ✅ Notifications instantanées préservant la confidentialité pour iOS à l'aide du service Apple Push Notification.
- ✅ Exportation et importation de la base de données de chat.
- ✅ Groupes de discussion dans les applications mobiles.
- ✅ Connexion aux serveurs de messagerie via Tor.
- ✅ Double adresse de serveur pour accéder aux serveurs de messagerie en tant que services cachés v3.
- ✅ Serveur de chat et SDK client TypeScript pour développer des interfaces de chat, des intégrations et des bots de chat (prêt à être annoncé).
- ✅ Mode incognito pour partager un nouveau nom aléatoire avec chaque contact.
- ✅ Chiffrement de la base de données de chat.
- ✅ Suppression automatique de l'historique des chats.
- ✅ Liens pour rejoindre des groupes et améliorer la stabilité des groupes.
- ✅ Messages vocaux (avec opt-out du destinataire par contact).
- ✅ Authentification de base pour les serveurs SMP (pour autoriser la création de nouvelles files d'attente).
- ✅ Affichage des messages supprimés, suppression complète des messages par l'expéditeur (avec opt-in du destinataire par contact).
- ✅ Blocage des captures d'écran et affichage dans les apps récentes.
- ✅ Configuration avancée du serveur.
- ✅ Messages éphèmére (avec opt-in du destinataire par contact).
- ✅ Messages "en direct" (dynamique).
- ✅ Vérification des contacts via un canal distinct extérieur.
- ✅ Plusieurs profils d'utilisateurs dans la même base de données de chat.
- ✅ Optionnellement, éviter de réutiliser la même session TCP pour plusieurs connexions.
- ✅ Conservation des brouillons de message.
- 🏗 Serveur de fichiers pour optimiser l'envoi efficace et privé de fichiers volumineux.
- 🏗 Amélioration des appels audio et vidéo.
- 🏗 Redondance et rotation des files d'attente SMP (mode manuel déjà fonctionnel).
- 🏗 Réduction de l'utilisation de la batterie et du trafic dans les grands groupes.
- 🏗 Prise en charge d'anciens OS d'Android et des processeurs 32 bits.
- Conversations éphémères/disparaissantes/OTR avec les contacts existants.
- Mot de passe/pin d'accès (avec mot de passe d'accès alternatif facultatif).
- Chiffrement des fichiers locaux de l'app.
- Messages vidéo.
- Amélioration de la navigation et de la recherche dans la conversation (développer et faire défiler jusqu'au message cité, faire défiler jusqu'aux résultats de la recherche, etc.)
- Confirmation de la distribution du message (avec option d'acceptation ou de refus de l'expéditeur par contact, TBC).
- Flux/diffusion.
- Widgets Web pour une interactivité personnalisée dans les chats.
- Automatismes et règles de chat programmables (réponses/transferts/suppressions/envois automatiques, rappels, etc.)
- Prise en charge du même profil sur plusieurs appareils.
- Client de bureau.
- Serveur d'identité préservant la confidentialité des adresses de contact/groupe basées sur le DNS pour simplifier la connexion et la découverte, mais non utilisé pour délivrer des messages :
  - conservez tous vos contacts et groupes même si vous perdez le domaine.
  - le serveur ne dispose pas d'informations sur vos contacts et groupes.
- Serveur hôte pour les grands groupes, les communautés et les canaux publics.
- Relais de distribution des messages pour les expéditeurs (pour dissimuler l'adresse IP des serveurs des destinataires et réduire le trafic).
- Relais SMP multi-nœuds à haute capacité.

## Rejoindre un groupe d'utilisateurs

Vous pouvez rejoindre le groupe anglophone général : [#SimpleX-Group](https://simplex.chat/contact#/?v=1-2&smp=smp%3A%2F%2Fhpq7_4gGJiilmz5Rf-CswuU5kZGkm_zOIooSw6yALRg%3D%40smp5.simplex.im%2FcIS0gu1h0Y8pZpQkDaSz7HZGSHcKpMB9%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEAKzzWAJYrVt1zdgRp4pD3FBst6eK7233DJeNElENLJRA%253D%26srv%3Djjbyvoemxysm7qxap7m5d5m35jzv5qq6gnlv7s4rsn7tdwwmuqciwpid.onion&data=%7B%22type%22%3A%22group%22%2C%22groupLinkId%22%3A%228mazMhefXoM5HxWBfZnvwQ%3D%3D%22%7D). Just bear in mind that it has ~300 members now, and that it is fully decentralized, so sending a message and connecting to all members in this group will take some time, only join it if you:
- voir comment fonctionnent les grands groupes.
- le réseau n'est pas un problème (l'envoi de chaque message est de ~5mb).

Vous pouvez également rejoindre un nouveau groupe anglophone, plus petit, si vous souhaitez poser des questions sans trop de trafic : [#SimpleX-Group-2](https://simplex.chat/contact#/?v=1-2&smp=smp%3A%2F%2Fhpq7_4gGJiilmz5Rf-CswuU5kZGkm_zOIooSw6yALRg%3D%40smp5.simplex.im%2FQP8zaGjjmlXV-ix_Er4JgJ0lNPYGS1KX%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEApAgBkRZ3x12ayZ7sHrjHQWNMvqzZpWUgM_fFCUdLXwo%253D%26srv%3Djjbyvoemxysm7qxap7m5d5m35jzv5qq6gnlv7s4rsn7tdwwmuqciwpid.onion&data=%7B%22type%22%3A%22group%22%2C%22groupLinkId%22%3A%22xWpPXEZZsQp_F7vwAcAYDw%3D%3D%22%7D)

Il existe également plusieurs groupes dans des langues autres que l'anglais, dans lesquelles nous avons traduit l'interface de l'application. Ces groupes sont destinés à tester et à poser des questions aux autres utilisateurs de SimpleX Chat. Nous ne répondons pas toujours aux questions dans ces groupes, alors veuillez les poser dans l'un des groupes anglophones.

- [\#SimpleX-DE](https://simplex.chat/contact#/?v=1-2&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2FkIEl7OQzcp-J6aDmjdlQbRJwqkcZE7XR%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEAR16PCu02MobRmKAsjzhDWMZcWP9hS8l5AUZi-Gs8z18%253D%26srv%3Dbylepyau3ty4czmn77q4fglvperknl4bi2eb2fdy2bh4jxtf32kf73yd.onion&data=%7B%22type%22%3A%22group%22%2C%22groupLinkId%22%3A%22puYPMCQt11yPUvgmI5jCiw%3D%3D%22%7D) (Germanophone).
- [\#SimpleX-FR](https://simplex.chat/contact#/?v=1-2&smp=smp%3A%2F%2Fhpq7_4gGJiilmz5Rf-CswuU5kZGkm_zOIooSw6yALRg%3D%40smp5.simplex.im%2FvIHQDxTor53nwnWWTy5cHNwQQAdWN5Hw%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEAPdgK1eBnETmgiqEQufbUkydKBJafoRx4iRrtrC2NAGc%253D%26srv%3Djjbyvoemxysm7qxap7m5d5m35jzv5qq6gnlv7s4rsn7tdwwmuqciwpid.onion&data=%7B%22type%22%3A%22group%22%2C%22groupLinkId%22%3A%221FyUryBPza-1ZFFE80Ekbg%3D%3D%22%7D) (Francophone).
- [\#SimpleX-RU](https://simplex.chat/contact#/?v=1-2&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2FXZyt3hJmWsycpN7Dqve_wbrAqb6myk1R%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEAMFVIoytozTEa_QXOgoZFq_oe0IwZBYKvW50trSFXzXo%253D%26srv%3Dbylepyau3ty4czmn77q4fglvperknl4bi2eb2fdy2bh4jxtf32kf73yd.onion&data=%7B%22type%22%3A%22group%22%2C%22groupLinkId%22%3A%22xz05ngjA3pNIxLZ32a8Vxg%3D%3D%22%7D) (Russophone).
- [\#SimpleX-IT](https://simplex.chat/contact#/?v=1-2&smp=smp%3A%2F%2Fu2dS9sG8nMNURyZwqASV4yROM28Er0luVTx5X1CsMrU%3D%40smp4.simplex.im%2F0weR-ZgDUl7ruOtI_8TZwEsnJP6UiImA%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEAq4PSThO9Fvb5ydF48wB0yNbpzCbuQJCW3vZ9BGUfcxk%253D%26srv%3Do5vmywmrnaxalvz6wi3zicyftgio6psuvyniis6gco6bp6ekl4cqj4id.onion&data=%7B%22type%22%3A%22group%22%2C%22groupLinkId%22%3A%22e-iceLA0SctC62eARgYDWg%3D%3D%22%7D) (Italophone).

Vous pouvez rejoindre ces groupes soit en ouvrant ces liens dans l'application, soit en les ouvrant dans un navigateur de bureau et en scannant le code QR.

Rejoignez-nous via l'application pour échanger sur ce qui se passe et poser toutes vos questions !

## Traduire l'application

Grâce à nos utilisateurs et à [Weblate](https://hosted.weblate.org/engage/simplex-chat/), les applications de chat SimpleX sont traduites dans de nombreuses autres langues. Rejoignez nos traducteurs pour aider SimpleX à se développer plus rapidement !

Langues d'interface existantes :

Anglais (langue de développement)
Allemand : [@mlanp](https://github.com/mlanp)
Français : [@ishi_sama](https://github.com/ishi-sama)
Italien : [@unbranched](https://github.com/unbranched)
Russe : équipe du projet

Langues en cours : Chinois, hindi, japonais, néerlandais et [beaucoup d'autres](https://hosted.weblate.org/projects/simplex-chat/#languages). Nous ajouterons d'autres langues au fur et à mesure de l'achèvement de certaines d'entre elles. N'hésitez pas à suggérer de nouvelles langues et à nous contacter !

## Contribuer

Nous serions ravis de vous voir rejoindre le développement ! Vous pouvez contribuer à SimpleX Chat pour :

- traduire la page d'accueil du site web - il y a beaucoup de contenu que nous aimerions partager, cela permettrait d'attirer les nouveaux utilisateurs.
- écrire un tutoriel ou des recommandations sur l'hébergement de serveurs, l'automatisation des bots de chat, etc.
- développer des nouvelles fonctionnalités - veuillez nous contacter par chat pour que nous puissions vous aider à démarrer.

## Aidez-nous en faisant des dons

Un grand merci à tous ceux qui ont fait un don à SimpleX Chat !

Nous donnons la priorité à la confidentialité et à la sécurité des utilisateurs - ce serait impossible sans votre soutien.

Notre promesse à nos utilisateurs est que les protocoles SimpleX sont et resteront open-source, et dans le domaine public, - afin que tout le monde puisse créer les futures implémentations des clients et des serveurs. Nous créons une plateforme SimpleX basée sur les mêmes principes que l'e-mail et le web, mais bien plus privée et sécurisée.

Vos dons nous aident à collecter plus de fonds - tout montant, même le prix d'une tasse de café, ferait une grande différence pour nous.

Il est possible de faire un don via :

- [GitHub](https://github.com/sponsors/simplex-chat) - sans commission pour nous.
- [OpenCollective](https://opencollective.com/simplex-chat) - perçoit une commission, et accepte également les dons en crypto-monnaies.
- Adresse Monero : 8568eeVjaJ1RQ65ZUn9PRQ8ENtqeX9VVhcCYYhnVLxhV4JtBqw42so2VEUDQZNkFfsH5sXCuV7FN8VhRQ21DkNibTZP57Qt
- Adresse Bitcoin : 1bpefFkzuRoMY3ZuBbZNZxycbg7NYPYTG
- Adresse BCH : 1bpefFkzuRoMY3ZuBbZNZxycbg7NYPYTG
- Adresse Ethereum : 0x83fd788f7241a2be61780ea9dc72d2151e6843e2
- Adresse Solana : 43tWFWDczgAcn4Rzwkpqg2mqwnQETSiTwznmCgA2tf1L
- si vous souhaitez effectuer un don dans une autre crypto-monnaie, veuillez nous en informer via une demande GitHub ou un chat. Nous ajouterons l'adresse à la liste.

Merci,

Evgeny

Fondateur de SimpleX Chat

## Avertissements

[Protocoles et modèle de sécurité de SimpleX](https://github.com/simplex-chat/simplexmq/blob/master/protocol/overview-tjr.md) a été revu et a fait l'objet de nombreuses modifications et améliorations dans la v1.0.0.

L'audit de sécurité a été réalisé en octobre 2022 par [Trail of Bits](https://www.trailofbits.com/about), et la plupart des corrections ont été publiées dans la version 4.2.0 - voir [l'annonce](/blog/20221108-simplex-chat-v4.2-security-audit-new-website.md).

SimpleX Chat est une plateforme encore relativement jeune (les applications mobiles ont été lancées en mars 2022), il est donc possible que vous découvriez quelques bugs et des fonctionnalités manquantes. Nous apprécierions vraiment que vous nous fassiez part de tout ce qui doit être corrigé ou amélioré.

Les serveurs par défaut configurés dans l'application sont fournis dans la mesure du possible. Nous ne garantissons actuellement aucun accord de niveau de service, bien qu'historiquement nos serveurs aient eu un temps de disponibilité de plus de 99,9 % chacun.

Nous n'avons jamais fourni ou demandé l'accès à nos serveurs ou à toute information provenant de nos serveurs à des tiers. S'il nous est demandé de fournir un tel accès ou de telles informations, nous suivrons la procédure légale en vigueur.

Nous n'enregistrons pas les adresses IP des utilisateurs et nous n'effectuons aucune corrélation de trafic sur nos serveurs. Si la sécurité au niveau du trafic est primordiale, vous devez utiliser Tor ou un autre réseau similaire pour accéder aux serveurs de messagerie. Nous allons améliorer les applications client afin de réduire les possibilités de corrélation du trafic.

Pour en savoir plus, consultez la section [Conditions et politique de confidentialité](/PRIVACY.md).

## Contact de sécurité

Pour signaler une vulnérabilité de sécurité, veuillez nous envoyer un courriel à chat@simplex.chat. Nous coordonnerons la correction et la divulgation. Veuillez ne PAS signaler les vulnérabilités de sécurité via les problèmes GitHub.

Veuillez traiter toute découverte d'une éventuelle attaque par corrélation de trafic permettant de corréler deux conversations différentes au même utilisateur, autre que celle couverte par [le modèle de menace](https://github.com/simplex-chat/simplexmq/blob/stable/protocol/overview-tjr.md#threat-model), comme une vulnérabilité de sécurité, et suivez ce processus de publication.

## Licence

[AGPL v3](/LICENSE)

[<img src="https://github.com/simplex-chat/.github/blob/master/profile/images/apple_store.svg" alt="iOS app" height="42">](https://apps.apple.com/us/app/simplex-chat/id1605771084)
&nbsp;
[![Android app](https://github.com/simplex-chat/.github/blob/master/profile/images/google_play.svg)](https://play.google.com/store/apps/details?id=chat.simplex.app)
&nbsp;
[<img src="https://github.com/simplex-chat/.github/blob/master/profile/images/f_droid.svg" alt="F-Droid" height="41">](https://app.simplex.chat)
&nbsp;
[<img src="https://github.com/simplex-chat/.github/blob/master/profile/images/testflight.png" alt="iOS TestFlight" height="41">](https://testflight.apple.com/join/DWuT2LQu)
&nbsp;
[<img src="https://github.com/simplex-chat/.github/blob/master/profile/images/apk_icon.png" alt="APK" height="41">](https://github.com/simplex-chat/simplex-chat/releases/latest/download/simplex.apk)
