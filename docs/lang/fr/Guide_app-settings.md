---
title: Paramètres de l'application
---
# Paramètres de l'application

## Ouvrir les paramètres de l'application

Pour ouvrir les paramètres de l'application :

- Ouvrez l'application.
- Appuyez sur l'image de votre profil d'utilisateur en haut à droite de l'écran.
- Si vous avez plus d'un profil, appuyez à nouveau sur le profil actuel ou sélectionnez Paramètres.

## Vos paramètres de profil

Cette section est intitulée **"Vous "** dans les paramètres de l'application.

### Votre profil actif

Tapez sur votre avatar/nom pour mettre à jour vos noms de profil et avatar actuels.

Le nom d'affichage ne peut pas contenir d'espaces et il est recommandé d'utiliser des caractères latins et des chiffres pour faciliter la saisie de ces noms pour les utilisateurs qui utilisent[SimpleX Chat pour terminal](../CLI.md) (CLI).

**Veuillez noter** : Lorsque vous enregistrez votre profil, la mise à jour est envoyée à tous vos contacts (à l'exception des contacts avec lesquels vos profils incognito ont été partagés). Si vous avez un grand nombre de contacts, cela peut prendre plusieurs secondes.

### Vos profils de chat

Cette page permet d'ajouter et de configurer vos profils de chat. Veuillez consulter [Vos profils de chat](./chat-profiles.md) pour plus de détails.

### Incognito

Cette fonction est unique à SimpleX Chat - elle est indépendante des profils de chat.

Lorsque la fonction "Incognito" est activée, le nom et l'image de votre profil actuel ne sont PAS partagés avec vos nouveaux contacts. Cela permet d'établir des connexions avec d'autres personnes sans partager de données - lorsque vous établissez de nouvelles connexions ou rejoignez des groupes via un lien, un nouveau nom de profil aléatoire est généré pour chaque contact ou groupe.

Pour en savoir plus, consultez [cet article](../../blog/20220901-simplex-chat-v3.2-incognito-mode.md#incognito-mode).

### Votre adresse de contact SimpleX

<img src="../../blog/images/20221108-address1.png" width="288"> &nbsp;&nbsp; <img src="../../blog/images/20221108-address2.png" width="288">

Cette page vous permet de créer une adresse à long terme qui peut être utilisée par d'autres personnes pour se connecter avec vous. Contrairement aux liens d'invitation uniques, ces adresses peuvent être utilisées plusieurs fois, ce qui les rend utiles à partager en ligne, par exemple sur d'autres réseaux sociaux.

Lorsque des personnes se connectent à vous via cette adresse, vous recevez une demande de connexion que vous pouvez accepter ou rejeter. Vous pouvez configurer une fonction automatique permettant d'accepter les demandes de connexion et un message de bienvenue automatique qui sera envoyé aux nouveaux contacts.

Si vous commencez à recevoir trop de demandes via cette adresse, il est toujours prudent de la supprimer - toutes les connexions que vous avez créées via cette adresse resteront actives, car cette adresse n'est pas utilisée pour délivrer les messages.

Pour en savoir plus, consultez [cet article](../../blog/20221108-simplex-chat-v4.2-security-audit-new-website.md#auto-accept-contact-requests).

### Préférences de chat

Cette page permet de configurer les préférences de chat qui s'appliquent à tous les contacts - elles peuvent être modifiées séparément pour chaque contact :

- _messages éphémères_ - pour autoriser les messages éphémères dans les conversations avec vous, uniquement si vos contacts les autorisent.
- _supprimer pour tout le monde_ - pour permettre la suppression irréversible des messages (par défaut, les messages sont marqués comme supprimés, mais pas complètement effacés). Le paramètre "Oui" ne l'autorise que si vos contacts l'autorisent pour vous, et "Toujours" - même s'ils ne l'autorisent pas.
- _messages vocaux_ - pour autoriser l'envoi de messages vocaux.

Pour définir les préférences de chat de chaque contact, appuyez sur le nom du contact en haut de la conversation, puis choisissez "Préférences du contact".

Les propriétaires de groupes peuvent définir des préférences similaires pour leurs groupes, lors de la création du groupe ou ultérieurement : appuyez sur le nom du groupe en haut de la conversation, puis choisissez "Préférences du groupe".

## Paramètres de l'application

Cette section est intitulée **"Paramètres"** dans les paramètres de l'app.

### Notifications

Cette page permet de configurer le mode de notification : instantanée, périodique ou uniquement lorsque l'application est en cours d'exécution. Il y a quelques différences dans le fonctionnement de ces options sur iOS et Android. Pour en savoir plus sur le fonctionnement des notifications, consultez [cet article](../../blog/20220404-simplex-chat-instant-notifications.md).

Vous pouvez également configurer l'affichage de l'aperçu du message lors de son arrivée.

**Veuillez noter** : Pour que les notifications instantanées et périodiques fonctionnent sur Android, vous devez désactiver l'optimisation de l'alimentation lorsque l'application vous le demande, ou plus tard via les paramètres. De plus, certaines variantes du système Android nécessitent des paramètres supplémentaires, par exemple le système MIU sur les téléphones Xiaomi nécessite d'activer le "Démarrage automatique" de l'application pour que le service de notification fonctionne. Veuillez lire le guide [Don't kill my app] (https://dontkillmyapp.com/) pour connaître les paramètres éventuellement requis sur votre appareil.

De plus, les notifications instantanées ont actuellement la consommation de batterie la plus élevée - nous travaillons à la réduire pour qu'elle soit inférieure ou identique à celle des notifications périodiques.

### Réseau et serveurs

Cette page permet de configurer vos propres relais SMP et de modifier d'autres paramètres du réseau.

<img src="../../blog/images/20230204-transport.png" width="288">

#### Serveurs SMP

<img src="../../blog/images/20221206-server1.png" width="288"> &nbsp;&nbsp ; <img src="../../blog/images/20221206-server2.png" width="288"> &nbsp;&nbsp ; <img src="../../blog/images/20221206-server3.png" width="288">

Par défaut, l'application dispose de relais prédéfinis - vous pouvez les remplacer par les vôtres.

Cette page permet également de tester la connexion avec les serveurs.

Pour en savoir plus, consultez [cet article](../../blog/20221206-simplex-chat-v4.3-voice-messages.md#smp-servers-configuration-and-password).

#### Utiliser un proxy SOCKS (Android uniquement)

Cette option permet à l'application de se connecter via un proxy SOCKS qui devrait être fourni par une autre application fonctionnant sur votre appareil.

L'utilisation la plus typique est de faire fonctionner l'application Orbot qui fournit un proxy SOCKS pour se connecter via le réseau Tor, mais il peut s'agir d'une autre application et elle peut fournir des connexions via un autre réseau superposé.

#### Utiliser des hôtes .onion

##### Android

L'option **Utiliser les hôtes .onion** n'est disponible que lorsque l'option **Utiliser le proxy SOCKS** est activée. Vous pouvez choisir :

- _non_ : ne jamais utiliser les hôtes .onion. Choisissez cette option si votre proxy SOCKS ne se connecte pas via le réseau Tor.
- _quand disponible_ (par défaut) : lorsque le proxy SOCKS est activé, l'application suppose qu'il fournit des connexions via le réseau Tor et utilise les adresses .onion lorsque les relais SMP les incluent dans leur configuration.
- _nécessaire_ : toujours utiliser les hôtes .onion. Choisissez cette option si votre proxy SOCKS se connecte via le réseau Tor et que vous souhaitez éviter les connexions sans Tor. Dans ce cas, si l'adresse du relais SMP n'inclut pas .onion host, la connexion échouera.

##### iOS

<img src="../../blog/images/20220901-onion1.png" width="330"> &nbsp ; <img src="../../blog/images/20220901-onion2.png" width="330">

Bien qu'iOS ne prenne pas en charge les proxies SOCKS, vous pouvez installer l'application Orbot qui fonctionne comme un fournisseur de VPN. Vous pouvez choisir :

- _non_ (par défaut) : ne pas utiliser les hôtes .onion. Choisissez cette option si vous n'utilisez pas Orbot ou si vous utilisez un VPN qui proxifie les connexions via un autre réseau superposé.
- _quand disponible_ : utiliser les adresses d'hôtes .onion lorsque les relais SMP les incluent dans leur configuration. Le VPN Orbot doit être activé pour que cette option fonctionne.
- nécessaire_ : toujours utiliser les hôtes .onion. Choisissez cette option si vous utilisez Orbot VPN et que vous voulez éviter les connexions sans Tor. Dans ce cas, si l'adresse du relais SMP n'inclut pas .onion host, la connexion échouera. Si vous utilisez cette option, vous pouvez activer l'option " Désactiver Orbot pour le trafic non onion" dans les paramètres d'Orbot pour que le reste du trafic de votre appareil n'utilise pas Tor.

**Veuillez noter que le VPN sur iOS peut envoyer une partie du trafic vers le réseau habituel, si, par exemple, l'application VPN tombe en panne. Vous pouvez configurer l'activation du mode VPN toujours actif sur les appareils iOS gérés, mais cela n'est pas applicable à la plupart des appareils individuels.

#### Isolement du transport (BETA)

Cette option n'est disponible que si vous avez activé les outils de développement.

Veuillez lire les détails dans [cet article](../../blog/20230204-simplex-chat-v4-5-user-chat-profiles.md#transport-isolation).

#### Paramètres réseau avancés

<img src="../../blog/images/20220808-network.png" width="330">

Si votre réseau est lent et que les connexions aux serveurs échouent sans cesse (vous verriez un indicateur sur vos contacts), augmentez les délais d'attente pour le TCP et le protocole dans cette page.

### Confidentialité et sécurité

#### SimpleX Lock

SimpleX Lock, lorsqu'il est activé, requiert l'authentification de l'appareil lorsque vous ouvrez l'application ou que vous utilisez certaines fonctions sensibles en matière de sécurité ou de confidentialité.

Il vous sera proposé de l'activer après avoir ouvert l'application plusieurs fois.

Pour l'activer ultérieurement :

- Ouvrez les paramètres de l'application (#opening-the-app-settings).
- Tapez sur "Confidentialité et sécurité".
- Activez le curseur "SimpleX Lock".
- Confirmez les informations d'identification de votre téléphone.

C'est fait ! Désormais, vous devrez vous authentifier lorsque vous démarrerez ou reprendrez l'application après 30 secondes en arrière-plan.

#### Protéger l'écran de l'application

<img src="../../blog/images/20221206-protect.png" width="330">

Cette option permet de masquer l'écran de l'application dans les applications récentes ; elle est activée par défaut. Sur Android, elle empêche également les captures d'écran.

#### Accepter automatiquement les images

Cette option peut réduire la confidentialité : vos contacts sauront que vous êtes en ligne.

- Ouvrez les paramètres de l'application (#opening-the-app-settings).
- Tapez sur "Confidentialité et sécurité".
- Activez l'option "Acceptation automatique d'images".

#### Envoyer des aperçus de liens

L'activation de l'envoi d'aperçus de liens peut réduire la confidentialité - votre application chargera l'aperçu du lien à partir du site web.

- Ouvrez les paramètres de l'application (#opening-the-app-settings).
- Tapez sur "Confidentialité et sécurité".
- Activez l'option "Envoyer des aperçus de liens".

#### Liens SimpleX

Cette option affecte la manière dont les liens permettant de se connecter à d'autres utilisateurs SimpleX ou de rejoindre des groupes sont affichés dans les conversations. Vous pouvez choisir entre :

- _description_ (par défaut) : seule la description du lien et le nom d'hôte du serveur sont affichés. Le lien ne sera pas ouvert dans le navigateur.
- Lien complet : le lien complet est affiché. Le lien ne sera toujours pas ouvert dans le navigateur.
- _via navigateur_ : le lien complet est affiché, et il sera ouvert dans le navigateur. Dans ce cas, si le domaine du lien est différent de simplex.chat, le lien sera affiché en rouge, car il pourrait être malveillant.

En savoir plus sur [Sécurité des liens SimpleX](../../blog/20221206-simplex-chat-v4.3-voice-messages.md#privacy-and-security-of-simplex-invitation-links).

### Apparence

Cette page permet de configurer :

- la langue de l'interface
- l'icône de l'application
- la couleur de l'accent

### Mot de passe et exportation de la base de données

Cette page permet de modifier la phrase d'authentification de la base de données, d'exporter et d'importer la base de données et de configurer la période de conservation des messages.

Pour en savoir plus, consultez la page [Gérer vos données](./managing-data.md).

## Aide et commentaires

Cette section contient des informations sur l'utilisation de l'application et des liens pour se connecter à l'équipe. Veuillez utiliser [Envoyer des questions et des idées](https://simplex.chat/contact#/?v=1&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2FK1rslx-m5bpXVIdMZg9NLUZ_8JBm8xTt%23%2F%3Fv%3D1%26dh%3DMCowBQYDK2VuAyEALDeVe-sG8mRY22LsXlPgiwTNs9dbiLrNuA7f3ZMAJ2w%253D%26srv%3Dbylepyau3ty4czmn77q4fglvperknl4bi2eb2fdy2bh4jxtf32kf73yd.onion) pour vous connecter à nous via le chat pour poser des questions, faire des suggestions et signaler des problèmes.

## Support SimpleX Chat

- contribuer - le lien vers les informations sur la façon de contribuer et de faire un don au projet.
- noter l'application - noter et évaluer l'application sur l'App Store ou le Play Store - vos commentaires nous aident beaucoup.
- star sur GitHub - cela nous aide aussi beaucoup à grandir.

Merci pour votre soutien !

## Outils pour les développeurs

Cette page contient des options qui ne sont normalement nécessaires qu'aux développeurs d'applications et qui peuvent être utilisées pour déboguer l'application au cas où quelque chose ne fonctionnerait pas.

### Console de chat

Vous pouvez ici voir et utiliser la commande de la console avec le système de chat. Aucune de ces informations n'est envoyée sur le réseau, il s'agit d'une communication interne entre les différentes parties de l'application.

Attention, certaines commandes peuvent perturber le fonctionnement de l'application, ne les utilisez que si vous savez ce que vous faites ou si vous avez reçu des instructions de l'équipe.

**Veuillez noter que le journal de la console peut contenir ce qui ressemble à des erreurs. A moins que vous ne rencontriez des problèmes dans l'interface utilisateur de l'application, ces erreurs ne doivent pas être considérées comme un dysfonctionnement de l'application - il s'agit probablement d'un comportement normal et attendu.

### Confirmer les mises à jour de la base de données

Cette option est désactivée par défaut - la base de données de l'application est migrée vers la nouvelle version sans aucune confirmation. À partir de la version 4.6.2, ces migrations sont réversibles - vous pouvez revenir à la (aux) version(s) précédente(s) de l'application (pas avant la version 4.6.1). Si vous souhaitez qu'une confirmation vous soit demandée à chaque fois que la base de données est mise à niveau, vous pouvez activer cette option - cela n'est pas recommandé, car cela se produit presque à chaque mise à jour de l'application, et ne devrait pas poser de problème.

### Afficher les options du développeur

Cette option active actuellement l'option [Mode isolement du transport](#transport-isolation-beta) et permet également d'afficher les identifiants de la base de données dans les contacts, les groupes et les membres des groupes afin de simplifier le débogage via les commandes de la console.
