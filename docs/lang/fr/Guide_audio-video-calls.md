---
title: Appels audio & vidéo
---
# Appels audio & vidéo

SimpleX Chat vous permet de passer des appels audio et vidéo chiffrés de bout en bout avec vos contacts via WebRTC. Remarque : les appels de groupe ne sont pas pris en charge pour le moment.

## Passer et répondre à un appel

### Comment passer un appel audio

1. Tapez sur un contact.
2. Tapez sur l'icône de téléphone à droite du nom de votre contact en haut de l'écran.

### Comment passer un appel vidéo

1. Tapez sur un contact.
2. Tapez sur les trois points verticaux dans le coin supérieur droit de l'écran pour accéder à d'autres options.
3. Choisissez **Appel vidéo**.

### Répondre aux appels

En cas d'appel entrant, trois options s'offrent à vous :

- accepter : pour prendre l'appel
- rejeter : pour rejeter l'appel, _sans_ en informer son émetteur.
- ignorer : rejeter temporairement l'appel, mais de manière à pouvoir l'accepter plus tard, si le correspondant est toujours en attente, vous pouvez répondre par la suite via le message **Accepter l'appel** dans la conversation avec ce contact.

Il n'y a pas de limite de temps pour que l'invitation à l'appel reste active - tant que le contact attend, vous pouvez accepter l'appel à n'importe quel moment.

L'appel peut être accepté à partir de l'écran de verrouillage, à la fois sur Android (il doit être activé via les options) et sur iOS (par défaut, en utilisant l'interface d'appel native d'iOS qui peut être désactivée).

### Appels sur l'écran de verrouillage sur Android

SimpleX Chat affiche par défaut un appel entrant sur l'écran de verrouillage de votre appareil. Cependant, vous pouvez modifier ce comportement dans le menu des paramètres de l'application.

1. Ouvre le menu des paramètres de l'application.
2. Appuyez sur **Appels audio et vidéo**.
3. Dans la liste déroulante **Appels sur l'écran de verrouillage**, choisissez l'une des trois options suivantes :
   - Désactiver - l'appel s'affiche sous forme de notification.
   - Afficher - l'appel s'affiche sur l'écran de verrouillage, vous devez déverrouiller l'appareil et l'application pour l'accepter.
   - Accepter - l'appel peut être accepté ou rejeté directement à partir de l'écran de verrouillage, sans ouvrir l'application.

**Remarque** : certains systèmes/appareils Android interdisent l'affichage en plein écran sur l'écran de verrouillage - dans ce cas, l'appel s'affichera sous la forme d'une notification habituelle.

### Appels sur l'écran de verrouillage sur iOS

<img src="../../blog/images/20230328-call1.png" width="288">

Par défaut, SimpleX Chat utilise l'interface d'appel native d'iOS, lorsqu'elle est autorisée, pour afficher les appels entrants sur l'écran de verrouillage. Vous pouvez la désactiver :

1. Ouvrez le menu des paramètres de l'application.
2. Appuyez sur **Appels audio et vidéo**.
3. Désactivez l'option **Utiliser l'interface d'appel iOS**.

**Remarque : l'interface d'appel d'iOS permet d'accepter les appels sans déverrouiller l'appareil et l'application. Si cela n'est pas souhaitable, désactivez-la - dans ce cas, les appels s'afficheront sous forme de notifications.

Pour en savoir plus, consultez [cet article](../../blog/20230328-simplex-chat-v4-6-hidden-profiles.md#improved-audiovideo-calls).

## Paramètres d'appel avancés

### Serveurs WebRTC ICE

<img src="../../blog/images/20220928-ice-servers.png" width="330">

SimpleX Chat utilise un serveur relais prédéfini pour cacher votre adresse IP à vos contacts par défaut, mais il peut également observer la durée de vos appels. Si vous ne souhaitez pas cela, vous pouvez configurer et utiliser vos propres serveurs de relais WebRTC à la place pour un meilleur contrôle de vos appels.

1. Ouvrez le menu des paramètres de l'application.
2. Appuyez sur **Appels audio et vidéo**.
3. Appuyez sur **Serveurs WebRTC ICE**.
4. Activez l'interrupteur **Configurer les serveurs ICE**.
5. Saisissez les adresses de vos serveurs ICE (une par ligne).
6. Appuyez sur **Sauvegarder**.

**Remarque : contrairement aux relais de messagerie (serveurs SMP), la configuration des serveurs WebRTC ICE est stockée sur l'appareil actuel, et non dans la base de données du chat. Si vous transférez la base de données du chat vers un autre appareil, vous devez mettre à jour cette configuration.

### Toujours utiliser un relais

Les appels audio et vidéo sur SimpleX Chat sont acheminés par défaut via un serveur relais TURN. En option, vous pouvez le désactiver et utiliser le peer-to-peer (P2P) à la place, lorsque votre réseau le supporte. Cependant, votre adresse IP sera connue de vos contacts.

1. Ouvrez le menu des paramètres de l'application.
2. Appuyez sur **Appels audio et vidéo**.
3. Activez l'option **Toujours utiliser un relais** pour utiliser un serveur relais ou désactivez-la pour le P2P.

**Remarque : la désactivation de cette option permet les appels P2P, mais n'interdit pas l'utilisation des relais TURN - si votre fournisseur de réseau bloque les connexions P2P, l'appel utilisera toujours les relais s'ils sont disponibles. Pour interdire l'utilisation des relais, vous devez modifier la configuration du serveur WebRTC ICE pour n'inclure que les serveurs STUN, par exemple :
