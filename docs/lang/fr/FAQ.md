---
title: Foire aux questions
permalink: /faq/index.html
revision: 23.04.2024
---

| 23.04.2024 | FR, [EN](/docs/FAQ.md)

# Foire aux questions

[Comment l'utiliser](#how-to-use-it)
- [Je n'ai personne avec qui discuter ! Où puis-je trouver des groupes ?](#i-have-nobody-to-chat-with-where-can-i-find-any-groups)
- [Qu'est-ce qu'une base de données ? Que puis-je faire avec ?](#what-is-database-what-can-i-do-with-it)
- [Puis-je envoyer des fichiers via SimpleX ?](#can-i-send-files-over-simplex)
- [Qu'est-ce que le profil incognito ?](#whats-incognito-profile)
- [Comment fonctionnent les invitations ?](#how-do-invitations-work)
- [Comment configurer et supprimer des groupes ?](#how-to-configure-and-delete-groups)
- [Y a-t-il des réactions aux messages ? Puis-je répondre directement à des messages spécifiques ?](#are-there-any-reactions-to-messages-can-i-answer-specific-messages-directly)
- [Que signifient les coches ?](#what-do-checkmarks-mean)
- [Puis-je utiliser le même profil sur mon ordinateur de bureau ? Les messages sont-ils synchronisés entre les plates-formes ?](#can-i-use-the-same-profile-on-desktop-do-messages-sync-cross-platform)

[Dépannage](#troubleshooting)
- [Je ne reçois pas de messages ou de notifications de messages](#i-do-not-receive-messages-or-message-notifications)
- [Je ne vois pas la deuxième coche sur les messages que j'ai envoyés](#i-do-not-see-the-second-tick-on-the-messages-i-sent)
- [Je vois l'aperçu de l'image mais je ne peux pas l'ouvrir](#i-see-image-preview-but-cannot-open-the-image)
- [Je ne peux pas écouter un message vocal](#i-cannot-play-a-voice-message)
- [Les appels audio ou vidéo ne se connectent pas](#audio-or-video-calls-do-not-connect)
- [Appels audio ou vidéo sans chiffrement e2e](#audio-or-video-calls-without-e2e-encryption)
- [J'ai cliqué sur le lien pour me connecter, mais je n'ai pas pu me connecter.](#i-clicked-the-link-to-connect-but-could-not-connect)

[Confidentialité et sécurité](#privacy-and-security)
- [SimpleX prend-il en charge la cryptographie post quantique ?](#does-simplex-support-post-quantum-cryptography)
- [Quelles données utilisateur peuvent être fournies sur demande ?](#what-user-data-can-be-provided-on-request)
- [SimpleX protège-t-il mon adresse IP ?](#does-simplex-protect-my-ip-address)
- [Puis-je héberger mes propres relais ?](#can-i-host-my-own-relays)

[Financement et modèle économique](#funding-and-business-model)
- [Comment êtes-vous financé ?](#how-are-you-funded)
- [Pourquoi les sociétés de capital-risque ?](#why-vcs)
- [Quel sera le modèle d'entreprise ?](#what-will-be-the-business-model)

## Comment l'utiliser

### Je n'ai personne avec qui discuter ! Où puis-je trouver des groupes ?

Veuillez d'abord consulter notre [Annuaire des groupes](./DIRECTORY.md). Vous pourriez trouver des groupes intéressants et rencontrer des gens encore plus intéressants.

### Qu'est-ce qu'une base de données ? Que puis-je faire avec ?

La base de données est essentielle au bon fonctionnement de SimpleX Chat. En comparaison avec les fournisseurs de messagerie centralisés, c'est _l'utilisateur_ qui est responsable de l'entretien de ses données. D'autre part, l'utilisateur est sûr que _personne d'autre que lui_ n'y a accès. Pour en savoir plus : [Base de données](./guide/managing-data.md).

### Puis-je envoyer des fichiers via SimpleX ?

Bien sûr ! Ce faisant, vous utilisez un protocole _de pointe_ qui réduit considérablement les fuites de métadonnées. Lisez-en plus à ce sujet : [Protocole XFTP](../../blog/20230301-simplex-file-transfer-protocol.md).

### Qu'est-ce que le profil incognito ?

Cette fonction est unique à SimpleX Chat - elle est indépendante des profils de chat. 

Lorsque le "Mode Incognito" est activé, le nom et l'image de ton profil sont cachés à tes nouveaux contacts. Il permet des connexions anonymes avec d'autres personnes sans aucune donnée partagée - lorsque tu fais de nouvelles connexions ou que tu rejoins des groupes via un lien, un nouveau nom de profil aléatoire sera généré pour chaque connexion. 

### Comment fonctionnent les invitations ?

Il s'agit d'un processus assez complexe, mais heureusement, tout se passe en arrière-plan, de sorte que l'utilisation est simple.

Lorsque quelqu'un se connecte à vous via votre adresse, il demande à votre client s'il souhaite établir une connexion. Ensuite, vous pouvez accepter ou refuser.
Si vous êtes intéressé, lisez la suite : [Adresses et invitations](./guide/making-connections.md).

### Comment configurer et supprimer des groupes ?

Veuillez consulter : [Guide de l'utilisateur](./guide/secret-groups.md).

### Y a-t-il des réactions aux messages ? Puis-je répondre directement à des messages spécifiques ?

Oui ! Actuellement, six emojis sont disponibles. De plus, vous pouvez répondre à un message spécifique en le maintenant enfoncé et en sélectionnant _Reply_.

### Que signifient les coches ?

C'est très simple :
- une coche - le message est remis au relais (le serveur).
- deux coches - le message est remis à l'appareil du destinataire.
La mention "envoyé" signifie que le relais accepte de délivrer le message, la mention "délivré" signifie que le message est stocké sur l'appareil du destinataire.

[Voir aussi](#i-do-no-see-the-second-tick-on-the-messages-i-sent)

### Puis-je utiliser le même profil sur mon ordinateur de bureau ? Les messages sont-ils synchronisés entre les plates-formes ?

Vous pouvez utiliser votre profil depuis un appareil mobile sur un ordinateur de bureau. Toutefois, pour ce faire, vous devez être sur le même réseau, à la fois sur votre appareil mobile et sur votre ordinateur de bureau. En savoir plus : [Release info](../blog/20231125-simplex-chat-v5-4-link-mobile-desktop-quantum-resistant-better-groups.md#link-mobile-and-desktop-apps-via-secure-quantum-resistant-protocol).

## Dépannage

### Je ne reçois pas de messages ou de notifications de messages

Il peut y avoir plusieurs raisons pour lesquelles les messages de votre contact ne vous parviennent pas :

**Vous ou votre contact ne pouvez pas vous connecter au serveur que vous utilisez pour recevoir les messages de votre contact**.

Vous pouvez vérifier quel serveur est utilisé pour recevoir les messages en appuyant sur le nom du contact au-dessus de la conversation.

Vous pouvez également effectuer des tests pour ce serveur à partir de l'application Paramètres réseau.

Demandez à votre contact s'il a coché une seule fois le message pour déterminer si l'envoi du message a échoué ou si vous ne l'avez pas reçu.

**L'envoi du message a été bloqué en raison d'un bogue non résolu.**

Le redémarrage complet de l'application est la solution pour reprendre l'envoi du message.

Pour le faire sur iOS, il suffit de fermer l'application (balayez vers le haut à partir des applications ouvertes) et de l'ouvrir à nouveau.

Pour le faire sur Android, choisissez Redémarrer dans les paramètres de l'application. Le simple fait de fermer et de rouvrir l'application ne redémarre pas le service de messagerie.

**Votre système d'exploitation Android termine l'application lorsqu'elle est en arrière-plan.**

Vérifiez les paramètres de la batterie pour l'application - elle doit être réglée sur Sans restriction.

Pour certains appareils, il peut y avoir des options supplémentaires pour empêcher l'application d'être terminée - par exemple, sur Xiaomi, vous devez activer le paramètre Démarrage automatique pour l'application. Veuillez consulter le site https://dontkillmyapp.com pour connaître les paramètres supplémentaires de votre appareil.

**Les notifications iOS n'ont pas réussi à s'initialiser correctement.**

Vérifiez la couleur de l'icône du boulon à côté de Notifications dans les paramètres de l'application - elle devrait être verte.

Si ce n'est pas le cas, ouvrez les notifications, désactivez-les (choisissez Désactivé / Local), puis réactivez-les - vous devez le faire lorsque vous avez une connexion Internet.

Si ce qui précède n'a rien donné, il se peut qu'iOS n'ait pas réussi à émettre le jeton de notification - nous avons constaté ce problème à plusieurs reprises. Dans ce cas, le redémarrage complet de l'appareil devrait aider.

**Le serveur de messagerie ou de notification est en cours de maintenance**

Veuillez vérifier l'état actuel des serveurs prédéfinis sur [https://status.simplex.chat](https://status.simplex.chat). Vous pouvez également vous connecter au robot d'état via le code QR sur cette page - il enverra les mises à jour lorsque le serveur est hors ligne pour maintenance, et également lorsque les nouvelles versions de l'application sont publiées.

### Je ne vois pas la deuxième coche sur les messages que j'ai envoyés

Il se peut que vous ne voyiez pas la deuxième coche sur les messages que vous avez envoyés pour les raisons suivantes :

- votre contact n'est pas en ligne et n'a pas reçu votre message.
- Il est possible que la distribution des messages à votre contact ou à vous-même soit perturbée - voir [Je ne reçois pas de messages](#i-do-ne-receive-messages-or-message-notifications) - veuillez vérifier auprès de votre contact via un autre canal s'il a reçu votre message. Si le message a été délivré, cela signifie que votre appareil n'a pas reçu la notification de livraison.
- Il est possible que votre contact ait désactivé l'envoi des accusés de réception - cette désactivation peut concerner un contact en particulier ou tous les contacts - veuillez vérifier auprès de votre contact.

### Je vois l'aperçu de l'image mais je ne peux pas l'ouvrir

Les raisons suivantes peuvent être à l'origine de ce problème :
- votre contact n'a pas fini de télécharger le fichier image, peut-être en fermant l'application trop rapidement. Lorsque le fichier image est entièrement téléchargé, une coche apparaît dans le _coin supérieur droit_ de l'image.
- votre appareil ne parvient pas à la recevoir. Vérifiez la connectivité du serveur et effectuez des tests, et essayez également d'augmenter les délais d'attente du réseau dans les paramètres avancés du réseau. La réception des fichiers a été considérablement améliorée dans la version 5.7 - assurez-vous que vous utilisez la dernière version.
- Le fichier a expiré et ne peut plus être reçu. Les fichiers ne peuvent être reçus que pendant 2 jours après leur envoi, après quoi ils ne seront plus disponibles et afficheront un X dans le coin supérieur droit.

### Je ne peux pas écouter un message vocal

Cela peut se produire pour les mêmes raisons que pour les [images](#i-see-image-preview-but-cannot-open-the-image).

Veuillez vérifier les paramètres de votre réseau et vous assurer que vous utilisez la dernière version de l'application.

Veuillez signaler ces problèmes si vous utilisez la version 5.7 ou une version plus récente.

### Les appels audio ou vidéo ne se connectent pas

L'application utilise WebRTC pour les appels. Vérifiez que vous pouvez vous connecter aux serveurs configurés dans l'application : stun.simplex.im et turn.simplex.im - voir [troubleshooting WebRTC](./WEBRTC.md#troubleshoot).

Si vous pouvez vous connecter au serveur, veuillez nous signaler ce problème en privé, en incluant les informations suivantes :

- comment vous vous connectez au réseau : WiFi, réseau mobile, fournisseur VPN - plus vous fournirez d'informations, mieux ce sera.

- la version de l'application et la plateforme. Pour les applications mobiles, il serait utile d'enregistrer l'écran des deux appareils pendant les appels infructueux et de le partager avec nous.

- si le problème concerne l'application de bureau, quel est le navigateur utilisé pour les appels. Dans ce cas également, veuillez vérifier la console du navigateur pendant l'appel et envoyez-nous le journal, idéalement des deux côtés des appels infructueux.

Merci de nous aider à déboguer et à améliorer les appels.

### Appels audio ou vidéo sans chiffrement e2e

Pendant l'appel, l'application indique si l'appel dispose ou non d'un chiffrement de bout en bout.

Si l'un des interlocuteurs utilise une application Android (ou de bureau), l'appel utilise la vue web (ou le navigateur) du système Android. Certains systèmes plus anciens ne prennent pas en charge le chiffrement du flux multimédia, auquel cas l'appel se connectera sans chiffrement.

Pour déterminer s'il s'agit d'une limitation de votre appareil, de celui de votre contact ou des deux :
- si certains de vos appels ont le chiffrement e2e et d'autres non, il s'agit certainement de l'ancienne version de webview ou de l'ancien navigateur de vos contacts - demandez-leur de mettre à jour.
- si vous n'êtes pas sûr, vous pouvez vérifier à quel moment la mention "pas de chiffrement e2e" apparaît :
  - s'il s'affiche lorsque l'appel sonne sur votre appareil, cela signifie que l'appareil de votre contact ne prend pas en charge le chiffrement des appels.
  - s'il s'affiche sur votre écran dès que vous commencez l'appel, c'est que l'appareil de votre interlocuteur ne prend pas en charge le chiffrement des appels.
  - si, au début de l'appel, votre appareil affiche "chiffrement e2e" mais que, lorsque votre interlocuteur accepte l'appel, il passe à "pas de chiffrement e2e", c'est uniquement l'appareil de votre interlocuteur qui ne prend pas en charge le chiffrement.

Vous devez mettre à jour la webview (certains systèmes Android le permettent), le système Android ou l'appareil pour qu'il prenne en charge le chiffrement e2e dans les appels - toutes les webviews modernes (et les navigateurs) le prennent en charge.

### J'ai cliqué sur le lien pour me connecter, mais je n'ai pas pu me connecter.

Si vous avez confirmé la connexion dans l'application, la connexion en attente sera affichée dans la liste des chats - vous pouvez lui attribuer un nom, afin de savoir de qui il s'agit lorsque votre contact est connecté (par exemple, s'il choisit un nom que vous ne reconnaissez pas).

Pour que la connexion soit établie, votre contact doit être en ligne et avoir l'application en cours d'exécution - demandez-lui d'ouvrir l'application, et essayez d'avoir l'application ouverte en même temps - cela permettra d'établir la connexion plus rapidement.

Une fois la connexion établie, vous n'avez pas besoin d'être en ligne en même temps pour envoyer des messages.

## Confidentialité et sécurité

### SimpleX prend-il en charge la cryptographie post quantique ?

Oui ! Pour en savoir plus sur l'ajout d'un chiffrement résistant aux quanta dans SimpleX Chat et sur les différentes propriétés du chiffrement de bout en bout, consultez [ce billet](../../blog/20240314-simplex-chat-v5-6-quantum-resistance-signal-double-ratchet-algorithm.md).

### Quelles données utilisateur peuvent être fournies sur demande ?

Notre objectif est de faire en sorte qu'aucune donnée utilisateur et le minimum absolu de métadonnées nécessaires au fonctionnement du réseau ne puissent être divulgués par les opérateurs d'infrastructure, quelles que soient les circonstances.

Veuillez consulter notre [Politique de confidentialité](../PRIVACY.md) et nos [Rapports de transparence](./TRANSPARENCY.md).

### SimpleX protège-t-il mon adresse IP ?

Pas encore complètement, c'est un travail en cours. Bien que votre appareil ne se connecte pas directement aux appareils de vos contacts, comme c'est le cas dans les réseaux p2p, vos contacts peuvent héberger eux-mêmes leurs relais, et vous vous connecterez à eux lorsque vous enverrez des messages. Un relais modifié peut enregistrer les adresses IP des appareils qui se connectent, comme c'est le cas pour tout autre serveur, y compris les nœuds d'entrée Tor, les fournisseurs de VPN, etc. - L'adresse IP est fondamentale pour le fonctionnement d'Internet, et il y aura toujours un serveur qui pourra observer votre adresse IP.

Nous travaillons actuellement sur la prochaine version du protocole de routage des messages qui protégera votre adresse IP des relais choisis par vos contacts, de sorte qu'elle ne sera visible que par les relais que vous aurez choisis. Pour en savoir plus sur les détails techniques, cliquez ici : [RFC](https://github.com/simplex-chat/simplexmq/blob/stable/rfcs/2023-09-12-second-relays.md). 

### Puis-je héberger mes propres relais ?

Bien sûr ! Veuillez consulter ces tutoriels : [Serveur SMP](./SERVER.md) et [Serveur XFTP](./XFTP-SERVER.md).

## Financement et modèle économique

### Comment êtes-vous financé ?

SimpleX Chat Ltd est financé par des investisseurs privés et du capital-risque. En tant que projet open-source, il est également généreusement soutenu par des dons. Lire [plus de détails](../../blog/20230422-simplex-chat-vision-funding-v5-videos-files-passcode.md#comment-est-ce-que-c'est-financé-et-que-est-ce-que-c'est-le-modèle-d'affaires).

### Pourquoi les sociétés de capital-risque ?

Voici quelques réflexions sur la nécessité d'un financement par capital-risque pour un projet de cette envergure, ainsi que sur la durabilité et la rentabilité des opérations à long terme : https://www.poberezkin.com/posts/2023-10-31-why-privacy-impossible-without-venture-funding.html

Et un autre point de vue d'un membre de l'équipe sur l'équilibre délicat entre les structures financées par le capital-risque et les structures à but non lucratif, ainsi que sur les projets d'évolution des protocoles du réseau SimpleX sous l'égide d'entités à but non lucratif dans diverses juridictions, afin que son évolution continue s'aligne plus étroitement sur la vision d'une gouvernance indépendante et transparente axée sur la communauté :
[https://simplex.chat/blog/20240404-why-i-joined-simplex-chat-esraa-al-shafei.html](../../blog/20240404-why-i-joined-simplex-chat-esraa-al-shafei.md).

### Quel sera le modèle d'entreprise ?

Nous nous concentrons sur l'adéquation entre le produit et le marché, et le modèle commercial est donc encore en cours d'élaboration. Cependant, l'application aura un modèle freemium avec des fonctions ou des capacités supplémentaires pour les utilisateurs payants (en prenant en considération une formule potentielle comme 5% payant 5$/mois est 3$/utilisateur/année - ~90% de marge bénéficiaire brute).

L'autre source de revenus proviendrait des services aux entreprises, pour les entités ayant besoin d'une assistance directe et personnalisée pour intégrer le protocole SimpleX ou des ressources connexes. Il y aura également un modèle de partage des revenus entre les clients et les opérateurs de réseau, afin de les inciter à continuer à faire fonctionner les nœuds, ce qui renforcera la décentralisation et la fiabilité du réseau.

Il n'est pas facile d'obtenir des modèles commerciaux non exploitants avec un code source entièrement ouvert, et nous nous engageons à trouver la meilleure solution possible pour notre contexte. Tout sera communiqué au fur et à mesure de l'avancement de ce plan.
