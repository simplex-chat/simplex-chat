---
title: Plateforme SimpleX
revision: 07.02.2023
---
| 07.02.2023 | FR, [EN](/docs/SIMPLEX.md), [CZ](/docs/lang/cs/SIMPLEX.md), [PL](/docs/lang/pl/SIMPLEX.md) |

# Plateforme SimpleX - motivation et comparaison

## Problèmes

Les plateformes et protocoles de chat existants présentent une partie ou la totalité des problèmes suivants :

- Absence de confidentialité du profil et des contacts de l'utilisateur (confidentialité des métadonnées).
- Aucune protection (ou seulement une protection optionnelle) des implémentations [E2EE][1] contre les attaques MITM via le fournisseur.
- Messages non sollicités (spam et abus).
- Absence de propriété et de protection des données.
- Complexité d'utilisation de tous les protocoles non centralisés pour les utilisateurs non techniques.

La concentration de la communication dans un petit nombre de plateformes centralisées rend la résolution de ces problèmes assez difficile.

## Solutions

Les différents protocoles proposés résolvent ces problèmes en faisant en sorte que les messages comme les contacts soit stockés exclusivement sur l'appareil client, tout en réduisant le rôle des serveurs à de simples relais de message qui ne nécessite que l'autorisation des messages envoyés dans les files d'attentes, mais qui NE nécessite PAS une authentification utilisateur - les message ne sont pas les seuls à être protégé, les métadonnées le sont aussi car l'utilisateur n'est pas lié à un identifiant - contrairement aux autres platformes. 

Voir [le livre blanc de SimpleX](https://github.com/simplex-chat/simplexmq/blob/master/protocol/overview-tjr.md) pour plus d'information sur les objectifs de la platforme et ses détails techniques.

## Pourquoi utiliser SimpleX

## SimpleX une approche unique en terme de confidentialité et de sécurité

Tout le monde devrait se soucier de la confidentialité et de la sécurité de ses communications - même une conversation lambda peut vous mettre en danger.

### Protection complète de votre identité, profil, contacts et métadonnées

**Contrairement aux autres platformes de messagerie, SimpleX ne lie pas d'identifiant à l'utilisateur** - il n'y a pas besoin de numéro de télèphone (comme Signal ou WhatsApp), d'adresses basés sur des noms de domaines (comme les e-mails, XMPP ou Matrix), de noms d'utilisateurs (comme Telegram), ou de clés publiques ou même de nombres aléatoires pour identifié les utilisateurs - on ne sait même pas combiens de personnes utilise SimpleX.

Pour distribuer les messages au lieu d'utiliser des identifiants d'utilisateur que toute les autres platformes utilisent, SimpleX utilise des adresses unidirectionnelles (simplex) de files d'attentes de messages. Utiliser SimpleX c'est comme avoir une adresse e-mail différentes ou un numéro de télèphone différent par contact, sans les inconvenients qui viennent avec. Dans un futur proche les apps SimpleX changeront automatiquement les files d'attentes, déplacant les conversations d'un serveur à un autre, pour une meilleure confidentialité.

Cette approche protège la vie privé de la personne avec qui vous communiquez, elle est masquée des serveurs SimpleX et de n'importe quel autre observateur. Vous pouvez encore améliorer votre protection en configurant votre réseau pour qu'il passe via des réseaux superposé comme, par exemple Tor.

### La meilleure protection contre le spam et les abus

Comme vous n'avez pas d'identifiant sur la plateforme SimpleX, vous ne pouvez pas être contacté, sauf si vous partagez un lien d'invitation unique ou une adresse d'utilisateur temporaire facultative. Même avec les adresses d'utilisateur facultatives, bien qu'elles puissent être utilisées pour envoyer des demandes de contact non sollicitées, vous pouvez les modifier ou les supprimer complètement sans perdre aucune de vos connexions.

### Propriété, contrôle et sécurité totale de vos données

SimpleX stocke toutes les données de l'utilisateur sur les appareils clients, les messages ne sont conservés que temporairement sur les serveurs relais SimpleX jusqu'à leur réception.

Nous utilisons un format de base de données portable qui peut être utilisé sur tous les appareils pris en charge. Nous ajouterons bientôt la possibilité d'exporter la base de données de chat depuis l'application mobile afin qu'elle puisse être utilisée sur un autre appareil.

Contrairement aux serveurs des réseaux fédérés (e-mail, XMPP ou Matrix), les serveurs SimpleX ne stockent pas les comptes des utilisateurs, ils se contentent de relayer les messages aux destinataires, protégeant ainsi la vie privée des deux parties. Il n'y a aucun identifiant ou message chiffré en commun entre le trafic envoyé et reçu du serveur, grâce à la couche de chiffrement supplémentaire pour les messages délivrés. Par conséquent, si quelqu'un observe le trafic du serveur, il ne peut pas facilement déterminer qui communique avec qui (Voir [le livre blanc de SimpleX](https://github.com/simplex-chat/simplexmq/blob/master/protocol/overview-tjr.md) pour les attaques connues de corrélation réseau).

### Les utilisateurs sont maîtres du réseau SimpleX

Vous pouvez utiliser SimpleX avec vos propres serveurs et continuer à communiquer avec les personnes utilisant les serveurs préconfigurés dans les applications ou tout autre serveur SimpleX.

La plateforme SimpleX utilise un protocole ouvert et fournit un SDK pour créer des chatbot, permettant la mise en œuvre de services avec lesquels les utilisateurs peuvent interagir via les applications SimpleX Chat - nous sommes vraiment impatients de voir quels services SimpleX peuvent être créés.

Si vous envisagez de développer avec la plateforme SimpleX, que ce soit pour des services de chatbot pour les utilisateurs de l'application SimpleX ou pour intégrer la bibliothèque de chat SimpleX dans vos applications mobiles, n'hésitez pas à nous contacter pour tout conseil et assistance.

## Comparaison avec d'autres protocoles

|                                                |    SimpleX chat    | Signal et autres...   |  XMPP, Matrix   | Protocoles P2P  |
| :--------------------------------------------- | :----------------: | :-------------------: | :-------------: | :-------------: |
| Identifiants d'utilisateur nécessaire          |    Non = privé     |    Oui<sup>1</sup>    | Oui<sup>2</sup> | Oui<sup>3</sup> |
| Risque d'attaque MITM                          |    Non = securisé  |    Oui<sup>4</sup>    |       Oui       |       Oui       |
| Dépendance au DNS                              |   Non = résistant  |          Oui          |       Oui       |       Non       |
| Un opérateur ou un réseau unique               | Non = décentralisé |          Oui          |       Non       | Oui<sup>5</sup> |
| Attaque à l'échelle du réseau                  |   Non = résistant  |          Oui          | Oui<sup>2</sup> | Oui<sup>6</sup> |

1. Généralement basé sur un numéro de téléphone, dans certains cas sur des noms d'utilisateur.
2. Basé sur le DNS.
3. Clé publique ou tout autre identifiant global unique.
4. Si les serveurs de l'opérateur sont compromis.
5. Si les réseaux P2P et les réseaux basés sur les crypto-monnaies sont distribués, ils ne sont pas décentralisés : ils fonctionnent comme un seul réseau, avec un seul espace de noms des adresses des utilisateurs.
6. Les réseaux P2P ont soit une autorité centrale, soit l'ensemble du réseau peut être compromis - voir la section suivante.

## Comparaison avec les protocoles de messagerie [P2P][9]

Il existe plusieurs protocoles et implémentations de chat/messagerie P2P qui visent à résoudre le problème de la protection de la vie privée et de la centralisation, mais ils ont leur propre série de problèmes qui les rendent moins fiables que la forme proposée, plus complexes à mettre en œuvre et à analyser et plus vulnérables aux attaques.

1. Les réseaux [P2P][9] utilisent une variante de [DHT][10] pour acheminer les messages/demandes à travers le réseau. Les implémentations du DHT ont des designs complexes qui doivent équilibrer la fiabilité, la garantie de livraison et la latence. La méthode proposée offre à la fois de meilleures garanties de livraison et une latence plus faible (le message est transmis plusieurs fois en parallèle, à travers un nœud à chaque fois, en utilisant des serveurs choisis par le destinataire, alors que dans les réseaux P2P, le message est transmis à travers `O(log N)` nœuds séquentiellement, en utilisant des nœuds choisis par un algorithme).

2. Le modèle proposé, contrairement à la plupart des réseaux P2P, ne comporte aucun identifiant global d'utilisateur, même temporaire.

3. Le P2P en lui-même ne résout pas le problème des [attaques MITM][2], et la plupart des solutions existantes n'utilisent pas de messages hors bande pour l'échange initial de clés. La conception proposée utilise des messages hors bande ou, dans certains cas, des connexions sécurisées et fiables préexistantes pour l'échange initial de clés.

4. Les implémentations P2P peuvent être bloquées par certains fournisseurs d'accès à Internet (comme [BitTorrent][11]). Le modèle proposée est indépendant des moyens de transport : il peut fonctionner avec des protocoles web standard et les serveurs peuvent être déployés sur les mêmes domaines que les sites web.

5. Tous les réseaux P2P connus sont susceptibles d'être vulnérables à une [attaque Sybil][12], car chaque nœud peut être découvert et le réseau fonctionne comme un tout. Les mesures connues pour réduire la probabilité de l'attaque Sybil nécessitent soit un composant centralisé, soit des [preuves de travail][13] coûteuses. Le modèle proposée, au contraire, ne permet pas de découvrir les serveurs - les serveurs ne sont pas connectés, ni connus les uns des autres, ni de tous les clients. Le réseau SimpleX est fragmenté et fonctionne comme de multiples connexions isolées. Cela rend impossible les attaques à l'échelle du réseau SimpleX - même si certains serveurs sont compromis, d'autres parties du réseau peuvent fonctionner normalement, et les clients affectés peuvent passer à l'utilisation d'autres serveurs sans perdre de contacts ou de messages.

6. Les réseaux P2P sont susceptibles d'être [vulnérables][14] aux [attaques DRDoS][15]. Dans la conception proposée, les clients ne relaient que le trafic provenant de connexions de confiance connues et ne peuvent pas être utilisés pour refléter et amplifier le trafic dans l'ensemble du réseau.

[1]: https://fr.wikipedia.org/wiki/Chiffrement_de_bout_en_bout
[2]: https://fr.wikipedia.org/wiki/Attaque_de_l%27homme_du_milieu
[9]: https://fr.wikipedia.org/wiki/Pair-%C3%A0-pair
[10]: https://fr.wikipedia.org/wiki/Table_de_hachage_distribu%C3%A9e
[11]: https://fr.wikipedia.org/wiki/BitTorrent
[12]: https://fr.wikipedia.org/wiki/Attaque_Sybil
[13]: https://fr.wikipedia.org/wiki/Preuve_de_travail
[14]: https://www.usenix.org/conference/woot15/workshop-program/presentation/p2p-file-sharing-hell-exploiting-bittorrent
[15]: https://en.wikipedia.org/wiki/Denial-of-service_attack#Reflected_attack
