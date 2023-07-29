# Glossaire

Le choix d'une messagerie privée nécessite la compréhension de nombreux termes techniques, que beaucoup d'utilisateurs, même très techniques, ne comprennent pas toujours. Cette liste vise à combler ce manque de connaissances. N'hésitez pas à suggérer des changements ou des ajouts.

Bien que ce glossaire se veuille factuel et objectif, il n'est pas totalement impartial. Nous avons conçu SimpleX comme le réseau de communication le plus privé, le plus sûr et le plus résistant, et certaines définitions reflètent ce point de vue.

**Remarque** : Les pages wikipédia en anglais sont souvent plus complètes que celles en francais. N'hésitez pas à les consulter !

## Portabilité de l'adresse

#### _Address portability_ 🇬🇧

À la manière de la [portabilité du numéro de téléphone](https://fr.wikipedia.org/wiki/Transf%C3%A9rabilit%C3%A9_du_num%C3%A9ro_de_t%C3%A9l%C3%A9phone) (la possibilité pour le client de transférer le service à un autre fournisseur sans changer de numéro), la portabilité de l'adresse signifie la possibilité pour le client d'un service de communication de changer de fournisseur de services sans changer d'adresse de service. De nombreux [réseaux fédérés](#réseau-fédéré) prennent en charge les enregistrements SRV pour assurer la portabilité des adresses, mais le fait de permettre aux utilisateurs de services de configurer leurs propres domaines pour les adresses n'est pas aussi couramment pris en charge par les logiciels serveur et client disponibles que pour le courrier électronique.

## Justificatifs d'identité anonymes

#### _Anonymous credentials_ 🇬🇧

Un justificatif qui permet de prouver quelque chose, par exemple le droit d'accéder à une ressource, sans identifier l'utilisateur. Ce justificatif peut être généré par un tiers de confiance ou par l'utilisateur lui-même et fourni avec la demande de création de la ressource. La première approche crée une dépendance centralisée dans la plupart des cas. La seconde approche ne nécessite aucune confiance - elle est utilisée dans le réseau SimpleX pour autoriser l'accès aux files d'attente de messages.

[Certificat numérique sur Wikipédia](https://en.wikipedia.org/wiki/Digital_credential)

## Chaîne de blocs

#### _Blockchain_ 🇬🇧

Au sens large, la blockchain désigne une séquence de blocs de données, où chaque bloc contient un hachage cryptographique du bloc précédent, assurant ainsi l'intégrité de l'ensemble de la chaîne. Les blockchains sont utilisées dans de nombreux systèmes de communication et de stockage d'informations pour assurer l'intégrité et l'immuabilité des données. Par exemple, les disques BluRay utilisent la blockchain. Les files d'attente de messagerie SimpleX utilisent également la blockchain - chaque message contient le hachage du message précédent, afin de garantir l'intégrité - si un message est modifié, le destinataire le détectera lors de la réception du message suivant. Les blockchains sont un sous-ensemble des [graphes acycliques dirigés de Merkle](#graphe-acyclique-dirigé-de-merkle).

Dans un sens plus précis, en particulier dans les médias, la blockchain est utilisée pour désigner spécifiquement un registre distribué, où chaque enregistrement comprend également le hachage de l'enregistrement précédent, mais où les blocs doivent être approuvés par les pairs participants à l'aide d'un [protocole de consensus](https://fr.wikipedia.org/wiki/Probl%C3%A8me_du_consensus).

[Wikipédia](https://fr.wikipedia.org/wiki/Blockchain)

## Récupération après effraction 

#### _Break-in recovery_ 🇬🇧

[Sécurité post-compromission](#sécurité-post-compromission).

## Réseau centralisé

#### _Centralized network_ 🇬🇧

Les réseaux centralisés sont fournis ou contrôlés par une seule entité. Les exemples sont Threema, Signal, WhatsApp et Telegram. L'avantage de cette conception est que le fournisseur peut innover plus rapidement et qu'il dispose d'une approche centralisée de la sécurité. Mais l'inconvénient est que le fournisseur peut modifier ou interrompre le service, et faire fuir, vendre ou divulguer d'une autre manière toutes les données des utilisateurs, y compris les personnes avec lesquelles ils sont connectés.

## Remplissage du contenu

#### _Content padding_ 🇬🇧

[Rembourrage des messages](#rembourrage-des-messages).

## Réseau décentralisé

#### _Decentralized network_ 🇬🇧

Le réseau décentralisé est souvent utilisé pour signifier "le réseau basé sur une blockchain décentralisée". Dans son sens premier, un réseau décentralisé signifie qu'il n'y a pas d'autorité centrale ou d'autre point de centralisation dans le réseau, à l'exception de la spécification des protocoles de réseau. L'avantage des réseaux décentralisés est qu'ils résistent à la censure et à la disparition du fournisseur. L'inconvénient est qu'ils sont souvent plus lents à innover et que la sécurité peut être moins bonne qu'avec un réseau centralisé.

Les exemples de réseaux décentralisés sont le courrier électronique, le web, les DNS, XMPP, Matrix, BitTorrent, etc. Tous ces exemples disposent d'un espace d'adresse global partagé au niveau de l'application. Les blockchains de crypto-monnaies ont non seulement un espace d'adressage partagé, mais aussi un état partagé, et sont donc plus centralisées que le courrier électronique. Le réseau Tor dispose également d'un espace d'adressage global partagé, mais aussi d'une autorité centrale. Le réseau SimpleX n'a pas d'espace d'adressage partagé au niveau de l'application (il s'appuie sur les adresses partagées au niveau du transport - les noms d'hôtes ou les adresses IP des relais SMP), et il n'a pas d'autorité centrale ni d'état partagé.

## Défense en profondeur 

#### _Defense in depth_ 🇬🇧

À l'origine, il s'agit d'une stratégie militaire qui vise à retarder plutôt qu'à empêcher l'avancée d'un attaquant, en gagnant du temps et en causant des pertes supplémentaires en cédant de l'espace.

Dans le domaine de la sécurité informatique, la défense en profondeur représente l'utilisation de plusieurs techniques de sécurité informatique afin d'atténuer le risque qu'un élément de la défense soit compromis ou contourné. Par exemple, un logiciel antivirus peut être installé sur des postes de travail individuels alors qu'il existe déjà une protection antivirus sur les pare-feu et les serveurs dans le même environnement.

Le réseau SimpleX applique une approche de défense en profondeur de la sécurité en ayant plusieurs couches pour la sécurité et la confidentialité des communications :
- algorithme à double ratchet pour un [chiffrement de bout en bout](#chiffrement-de-bout-en-bout) avec une [confidentialité persistante](#confidentialité-persistante) et une [sécurité post-compromission](#sécurité-post-compromission),
- une couche supplémentaire de chiffrement de bout en bout pour chaque file d'attente de messagerie et une autre couche de chiffrement du serveur au destinataire à l'intérieur de TLS pour empêcher la corrélation par le texte chiffré,
- TLS avec uniquement des algorithmes de chiffrement forts,
- limitation de l'attaque [l'homme du milieu](#attaque-de-lhomme-du-milieu) sur la connexion client-serveur via la vérification indépendante du certificat du serveur,
- limitation des attaques par rejeu grâce à la signature sur le canal de transport,
- plusieurs couches de [rembourrage de messages](#rembourrage-des-messages) pour réduire l'efficacité de l'analyse du trafic,
- limitation de l'attaque de [l'homme du milieu](#attaque-de-lhomme-du-milieu) sur le réseau hors bande client-client lors de l'envoi de l'invitation,
- rotation des files d'attente de livraison pour réduire l'efficacité de l'analyse du trafic,
- etc.

[Wikipédia](https://fr.wikipedia.org/wiki/D%C3%A9fense_en_profondeur)

## Algorithme à double ratchet

#### _Double ratchet algorithm_ 🇬🇧

Il est utilisé par deux entités pour échanger des messages [chiffrés de bout en bout](#chiffrement-de-bout-en-bout). Les entités utiliseront un [protocole d'accord de clé](#protocole-daccord-de-clé) pour se mettre d'accord sur la clé secrète initiale partagée.

L'algorithme Double Ratchet fournit une [confidentialité persistante](#confidentialité-persistante) et une [sécurité post-compromission](#sécurité-post-compromission). Il est [conçu par Signal](https://signal.org/docs/specifications/doubleratchet), et utilisé dans SimpleX Chat et de nombreux autres messagers sécurisés. La plupart des experts considèrent qu'il s'agit du protocole de chiffrement le plus avancé en matière de chiffrement des messages.

## Chiffrement de bout en bout

#### _End-to-end encryption_ 🇬🇧

Un système de communication dans lequel seules les parties communicantes peuvent lire les messages. Il est conçu pour protéger le contenu des messages de toute écoute potentielle - fournisseurs de télécommunications et d'Internet, acteurs malveillants, ainsi que le fournisseur du service de communication.

Le chiffrement de bout en bout nécessite un accord sur les clés cryptographiques entre l'expéditeur et le destinataire de manière à ce qu'aucune écoute ne puisse accéder aux clés convenues. Voir [protocole d'accord de clé](#protocole-daccord-de-clé). Cet échange de clés peut être compromis par une attaque de [l'homme du milieu](#attaque-de-lhomme-du-milieu), en particulier si l'échange de clés se fait par l'intermédiaire du même fournisseur de communication et qu'aucun canal hors bande n'est utilisé pour vérifier l'échange de clés.

[Wikipédia](https://fr.wikipedia.org/wiki/Chiffrement_de_bout_en_bout)

## Réseau fédéré

#### _Federated network_ 🇬🇧

Le réseau fédéré est fourni par plusieurs entités qui s'accordent sur les normes et exploitent le réseau collectivement. Cela permet aux utilisateurs de choisir leur fournisseur, qui conservera leur compte, leur historique de messagerie et leurs contacts, et communiquera avec les serveurs d'autres fournisseurs au nom de l'utilisateur. Les exemples sont le courrier électronique, XMPP, Matrix et Mastodon.

L'avantage de cette approche est qu'il n'y a pas d'organisation unique dont tous les utilisateurs dépendent, et qu'il est plus difficile de modifier les normes, sauf si cela profite à tous les utilisateurs. Il y a plusieurs inconvénients : 1) l'innovation est plus lente, 2) chaque compte d'utilisateur dépend toujours d'une seule organisation et, dans la plupart des cas, ne peut pas passer à un autre fournisseur sans changer son adresse réseau - il n'y a pas de [portabilité d'adresse](#portabilité-de-ladresse), 3) la sécurité et la confidentialité sont inévitablement moins bonnes qu'avec les réseaux centralisés.

[Fédération sur Wikipédia](https://fr.wikipedia.org/wiki/F%C3%A9d%C3%A9ration_(informatique))

## Confidentialité persistante 

#### _Forward secrecy_ 🇬🇧

Également connu sous le nom de "perfect forward secrecy" ou confidentialité persistante parfaite, il s'agit d'une caractéristique d'un [protocole d'accord de clé](#protocole-daccord-de-clé) qui garantit que les clés de session ne seront pas compromises même si les secrets à long terme utilisés dans l'échange de clés de session sont compromis. Le secret de transmission protège les sessions passées contre les compromissions futures des clés de session ou des clés à long terme.

[Wikipédia](https://fr.wikipedia.org/wiki/Confidentialit%C3%A9_persistante)

## Protocole d'accord de clé

#### _Key agreement protocol_ 🇬🇧

Également connu sous le nom d'échange de clés, il s'agit d'un processus qui consiste à établir des clés cryptographiques entre l'expéditeur et le(s) destinataire(s) du message. Il est nécessaire pour que le [chiffrement de bout en bout](#chiffrement-de-bout-en-bout) fonctionne.

[Wikipédia](https://en.wikipedia.org/wiki/Key-agreement_protocol)

## Échange de clés

#### _Key exchange_ 🇬🇧

[Protocole d'accord de clé](#protocole-daccord-de-clé).

## Attaque de l'homme du milieu

#### _Man-in-the-middle attack_ 🇬🇧

Il s'agit d'une attaque au cours de laquelle l'attaquant relaie secrètement et éventuellement modifie les communications entre deux entités qui croient communiquer directement l'une avec l'autre.

Cette attaque peut être utilisée pour compromettre le [chiffrement de bout en bout](#chiffrement-de-bout-en-bout) en interceptant les clés publiques pendant [l'échange de clés](#protocole-daccord-de-clé), en les remplaçant par les clés de l'attaquant, puis en interceptant et en chiffrant à nouveau tous les messages, sans en modifier le contenu. Avec cette attaque, l'attaquant ne modifie pas le contenu des messages, mais il peut les lire, alors que les parties communicantes croient que les messages sont chiffrés de bout en bout.

Cette attaque est possible avec tout système qui utilise le même canal pour l'échange de clés que pour l'envoi des messages - cela inclut presque tous les systèmes de communication à l'exception de SimpleX, où la clé publique initiale est toujours transmise hors bande. Même avec SimpleX, l'attaquant peut intercepter et substituer la clé envoyée par un autre canal, ce qui lui permet d'accéder à la communication. Ce risque est nettement plus faible, car l'attaquant ne sait pas à l'avance quel canal sera utilisé pour transmettre la clé.

Pour limiter ce type d'attaque, les entités qui communiquent doivent vérifier l'intégrité de l'échange de clés - SimpleX et de nombreuses autres applications de messagerie, telles que Signal et WhatsApp, disposent d'une fonction qui le permet.

[Wikipédia](https://fr.wikipedia.org/wiki/Attaque_de_l%27homme_du_milieu).

## Graphe acyclique dirigé de Merkle

#### _Merkle directed acyclic graph_ 🇬🇧

Également connu sous le nom de "Merkle DAG", il s'agit d'une structure de données basée sur une structure graphique générale dans laquelle le nœud contient les hachages cryptographiques des nœuds précédents qui pointent vers lui. Les arbres de Merkle sont un sous-ensemble des DAG de Merkle - dans ce cas, chaque feuille contient un hachage cryptographique du parent.

Cette structure permet de vérifier l'intégrité de l'ensemble de la structure en calculant ses hachages et en les comparant aux hachages inclus dans les nœuds, de la même manière qu'avec une [blockchain](#chaîne-de-blocs).

La motivation pour utiliser le DAG dans des environnements distribués au lieu d'une blockchain linéaire plus simple est de permettre des ajouts simultanés, lorsqu'il n'y a pas d'exigence pour un ordre unique des éléments ajoutés. Le DAG Merkle est utilisé, par exemple, dans [IPFS](https://fr.wikipedia.org/wiki/InterPlanetary_File_System) et sera utilisé dans les groupes décentralisés SimpleX.

[Wikipédia](https://fr.wikipedia.org/wiki/Arbre_de_Merkle).

## Rembourrage des messages

#### _Message padding_ 🇬🇧

Également connu sous le nom de "remplissage de contenu", il s'agit d'ajouter des données au début ou à la fin d'un message avant de le chiffrer. Le remplissage dissimule la taille réelle du message aux oreilles indiscrètes. SimpleX comporte plusieurs couches de chiffrement et, avant chaque chiffrement, le contenu est ramené à une taille fixe.

[Wikipédia](https://fr.wikipedia.org/wiki/Remplissage_(cryptographie)).

## Routage en oignon 

#### _Onion routing_ 🇬🇧

Technique de communication anonyme sur un réseau informatique qui utilise plusieurs couches de chiffrement des messages, analogues aux couches d'un oignon. Les données chiffrées sont transmises par l'intermédiaire d'une série de nœuds de réseau appelés "routeurs oignons", dont chacun " épluche " une seule couche, révélant ainsi la prochaine destination des données. L'expéditeur reste anonyme car chaque intermédiaire ne connaît que l'emplacement des nœuds qui le précèdent et le suivent immédiatement.

Le réseau en oignon le plus utilisé est [Tor](https://fr.wikipedia.org/wiki/Tor_(r%C3%A9seau)).

Certains éléments du réseau SimpleX utilisent des idées similaires dans leur conception - différentes adresses pour la même ressource utilisée par différentes parties, et des couches de chiffrement supplémentaires. Actuellement, le protocole de messagerie SimpleX ne protège pas l'adresse du réseau de l'expéditeur, car le serveur de relais est choisi par le destinataire. Les relais de livraison choisis par l'expéditeur, qui sont prévus pour l'avenir, rapprocheraient la conception de SimpleX du routage en oignon.

[Wikipédia](https://en.wikipedia.org/wiki/Onion_routing)

## Réseau superposé

#### _Overlay network_ 🇬🇧

Les nœuds du réseau superposé peuvent être considérés comme étant connectés par des liens virtuels ou logiques, chacun d'entre eux correspondant à un chemin, qui peut passer par de nombreux liens physiques, dans le réseau sous-jacent. Tor, par exemple, est un réseau superposé au réseau IP qui, à son tour, est également un réseau superposé à un réseau physique sous-jacent.

Les clients SimpleX forment également un réseau utilisant des relais SMP et IP ou un autre réseau superposé (par exemple, Tor) pour communiquer entre eux. Les relais SMP, quant à eux, ne forment pas de réseau.

[Wikipédia](https://fr.wikipedia.org/wiki/R%C3%A9seau_superpos%C3%A9)

## Identifiant pseudonyme par paire

#### _Pairwise pseudonymous identifier_ 🇬🇧

En généralisant [la définition](https://csrc.nist.gov/glossary/term/pairwise_pseudonymous_identifier) des lignes directrices du NIST sur l'identité numérique, il s'agit d'un identifiant opaque et incontrôlable généré par un service utilisé pour permettre à une seule entité d'accéder à une ressource.

Dans le contexte du réseau SimpleX, il s'agit des identifiants générés par les relais SMP pour accéder aux files d'attente de messagerie anonyme, avec un identifiant distinct (et un justificatif d'accès) pour chaque partie accédante : le destinataire, l'expéditeur et, en option, l'abonné aux notifications. La même approche est utilisée par les relais XFTP pour accéder aux morceaux de fichiers, avec des identifiants (et des justificatifs d'accès) distincts pour l'expéditeur et chaque destinataire.

## Pair-à-pair

#### _Peer-to-peer_ 🇬🇧

Le pair-à-pair (P2P) est une architecture de réseau dans laquelle les participants ont des droits égaux et communiquent directement par l'intermédiaire d'un réseau de transport ou d'un réseau superposé à usage général. Contrairement à l'architecture client-serveur, tous les pairs d'un réseau P2P fournissent et consomment les ressources. Dans le contexte de la messagerie, l'architecture P2P signifie généralement que les messages sont envoyés entre pairs, sans que les comptes d'utilisateur ou les messages soient stockés sur des serveurs. Tox, Briar, Cwtch et bien d'autres en sont des exemples.

L'avantage est que les participants ne dépendent d'aucun serveur. Cette architecture présente de [nombreux inconvénients](./SIMPLEX.md#comparison-with-p2p9-messaging-protocols), tels que l'absence de transmission asynchrone des messages, la nécessité de disposer d'adresses de pairs à l'échelle du réseau, la possibilité d'attaques à l'échelle du réseau, qui ne sont généralement atténués que par l'utilisation d'une autorité centralisée. Ces inconvénients sont évités avec l'architecture [P2P par proxy](#pair-à-pair-par-proxy).

[Wikipédia](https://fr.wikipedia.org/wiki/Pair-%C3%A0-pair).

## Confidentialité persistante parfaite

#### _Perfect forward secrecy_ 🇬🇧

[Confidentialité persistante](#confidentialité-persistante).

## Sécurité post-compromission

#### _Post-compromise security_ 🇬🇧

Également connu sous le nom de récupération après effraction, il s'agit de la capacité du système de chiffrement de bout en bout à rétablir la sécurité contre un attaquant passif qui observe les messages chiffrés après avoir compromis l'une des entités (ou les deux). Également connu sous le nom de récupération après compromission ou récupération après intrusion. [L'algorithme à double ratchet](#algorithme-à-double-ratchet) possède cette qualité.

## Cryptographie post-quantique

#### _Post-quantum cryptography_ 🇬🇧

Tout système ou algorithme cryptographique proposé qui est considéré comme sûr contre une attaque par un ordinateur quantique. Il semble qu'en 2023, il n'y ait aucun système ou algorithme dont il soit prouvé qu'il est sûr contre de telles attaques, ou même qu'il est sûr contre les attaques d'ordinateurs conventionnels massivement parallèles, de sorte qu'une recommandation générale est d'utiliser des systèmes cryptographiques post-quantiques en combinaison avec les systèmes cryptographiques traditionnels.

[Wikipédia](https://fr.wikipedia.org/wiki/Cryptographie_post-quantique)

## Vie privée

#### _Privacy_ 🇬🇧

Le droit de quelqu'un de garder (ou l'état lorsqu'il garde) ses affaires personnelles et ses relations secrètes (par exemple, [dictionnaire de Cambridge](https://dictionary.cambridge.org/dictionary/english/privacy)). La confidentialité des systèmes de communication devrait inclure la confidentialité des connexions et des métadonnées, et pas seulement la confidentialité du contenu des messages. Le [chiffrement de bout en bout](#chiffrement-de-bout-en-bout) n'assure pas à lui seul la protection de la vie privée, car il ne protège que le contenu des messages et non les connexions ou les métadonnées.

[Wikipedia](https://en.wikipedia.org/wiki/Privacy)

## Pair-à-pair par proxy

#### _Proxied peer-to-peer_ 🇬🇧

Topologie de réseau du système de communication lorsque les pairs communiquent par l'intermédiaire de mandataires (proxy) qui ne forment pas eux-mêmes le réseau. Cette conception est utilisée dans Pond, qui dispose d'un serveur domestique fixe pour chaque utilisateur, et dans SimpleX, qui utilise de multiples relais fournissant des connexions temporaires.

## Rétablissement suite à un compromis

#### _Recovery from compromise_ 🇬🇧

[Sécurité post-compromission](#sécurité-post-compromission).

## Identité de l'utilisateur

#### _User identity_ 🇬🇧

Dans un système de communication, il s'agit de tout ce qui permet d'identifier de manière unique les utilisateurs du réseau. Selon le réseau de communication, il peut s'agir d'un numéro de téléphone, d'une adresse électronique, d'un nom d'utilisateur, d'une clé publique ou d'un identifiant opaque aléatoire. La plupart des réseaux de messagerie s'appuient sur une certaine forme d'identité de l'utilisateur. SimpleX semble être le seul réseau de messagerie qui ne repose sur aucune forme d'identité d'utilisateur - voir [cette comparaison](https://en.wikipedia.org/wiki/Comparison_of_instant_messaging_protocols).
