FR, [EN](/docs/GLOSSARY.md)

# Glossaire

Le choix d'une messagerie privée nécessite la compréhension de nombreux termes techniques, que beaucoup d'utilisateurs, même très techniques, ne comprennent pas toujours. Cette liste vise à combler ce manque de connaissances. N'hésitez pas à suggérer des changements ou des ajouts.

Bien que ce glossaire se veuille factuel et objectif, il n'est pas totalement impartial. Nous avons conçu SimpleX pour qu'il soit le réseau de communication le plus privé, le plus sûr et le plus résistant, et certaines définitions reflètent ce point de vue.

## Portabilité de l'adresse

De même que la [portabilité du numéro de téléphone](https://fr.wikipedia.org/wiki/Transf%C3%A9rabilit%C3%A9_du_num%C3%A9ro_de_t%C3%A9l%C3%A9phone) (la possibilité pour le client de transférer le service à un autre fournisseur sans changer de numéro), la portabilité de l'adresse signifie la possibilité pour le client d'un service de communication de changer de fournisseur de services sans changer d'adresse de service. De nombreux [réseaux fédérés](#federated-network) prennent en charge les enregistrements SRV pour assurer la portabilité des adresses, mais le fait de permettre aux utilisateurs de services de configurer leurs propres domaines pour les adresses n'est pas aussi couramment pris en charge par les logiciels serveur et client disponibles que pour le courrier électronique.

## Références anonymes

Le justificatif qui permet de prouver quelque chose, par exemple le droit d'accéder à une ressource, sans identifier l'utilisateur. Ce justificatif peut être généré par un tiers de confiance ou par l'utilisateur lui-même et fourni en même temps que la demande de création de la ressource. La première approche crée une dépendance centralisée dans la plupart des cas. La seconde approche ne nécessite aucune confiance - elle est utilisée dans le réseau SimpleX pour autoriser l'accès aux files d'attente de messagerie.

[Certificat numérique sur Wikipedia](https://en.wikipedia.org/wiki/Digital_credential)

## Blockchain

Au sens large, la blockchain désigne une séquence de blocs de données, où chaque bloc contient un hachage cryptographique du bloc précédent, assurant ainsi l'intégrité de l'ensemble de la chaîne. Les blockchains sont utilisées dans de nombreux systèmes de communication et de stockage d'informations pour assurer l'intégrité et l'immuabilité des données. Par exemple, les disques BluRay utilisent la blockchain. Les files d'attente de messagerie SimpleX utilisent également la blockchain - chaque message contient le hachage du message précédent, afin de garantir l'intégrité - si un message est modifié, le destinataire le détectera lors de la réception du message suivant. Les blockchains sont un sous-ensemble des [graphes acycliques dirigés de Merkle](#merkle-directed-acyclic-graph).

Dans un sens plus étroit, en particulier dans les médias, la chaîne de blocs est utilisée pour désigner spécifiquement un grand livre distribué, où chaque enregistrement comprend également le hachage de l'enregistrement précédent, mais où les blocs doivent être approuvés par les pairs participants à l'aide d'un [protocole de consensus](https://fr.wikipedia.org/wiki/Probl%C3%A8me_du_consensus).

[Wikipedia](https://fr.wikipedia.org/wiki/Blockchain)

## Récupération par effraction

[Sécurité post-compromission](#post-compromise-security).

## Réseau centralisé

Les réseaux centralisés sont fournis ou contrôlés par une seule entité. Les exemples sont Threema, Signal, WhatsApp et Telegram. L'avantage de cette conception est que le fournisseur peut innover plus rapidement et qu'il a une approche centralisée de la sécurité. Mais l'inconvénient est que le fournisseur peut modifier ou interrompre le service, et faire fuir, vendre ou divulguer d'une autre manière toutes les données des utilisateurs, y compris les personnes avec lesquelles ils sont connectés.

## Remplissage du contenu (Content padding)

[Message de remplissage](#message-padding).

## Réseau décentralisé

Le réseau décentralisé est souvent utilisé pour signifier "le réseau basé sur la blockchain décentralisée". Dans son sens premier, un réseau décentralisé signifie qu'il n'y a pas d'autorité centrale ou d'autre point de centralisation dans le réseau, à l'exception de la spécification des protocoles de réseau. L'avantage des réseaux décentralisés est qu'ils résistent à la censure et à la disparition du fournisseur. L'inconvénient est qu'ils sont souvent plus lents à innover et que la sécurité peut être moins bonne qu'avec un réseau centralisé.

Les exemples de réseaux décentralisés sont le courrier électronique, le web, le DNS, XMPP, Matrix, BitTorrent, etc. Tous ces exemples disposent d'un espace d'adressage global partagé au niveau de l'application. Les blockchains de crypto-monnaies ont non seulement un espace d'adressage partagé, mais aussi un état partagé, de sorte qu'elles sont plus centralisées que le courrier électronique. Le réseau Tor dispose également d'un espace d'adressage global partagé, mais aussi d'une autorité centrale. Le réseau SimpleX n'a pas d'espace d'adressage partagé au niveau de l'application (il s'appuie sur les adresses partagées au niveau du transport - les noms d'hôtes relais SMP ou les adresses IP), et il n'a pas d'autorité centrale ni d'état partagé.

## Défense en profondeur

À l'origine, il s'agit d'une stratégie militaire qui vise à retarder plutôt qu'à empêcher l'avancée d'un attaquant, en gagnant du temps et en causant des pertes supplémentaires en cédant de l'espace.

Dans le domaine de la sécurité de l'information, la défense en profondeur représente l'utilisation de plusieurs techniques de sécurité informatique afin d'atténuer le risque qu'un élément de la défense soit compromis ou contourné. Par exemple, un logiciel antivirus peut être installé sur des postes de travail individuels alors qu'il existe déjà une protection antivirus sur les pare-feux et les serveurs dans le même environnement.

Le réseau SimpleX applique l'approche de la défense en profondeur à la sécurité en disposant de plusieurs couches pour la sécurité des communications et la protection de la vie privée :
- algorithme à double ratchet pour le [chiffrement de bout en bout](#end-to-end-encryption) avec [perfect forward secrecy](#forward-secrecy) et [post-compromise security](#post-compromise-security),
- une couche supplémentaire de chiffrement de bout en bout pour chaque file d'attente de messagerie et une autre couche de chiffrement du serveur au destinataire à l'intérieur de TLS pour empêcher la corrélation par le texte chiffré,
- TLS avec uniquement des algorithmes de chiffrement puissants,
- atténuation de l'attaque [man-in-the-middle](#man-in-the-middle-attack) sur la connexion client-serveur via la vérification hors ligne du certificat du serveur,
- atténuation des attaques par rejeu via la signature sur le canal de transport,
- plusieurs couches de [message padding](#message-padding) pour réduire l'efficacité de l'analyse du trafic,
- atténuation de l'[attaque de l'homme du milieu](#man-in-the-middle-attack) sur le canal hors bande client-client lors de l'envoi de l'invitation,
- rotation des files d'attente pour réduire l'efficacité de l'analyse du trafic,
- etc.

[Wikipedia](https://fr.wikipedia.org/wiki/D%C3%A9fense_en_profondeur)

## Algorithme à double ratchet

Il est utilisé par deux parties pour échanger des messages [chiffrés de bout en bout](#chiffrement de bout en bout). Les parties utilisent un [protocole d'accord de clé](#key-agreement-protocol) pour se mettre d'accord sur la clé secrète partagée initiale.

L'algorithme double ratchet fournit [un secret parfait en aval](#forward-secrecy) et [une sécurité après compromis](#post-compromise-security). Il est [conçu par Signal](https://signal.org/docs/specifications/doubleratchet), et utilisé dans SimpleX Chat et de nombreux autres messagers sécurisés. La plupart des experts considèrent qu'il s'agit du protocole de chiffrement le plus avancé en matière de chiffrement des messages.

## Chiffrement de bout en bout

Système de communication dans lequel seules les parties communicantes peuvent lire les messages. Il est conçu pour protéger le contenu des messages de toute écoute potentielle - fournisseurs de télécommunications et d'Internet, acteurs malveillants, ainsi que le fournisseur du service de communication.

Le chiffrement de bout en bout nécessite un accord sur les clés cryptographiques entre l'expéditeur et le destinataire de manière à ce qu'aucune écoute ne puisse accéder aux clés convenues. Voir [protocole d'accord de clé](#key-agreement-protocol). Cet échange de clés peut être compromis par une [attaque de l'homme du milieu](#man-in-the-middle-attack), en particulier si l'échange de clés se fait via le même fournisseur de communication et qu'aucun canal hors bande n'est utilisé pour vérifier l'échange de clés.

[Wikipedia](https://fr.wikipedia.org/wiki/Chiffrement_de_bout_en_bout)

## Réseau fédéré

Le réseau fédéré est fourni par plusieurs entités qui s'accordent sur les normes et exploitent le réseau collectivement. Cela permet aux utilisateurs de choisir leur fournisseur, qui conservera leur compte, leur historique de messagerie et leurs contacts, et communiquera avec les serveurs d'autres fournisseurs au nom de l'utilisateur. Les exemples sont le courrier électronique, XMPP, Matrix et Mastodon.

L'avantage de cette conception est qu'il n'y a pas d'organisation unique dont tous les utilisateurs dépendent et que les normes sont plus difficiles à modifier, à moins que cela ne profite à tous les utilisateurs. Il y a plusieurs inconvénients : 1) l'innovation est plus lente, 2) chaque compte d'utilisateur dépend toujours d'une seule organisation et, dans la plupart des cas, ne peut pas passer à un autre fournisseur sans changer son adresse réseau - il n'y a pas de [portabilité d'adresse](#address-portability), 3) la sécurité et la confidentialité sont inévitablement moins bonnes qu'avec les réseaux centralisés.

[Fédération sur Wikipedia](https://fr.wikipedia.org/wiki/F%C3%A9d%C3%A9ration_(informatique))

## Secret d'acheminement

Également connu sous le nom de "perfect forward secrecy", il s'agit d'une caractéristique d'un [protocole d'accord de clé](#key-agreement-protocol) qui garantit que les clés de session ne seront pas compromises même si les secrets à long terme utilisés dans l'échange de clés de session sont compromis. Le secret de transmission protège les sessions passées contre les compromissions futures des clés de session ou des clés à long terme.

[Wikipedia](https://fr.wikipedia.org/wiki/Confidentialit%C3%A9_persistante)

## Protocole d'accord de clé

Également connu sous le nom d'échange de clés, il s'agit d'un processus d'accord sur les clés cryptographiques entre l'expéditeur et le·s destinataire·s du message. Il est nécessaire pour que le [chiffrement de bout en bout](#end-to-end-encryption) fonctionne.

[Wikipedia](https://en.wikipedia.org/wiki/Key-agreement_protocol)

## Échange de clés

[Protocole d'accord de clé](#key-agreement-protocol).

## Attaque de l'homme du milieu

Attaque au cours de laquelle l'attaquant relaie secrètement et éventuellement modifie les communications entre deux parties qui croient communiquer directement l'une avec l'autre.

Cette attaque peut être utilisée pour compromettre le [chiffrement de bout en bout](#end-to-end-encryption) en interceptant les clés publiques pendant l'[échange de clés](#key-agreement-protocol), en les remplaçant par les clés de l'attaquant, puis en interceptant et en chiffrant à nouveau tous les messages, sans en modifier le contenu. Avec cette attaque, l'attaquant ne modifie pas le contenu des messages, mais il peut les lire, alors que les parties communicantes croient que les messages sont chiffrés de bout en bout.

Cette attaque est possible avec tout système qui utilise le même canal pour l'échange de clés que pour l'envoi des messages - cela inclut presque tous les systèmes de communication à l'exception de SimpleX, où la clé publique initiale est toujours transmise hors bande. Même avec SimpleX, l'attaquant peut intercepter et substituer la clé envoyée par un autre canal, ce qui lui permet d'accéder à la communication. Ce risque est nettement plus faible, car l'attaquant ne sait pas à l'avance quel canal sera utilisé pour transmettre la clé.

Pour limiter ce type d'attaque, les parties qui communiquent doivent vérifier l'intégrité de l'échange de clés - SimpleX et de nombreuses autres applications de messagerie, telles que Signal et WhatsApp, disposent d'une fonction qui le permet.

[Wikipedia](https://fr.wikipedia.org/wiki/Attaque_de_l%27homme_du_milieu).

## Graphe acyclique dirigé de Merkle

Également connu sous le nom de DAG de Merkle, il s'agit d'une structure de données basée sur une structure de graphe générale où le nœud contient les hachages cryptographiques des nœuds précédents qui pointent vers lui. Les arbres de Merkle sont un sous-ensemble des DAG de Merkle - dans ce cas, chaque feuille contient un hachage cryptographique du parent.

Cette structure permet, de par sa conception, de vérifier l'intégrité de l'ensemble de la structure en calculant ses hachages et en les comparant aux hachages inclus dans les nœuds, de la même manière qu'avec [blockchain](#blockchain).

La motivation pour utiliser un DAG dans des environnements distribués au lieu d'une blockchain linéaire plus simple est de permettre des ajouts simultanés, lorsqu'il n'y a pas d'exigence pour un ordre unique des éléments ajoutés. Le DAG de Merkle est utilisé, par exemple, dans [IPFS](https://fr.wikipedia.org/wiki/InterPlanetary_File_System) et sera utilisé dans les groupes décentralisés SimpleX.

[Wikipedia](https://fr.wikipedia.org/wiki/Arbre_de_Merkle).

## Remplissage du message

Également connu sous le nom de "content padding", ce procédé consiste à ajouter des données au début ou à la fin d'un message avant de le chiffrer. Le remplissage dissimule la taille réelle du message aux oreilles indiscrètes. SimpleX comporte plusieurs couches de chiffrement et, avant chaque chiffrement, le contenu est complété à une taille fixe.

[Wikipedia](https://fr.wikipedia.org/wiki/Remplissage_(cryptographie)).

## Routage en oignon

Technique de communication anonyme sur un réseau informatique qui utilise plusieurs couches de chiffrement des messages, analogues aux couches d'un oignon. Les données chiffrées sont transmises via une série de nœuds de réseau appelés "routeurs oignons", dont chacun "pèle" une seule couche, révélant la prochaine destination des données. L'expéditeur reste anonyme car chaque intermédiaire ne connaît que l'emplacement des nœuds qui le précèdent et le suivent immédiatement.

Le réseau en oignon le plus utilisé est [Tor](https://fr.wikipedia.org/wiki/Tor_(r%C3%A9seau)).

Certains éléments du réseau SimpleX utilisent des idées similaires dans leur conception - différentes adresses pour la même ressource utilisée par différentes parties, et des couches de chiffrement supplémentaires. Actuellement, le protocole de messagerie SimpleX ne protège pas l'adresse du réseau de l'expéditeur, car le serveur de relais est choisi par le destinataire. Les relais de livraison choisis par l'expéditeur, qui sont prévus pour l'avenir, rapprocheraient la conception de SimpleX du routage en oignon.

[Wikipedia](https://en.wikipedia.org/wiki/Onion_routing)

## Réseau superposé

Les nœuds du réseau superposé peuvent être considérés comme étant connectés par des liens virtuels ou logiques, chacun d'entre eux correspondant à un chemin, qui peut passer par de nombreux liens physiques, dans le réseau sous-jacent. Tor, par exemple, est un réseau superposé au réseau IP qui, à son tour, est également un réseau superposé à un réseau physique sous-jacent.

Les clients SimpleX forment également un réseau utilisant des relais SMP et IP ou un autre réseau superposé (par exemple, Tor) pour communiquer entre eux. Les relais SMP, quant à eux, ne forment pas de réseau.

[Wikipedia](https://fr.wikipedia.org/wiki/R%C3%A9seau_superpos%C3%A9)

# Non-répudiation

Propriété du système cryptographique ou de communication qui permet au destinataire du message de prouver à tout tiers que l'expéditeur identifié par une clé cryptographique a envoyé le message. C'est le contraire de la [répudiation](#répudiation). Si, dans certains contextes, la non-répudiation peut être souhaitable (par exemple, pour les messages contractuels), dans le contexte des communications privées, elle peut être indésirable.

[Wikipedia](https://fr.wikipedia.org/wiki/Non-r%C3%A9pudiation)

## Identifiant pseudonyme par paire

Généralisant [la définition](https://csrc.nist.gov/glossary/term/pairwise_pseudonymous_identifier) des lignes directrices du NIST sur l'identité numérique, il s'agit d'un identifiant opaque et incontrôlable généré par un service utilisé pour accéder à une ressource par une seule partie.

Dans le contexte du réseau SimpleX, il s'agit des identifiants générés par les relais SMP pour accéder aux files d'attente de messagerie anonyme, avec un identifiant distinct (et un justificatif d'accès) pour chaque partie accédante : le destinataire, l'expéditeur et, en option, l'abonné aux notifications. La même approche est utilisée par les relais XFTP pour accéder aux morceaux de fichiers, avec des identifiants (et des références d'accès) distincts pour l'expéditeur et chaque destinataire.

## Peer-to-peer

Le pair à pair (P2P) est l'architecture de réseau dans laquelle les participants ont des droits égaux et communiquent directement par l'intermédiaire d'un réseau de transport ou d'un réseau superposé à usage général. Contrairement à l'architecture client-serveur, tous les pairs d'un réseau P2P fournissent et consomment les ressources. Dans le contexte de la messagerie, l'architecture P2P signifie généralement que les messages sont envoyés entre pairs, sans que les comptes d'utilisateur ou les messages soient stockés sur des serveurs. Tox, Briar, Cwtch et bien d'autres en sont des exemples.

L'avantage est que les participants ne dépendent d'aucun serveur. Cette architecture présente de [nombreux inconvénients](./SIMPLEX.md#comparison-with-p2p9-messaging-protocols), tels que l'absence de transmission asynchrone des messages, la nécessité de disposer d'adresses de pairs à l'échelle du réseau, la possibilité d'attaques à l'échelle du réseau, qui ne sont généralement atténués que par l'utilisation d'une autorité centralisée. Ces inconvénients sont évités avec l'architecture [P2P par procuration](#proxied-peer-to-peer).

[Wikipedia](https://fr.wikipedia.org/wiki/Pair-%C3%A0-pair).

## Secret de transmission parfait

[Forward secrecy](#forward-secrecy).

## Sécurité post-compromission

Également connue sous le nom de récupération après effraction, il s'agit de la qualité du système de chiffrement de bout en bout permettant de récupérer la sécurité contre un attaquant passif qui observe les messages chiffrés après avoir compromis l'une des parties (ou les deux). Également connu sous le nom de récupération après compromission ou récupération après effraction. [L'algorithme à double rochet](#double-ratchet-algorithme) a cette qualité.

## Cryptographie post-quantique

Tout système ou algorithme cryptographique proposé qui est censé être sécurisé contre une attaque par un ordinateur quantique. Il semble qu'en 2023, il n'y ait aucun système ou algorithme dont il soit prouvé qu'il est sûr contre de telles attaques, ou même qu'il est sûr contre les attaques d'ordinateurs conventionnels massivement parallèles, de sorte qu'une recommandation générale est d'utiliser des systèmes cryptographiques post-quantiques en combinaison avec les systèmes cryptographiques traditionnels.

[Wikipedia](https://fr.wikipedia.org/wiki/Cryptographie_post-quantique)

## Vie privée

Le droit d'une personne de garder (ou l'état lorsqu'elle garde) ses affaires personnelles et ses relations secrètes (par exemple, [Cambridge dictionary](https://dictionary.cambridge.org/dictionary/english/privacy)). La confidentialité des systèmes de communication devrait inclure la confidentialité des connexions et des métadonnées, et pas seulement la confidentialité du contenu des messages. Le [chiffrement de bout en bout](#end-to-end-encryption) ne garantit pas à lui seul la confidentialité, car il ne protège que le contenu des messages et non les connexions ou les métadonnées.

[Wikipedia](https://fr.wikipedia.org/wiki/Vie_priv%C3%A9e)

## Peer-to-peer par procuration

Topologie de réseau du système de communication où les pairs communiquent par l'intermédiaire de mandataires qui ne forment pas le réseau eux-mêmes. Ce type de conception est utilisé dans Pond, qui dispose d'un serveur domestique fixe pour chaque utilisateur, et dans SimpleX, qui utilise de multiples relais fournissant des connexions temporaires.

## Récupération en cas de compromis

(#post-compromise-security).

## Répudiation

Propriété du système cryptographique ou de communication qui permet à l'expéditeur du message de nier de manière plausible avoir envoyé le message, car si le destinataire peut vérifier que le message a été envoyé par l'expéditeur, il ne peut pas le prouver à un tiers - le destinataire a la capacité technique de falsifier le même message chiffré. Il s'agit d'une qualité importante des communications privées, car elle permet d'avoir une conversation qui peut être démentie par la suite, de la même manière qu'une conversation privée en face à face.

Voir aussi [non-répudiation](#non-repudiation).

## Identité de l'utilisateur

Dans un système de communication, il s'agit de tout ce qui permet d'identifier de manière unique les utilisateurs du réseau. Selon le réseau de communication, il peut s'agir d'un numéro de téléphone, d'une adresse électronique, d'un nom d'utilisateur, d'une clé publique ou d'un identifiant opaque aléatoire. La plupart des réseaux de messagerie s'appuient sur une certaine forme d'identité de l'utilisateur. SimpleX semble être le seul réseau de messagerie qui ne repose sur aucune forme d'identité d'utilisateur - voir [cette comparaison](https://en.wikipedia.org/wiki/Comparison_of_instant_messaging_protocols).
