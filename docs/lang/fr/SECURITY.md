---
title: Security Policy
permalink: /security/index.html
revision: 23.04.2024
---

| 23.04.2024 | FR, [EN](/docs/SECURITY.md)

# Politique de sécurité

Bien qu'un grand soin soit apporté pour assurer le plus haut niveau de sécurité et de confidentialité dans les serveurs et clients du réseau SimpleX, tous les logiciels peuvent avoir des failles, et nous pensons que c'est une partie essentielle de la responsabilité sociale d'une organisation de minimiser l'impact de ces failles par des efforts continus de découverte de vulnérabilités, la conception d'une défense en profondeur, et une remédiation et une notification rapides.

L'évaluation de la sécurité de la cryptographie et du réseau SimpleX a été réalisée par Trail of Bits en [novembre 2022] (https://simplex.chat/blog/20221108-simplex-chat-v4.2-security-audit-new-website.html).

Nous prévoyons une révision de la conception des protocoles SimpleX en juillet 2024 et une révision de la mise en œuvre en décembre 2024/janvier 2025.

## Signaler des problèmes de sécurité

Pour signaler un problème de sécurité, veuillez nous contacter directement par courriel [chat@simplex.chat](mailto:chat@simplex.chat). Veuillez NE PAS signaler de problèmes de sécurité via GitHub issues ou via tout autre canal public.

Veuillez chiffrer le message en utilisant la clé pour cette adresse depuis [keys.openpgp.org](https://keys.openpgp.org/search?q=chat%40simplex.chat) (son empreinte est `FB44 AF81 A45B DE32 7319 797C 8510 7E35 7D4A 17FC`) et mettez votre clé à disposition pour une réponse sécurisée.

Bien que nous vous encouragions à chiffrer le message, si cela constitue un obstacle à la déclaration, une soumission non chiffrée est préférable à l'absence de soumission.

## Tri des problèmes

Notre équipe examinera et classera par ordre de priorité le problème signalé. Nous pouvons travailler en privé avec des personnes qui ne font pas partie de notre équipe directe, ainsi qu'avec d'autres organisations, si nous pensons que cela peut contribuer à l'investigation, à la résolution ou au test du problème.

## Modèle de menace

Veuillez consulter le modèle de menace pour SimpleX : https://github.com/simplex-chat/simplexmq/blob/stable/protocol/overview-tjr.md#threat-model

Certaines menaces sont considérées comme hors de portée du modèle de sécurité de SimpleX. Par exemple, nous ne considérons pas SimpleX comme sûr contre les classes d'attaques suivantes :

- Défauts de l'unité centrale/du matériel.
- canaux secondaires d'observation physique (par exemple, consommation d'énergie, émissions électromagnétiques, etc.)

En outre, toutes les données utilisateur stockées sur l'appareil peuvent être consultées avec les privilèges de l'utilisateur ou de l'administrateur, comme par exemple
- les fichiers de l'utilisateur dans le stockage de l'application (chiffrées ou non, en fonction des paramètres de l'application).
- la base de données chiffrée.
- la clé de chiffrement de la base de données dans le cas où elle est stockée sur l'appareil.

Des mesures d'atténuation pour les problèmes de sécurité en dehors de notre modèle de menace peuvent toujours être mises en œuvre, mais elles seront évaluées en fonction de priorités concurrentes, et nous ne les classons pas comme des vulnérabilités SimpleX.

## Gravité du problème

Nous déterminerons le risque de chaque problème, en tenant compte de notre expérience des problèmes antérieurs, des versions concernées, des valeurs par défaut courantes et des cas d'utilisation. Nous classons les problèmes selon deux dimensions, en fonction de leur niveau de gravité et de la difficulté requise pour les exploiter.

**Niveaux de difficulté

- Faible** : La faille est bien connue ; il existe des outils publics pour l'exploiter ou il est possible de créer un script.
- Moyen** : Un attaquant doit écrire un programme d'exploitation ou aura besoin d'une connaissance approfondie du système.
- Élevé** : Un attaquant doit avoir un accès privilégié au système, peut avoir besoin de connaître des détails techniques complexes, ou doit découvrir d'autres faiblesses pour exploiter ce problème.

**Niveaux de gravité du problème**

- **Gravité CRITIQUE**. Ces problèmes doivent affecter des configurations courantes et être exploitables avec une difficulté faible ou moyenne. Par exemple : divulgation importante des messages ou des fichiers chiffrés des utilisateurs via des relais ou des canaux de communication, vulnérabilités pouvant être facilement exploitées à distance pour compromettre les clés privées des clients ou des serveurs. Ces problèmes resteront confidentiels et donneront lieu à la publication d'une nouvelle version de toutes les versions prises en charge.
- **Gravité ÉLEVÉE**. Il s'agit de problèmes dont le risque est inférieur à celui d'un problème critique, peut-être parce qu'ils affectent des configurations moins courantes, ou parce qu'ils sont très difficiles à exploiter. Ces problèmes resteront confidentiels et donneront lieu à la publication d'une nouvelle version de toutes les versions prises en charge.
- **Gravité MOYENNE**. Il s'agit de problèmes tels que les plantages d'applications clientes provoqués par les messages ou les fichiers reçus, les failles dans des protocoles moins couramment utilisés et les failles locales. Ces problèmes resteront généralement confidentiels jusqu'à la prochaine version, qui sera programmée de manière à pouvoir corriger plusieurs de ces failles en une seule fois.
- **FAIBLE gravité**. Cela inclut des problèmes tels que ceux qui n'affectent que l'application CLI de SimpleX, ou des configurations improbables, ou des problèmes qui seraient classés comme moyens mais qui sont très difficiles à exploiter. Ils seront en général corrigés immédiatement dans les dernières versions de développement, et pourront être rétroportés sur les anciennes versions qui reçoivent encore des mises à jour. Ces problèmes peuvent rester privés ou être inclus dans les messages de livraison.

## Politique de notification

Les correctifs de sécurité de gravité critique, élevée et moyenne NE DOIVENT PAS être mentionnés dans le message de livraison. Les correctifs de sécurité de faible gravité PEUVENT être mentionnés dans les messages de validation.

Nous informerons en privé nos partenaires de confiance des correctifs de sécurité à venir le jour où le correctif est rendu public et disponible au téléchargement via tous les canaux pris en charge, en indiquant le niveau du problème, mais pas d'autres détails.

7 jours après la publication de la nouvelle version du logiciel et sa mise à disposition pour téléchargement via tous les canaux pris en charge, nous indiquerons qu'elle corrige un problème de sécurité et son niveau, mais pas d'autres détails. Cette notification sera publiée dans nos notes de version et sur les canaux de diffusion que nous utilisons.

14 jours plus tard, les détails seront publiés dans les notes de publication, décrivant l'impact et la nature de la vulnérabilité, mais ne fournissant pas nécessairement des instructions détaillées pour l'exploitation - cela sera décidé au cas par cas.

## Partenaires de confiance

Vous pouvez être informé en privé des prochaines versions contenant des correctifs pour les problèmes de sécurité de gravité critique, élevée et moyenne. Nous ne communiquerons que le niveau de gravité du problème, et non le problème lui-même.

Pour figurer sur cette liste, vous devez être l'une des personnes suivantes :
- un fournisseur de logiciel ou de matériel qui dépend de notre code.
- une organisation commerciale ou à but non lucratif qui utilise notre logiciel dans des scénarios où la sécurité et la protection de la vie privée sont d'une importance cruciale.
- nous pouvons également inclure d'autres organisations qui ne figurent pas sur la liste mais qui rempliraient les conditions requises pour en faire partie.
- nous pouvons également inclure des organisations avec lesquelles nous entretenons des relations commerciales.
- nous pouvons cesser de notifier certaines organisations si elles divulguent des problèmes avant qu'ils ne soient rendus publics ou si elles n'apportent pas de valeur ajoutée.
