---
title: Service d'annuaire SimpleX
revision: 18.08.2023
---

| 18.08.2023 | FR, [EN](/docs/DIRECTORY.md)

# Service d'annuaire SimpleX

Vous pouvez utiliser un service d'annuaire expérimental pour découvrir les groupes créés et enregistrés par d'autres utilisateurs.

## Recherche de groupes

Se connecter au service d'annuaire via [cette adresse](https://simplex.chat/contact#/?v=1-4&smp=smp%3A%2F%2Fu2dS9sG8nMNURyZwqASV4yROM28Er0luVTx5X1CsMrU%3D%40smp4.simplex.im%2FeXSPwqTkKyDO3px4fLf1wx3MvPdjdLW3%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEAaiv6MkMH44L2TcYrt_CsX3ZvM11WgbMEUn0hkIKTOho%253D%26srv%3Do5vmywmrnaxalvz6wi3zicyftgio6psuvyniis6gco6bp6ekl4cqj4id.onion) et envoyez le message contenant les mots que vous souhaitez trouver dans le nom du groupe ou dans le message de bienvenue. Vous recevrez jusqu'à 10 groupes avec le plus grand nombre de membres dans la réponse, ainsi que les liens pour rejoindre ces groupes.

Veuillez noter que vos requêtes de recherche peuvent être conservées par le robot dans l'historique de la conversation, mais vous pouvez utiliser le mode incognito lorsque vous vous connectez au robot, afin d'éviter toute corrélation avec d'autres communications. Voir [Politique de confidentialité](../PRIVACY.md) pour plus de détails.

## Ajouter des groupes à l'annuaire


### Comment ajouter un groupe

Pour ajouter un groupe, vous devez en être le propriétaire. Une fois que vous vous êtes connecté au service d'annuaire et que vous avez envoyé `/help`, le service vous guidera tout au long de la procédure.

1. Invitez SimpleX Service Directory au groupe en tant que membre `admin`. Vous pouvez également définir le rôle à `admin` après avoir invité le service d'annuaire.

Le service d'annuaire doit être `admin` pour fournir une bonne expérience utilisateur pour rejoindre le groupe, car il créera un nouveau lien pour rejoindre le groupe, qui est censé être en ligne 99% du temps.

2. Ajoutez le lien qui vous a été envoyé par le service d'annuaire au message de bienvenue du groupe. Cette opération doit être effectuée par le membre du groupe qui a invité le service d'annuaire à rejoindre le groupe. Ce membre sera le propriétaire de l'enregistrement du groupe dans le service d'annuaire.

3. Une fois le lien ajouté, le groupe doit être approuvé par les administrateurs du service d'annuaire. Ce lien est fonctionnel avant même que le groupe ne soit approuvé, et vous pouvez continuer à l'utiliser même si le groupe n'est pas approuvé.

Le groupe est généralement approuvé dans les 24 heures. Veuillez voir ci-dessous quels groupes peuvent être ajoutés.

Une fois le groupe approuvé, il apparaîtra dans les résultats de recherche.

Vous pouvez dresser la liste de tous les groupes que vous avez soumis en envoyant `/list` au service d'annuaire.

### Comment supprimer le groupe de l'annuaire

Toute modification du profil du groupe (par exemple, modification du nom du groupe, du message de bienvenue ou suppression du lien permettant de rejoindre le groupe dans le message de bienvenue) fera disparaître le groupe des résultats de recherche jusqu'à ce qu'il soit à nouveau approuvé par les administrateurs des services d'annuaire.

S'il n'est pas souhaitable que le service ne soit pas trouvé dans les résultats de recherche pendant cette période, veuillez coordonner la date de ce changement avec les administrateurs du service d'annuaire afin d'obtenir une approbation rapide.

Changer le rôle du service d'annuaire supprimera temporairement le groupe des résultats de recherche et, à moins que vous n'ayez changé le rôle en "propriétaire", cela perturbera également de façon permanente les membres qui étaient en train de se connecter à d'autres membres par l'intermédiaire du service d'annuaire.

Pour supprimer le groupe de l'annuaire :

1. Supprimez le lien vers le groupe créé par le service d'annuaire dans le message de bienvenue. Cela n'empêchera pas les membres de rejoindre le groupe, même via ce lien, mais supprimera le groupe des résultats de recherche.
2. Après un certain temps (nous recommandons 3-4 jours), supprimez le service d'annuaire du groupe - il cessera de recevoir les messages et le groupe sera définitivement supprimé des résultats de recherche.

La suppression du groupe ne vous empêche pas de l'enregistrer à nouveau à l'avenir.

### Pourquoi limiter les groupes qui peuvent être ajoutés ?

La raison de restreindre le contenu acceptable est d'offrir une meilleure expérience à un plus grand nombre d'utilisateurs et de se conformer aux politiques de contenu des canaux de distribution des applications (App Store, Play Store, etc.), une fois que le service d'annuaire est disponible par l'intermédiaire de l'application sans configuration supplémentaire. Pour ce faire, le contenu des groupes répertoriés doit être limité pour être généralement approprié.

Cela ne va-t-il pas à l'encontre de l'idée de décentralisation et de liberté d'expression ?

Nous pensons que non, car

1. Le service restreint uniquement le contenu des groupes que vous choisissez d'enregistrer - nous ne disposons pas d'un registre de tous les groupes existants et n'avons pas accès à leur contenu.
2. Le service lui-même est open-source, et peut être auto-hébergé, de sorte que n'importe qui peut faire fonctionner un service d'annuaire alternatif avec les différentes politiques de contenu, ou sans aucune politique du tout.
3. La liberté d'expression doit respecter les droits légaux et les libertés d'autrui, et il semble donc nécessaire de convenir de certaines limites.

### Quels groupes peuvent être ajoutés

Le texte ci-dessous n'est pas la politique finale, il s'agit d'un travail en cours.

Actuellement, l'enregistrement des groupes est limité et manuel, car nous disposons de ressources limitées pour évaluer le contenu des groupes. La politique initiale en matière de contenu est donc assez restrictive - nous pensons qu'il est préférable de pouvoir étendre ce qui est autorisé plutôt que de devoir le réduire.

Pour figurer dans l'annuaire<sup>\*</sup>, le groupe doit compter au moins 10 membres. Les profils du groupe et de son propriétaire doivent inclure des images d'avatar pertinentes, appropriées et non offensantes, qui n'utilisent pas les marques déposées existantes.

Veuillez soumettre UNIQUEMENT les groupes sur les sujets suivants :
- solutions et fournisseurs de communications (messageries, réseaux sociaux, Internet, etc.)
- vie privée et sécurité
- crypto-monnaies
- développement de produits et de logiciels
- science et technologie
- médias et divertissements : livres, musique, films et jeux
- politique, société, culture et éducation

Le contenu du groupe doit être "approprié" pour le grand public, à partir de 12 ans.

Le contenu des groupes énumérés doit
- être légal pour la juridiction dans laquelle vous vous trouvez.
- NE PAS contenir de spam et de publicité.
- NE PAS contenir de violence, d'appels à la violence, d'appels à des manifestations publiques ou tout autre contenu dérangeant.
- NE PAS contenir de pornographie, de nudité, de contenu érotique ou à caractère sexuel.
- NE PAS contenir de racisme, d'incitation à la haine ou tout autre contenu encourageant la discrimination.
- NE PAS contenir d'informations sur les drogues, l'alcool ou toute autre substance.
- NE PAS contenir de contenu [NSFW] (https://en.wikipedia.org/wiki/Not_safe_for_work).
- NE PAS être offensant - ce point doit être clarifié.

Les propriétaires de groupes sont censés modérer le contenu des groupes. Si les membres publient un contenu inapproprié ou excessif et que les propriétaires de groupes ne le modèrent pas, le groupe risque d'être supprimé de l'annuaire.

Nous nous réservons le droit de refuser l'inscription d'un groupe dans l'annuaire ou d'annuler son inscription, et il peut arriver que nous ne puissions pas fournir d'explication. Nous essaierons certainement d'éviter cela en communiquant d'abord avec les propriétaires du groupe.

La combinaison du nom d'affichage et du nom complet doit être unique pour les groupes répertoriés.

Une fois le groupe répertorié dans l'annuaire, le robot vous invitera à rejoindre le groupe des propriétaires du groupe, où vous pourrez envoyer des idées ou des suggestions sur l'évolution de la fonctionnalité des groupes et contribuer à l'orientation du produit et des politiques.

<sup>\*</sup> "répertorié" signifie découvrable par le biais d'une recherche ou de toute autre fonction de service d'annuaire par tout utilisateur connecté autre que l'utilisateur qui a soumis l'enregistrement.
