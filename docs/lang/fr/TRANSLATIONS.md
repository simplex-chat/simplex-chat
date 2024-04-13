---
title: Contribuer aux traductions de SimpleX Chat
revision: 19.03.2023
---
| 19.03.2023 | FR, [EN](/docs/TRANSLATIONS.md), [CZ](/docs/lang/cs/TRANSLATIONS.md), [PL](/docs/lang/pl/TRANSLATIONS.md) |

# Contribuer aux traductions de SimpleX Chat

Un grand merci pour votre intérêt pour la traduction de SimpleX Chat - cela nous aide beaucoup à rendre l'application accessible à un plus grand nombre d'utilisateurs, et nous vous remercions de votre aide.

Cela demande un investissement en temps considérable - la plupart des gens le sous-estiment au départ - et une maintenance continue au fur et à mesure que nous développons l'application.

Ce document est créé pour accélérer ce processus, et partager quelques astuces importantes que nous avons découverts en travaillant avec Weblate - la plateforme que nous utilisons pour les traductions d'interface.

## Avant de commencer la traduction

1. Créez un compte sur Weblate, en utilisant le même e-mail que celui que vous utilisez sur GitHub - cela permettra d'associer vos contributions à votre compte GitHub, et pourra vous aider dans certains cas. Une fois la traduction publiée pour les utilisateurs, nous ajouterons le nom de votre compte à la [liste des traducteurs](https://github.com/simplex-chat/simplex-chat#translate-the-apps).

2. Avant de commencer la traduction, il y a un simple accord de licence pour les contributeurs à signer via Weblate - ceci afin d'éviter tout conflit autour des droits de propriété intellectuelle. La copie de cet accord est également [disponible ici](https://github.com/simplex-chat/cla/blob/master/CLA.md).

3. Nous pouvons également vous ajouter au groupe de traducteurs pour toute question et mise à jour - veuillez vous connecter à moi via le chat.

## Avancement de la traduction

1. Veuillez commencer par [l'app Android](https://hosted.weblate.org/projects/simplex-chat/android/), à la fois lorsque vous effectuez la traduction initiale la plus longue, et que vous ajoutez les chaînes de caractères plus tard. Premièrement, les chaînes iOS peuvent être un peu retardées dans leur apparition dans Weblate, car elles nécessitent une étape manuelle de notre part avant d'être visibles. Deuxièmement, l'application Android est configurée comme un glossaire pour l'application iOS, et 2/3 de toutes les chaînes ne nécessitent que quelques clics pour les transférer d'Android à iOS (cela prend toujours un certain temps, Weblate ne l'automatise pas, malheureusement).

2. Certaines des chaînes n'ont pas besoin d'être traduites, mais elles doivent quand même être copiées - il y a un bouton dans l'interface weblate pour cela :

<img src="/images/weblate_1.png" alt="weblate: copy source to translation" width="100%">

3. Weblate propose également des suggestions automatiques qui peuvent accélérer le processus. Parfois, elles peuvent être utilisées telles quelles, parfois elles nécessitent quelques retouches - cliquez pour les utiliser dans les traductions.

4. Une fois que toutes les chaînes de caractères de l'application Android sont traduites, veuillez les réviser pour vous assurer de la cohérence du style et de la langue, afin que les mêmes mots soient systématiquement utilisés pour des actions similaires de l'utilisateur, comme en anglais. Parfois, vous devrez utiliser des mots différents dans des cas où l'anglais n'en a qu'un seul. Veuillez essayer d'utiliser ces choix de manière cohérente dans des contextes similaires, afin de faciliter la tâche des utilisateurs finaux.

5. Quand vous traduisez [l'app iOS](https://hosted.weblate.org/projects/simplex-chat/ios/), la plupart des chaînes de caractères sont identiques, elles peuvent être copiées en un clic dans la section Glossaire. L'indice visuel que cela est possible est que la chaîne source entière est surlignée en jaune. De nombreuses autres chaînes sont très similaires, elles ne diffèrent que par la syntaxe d'interpolation ou la façon dont la police en gras est utilisée - elles ne nécessitent qu'une édition minimale. Certaines chaînes sont propres à la plate-forme iOS. Elles doivent être traduites séparément.

<img src="/images/weblate_2.png" alt="weblate: automatic suggestions" width="100%">

## Une fois la traduction terminée

Une fois que les applications Android et iOS sont traduites, veuillez nous en informer.

Nous allons ensuite :
  - revoir toutes les traductions et suggérer des corrections - cela prend aussi un peu de temps :)
  - les fusionner avec le code source - pendant que nous le ferons, weblate sera verrouillé pour les changements.
  - créer des versions bêta des applications iOS et Android - nous pouvons également vous ajouter aux groupes de testeurs internes, afin que vous puissiez installer les applications avant tout le monde.
  - diffuser l'application auprès de nos utilisateurs bêta - ce sont plus d'un millier de personnes qui utilisent nos versions bêta.
  - publier l'application et inclure la nouvelle langue dans l'annonce.

*Remarque* : nous souhaitons que les fonctions de l'application restent cohérentes entre les plateformes Android et iOS, dans la mesure du possible. Nous publierons et annoncerons donc une nouvelle langue une fois que les deux plateformes auront été traduites. Cela ne signifie pas que vous devez le faire, mais nous devrons attendre que quelqu'un d'autre traduise la deuxième plateforme. Mais si vous commencez par Android, l'ajout d'iOS prend généralement 3 à 4 fois moins de temps.

## La suite

1. Lorsque nous mettons l'application à jour, nous publions les mises à jour dans le groupe de traducteurs. Vous n'avez absolument aucune obligation de traduire ces chaînes supplémentaires. Nous apprécions énormément que vous le fassiez, car l'expérience des utilisateurs est bien meilleure, ils dépendent de vos traductions, si une nouvelle partie de l'application n'est pas traduite.

2. Vous pouvez également aider à promouvoir l'application dans votre pays / groupe linguistique en traduisant nos documents - nous venons de commencer - ainsi que le contenu de notre site web. Il y a eu beaucoup de demandes pour le faire et nous sommes en train d'ajouter le cadre de traduction pour le site web.

3. De plus, si vous souhaitez être modérateur/administrateur du groupe d'utilisateurs dans votre langue, une fois l'application traduite, nous pourrons héberger un tel groupe. Nous sommes en train de préparer des règles de conduite pour la communauté et d'ajouter des outils de modération à l'application qui sortira dans la v5 en mars.


Encore une fois un grand merci de nous aider à développer SimpleX Chat !

Evgeny, fondateur de SimpleX Chat.
