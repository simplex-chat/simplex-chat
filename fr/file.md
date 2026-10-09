# Transfert de fichiers SimpleX

Glissez-déposez un fichier ici

ou

Choisir un fichier

Max 100 Mo - [l’app SimpleX Chat](https://simplex.chat/downloads) prend en charge les fichiers jusqu’à 1 Go

Envoyez des fichiers en toute sécurité avec un chiffrement de bout en bout — sans compte, sans suivi.

## Obtenez SimpleX Chat — la messagerie la plus sécurisée et la plus respectueuse de la vie privée

Le transfert de fichiers que vous venez d’utiliser repose sur le même protocole de routage des données que SimpleX Chat. L’application propose une messagerie chiffrée de bout en bout, des appels audio et vidéo, des groupes et l’envoi de fichiers. Aucun compte. Aucun numéro de téléphone. Aucun e-mail. Aucun identifiant de profil utilisateur.

# Protocole XFTP : le transfert de fichiers le plus sécurisé

### Aucun compte requis

Chaque fragment de fichier utilise une nouvelle clé aléatoire. Les routeurs de données n’ont ni « utilisateurs » ni « fichiers » — ils transfèrent des fragments de fichiers chiffrés de taille fixe.

### Chiffré trois fois dans votre navigateur

La clé de chiffrement du fichier est présente uniquement dans le fragment de hachage de l’URL — votre navigateur ne l’envoie jamais à un serveur. Il existe trois couches de chiffrement : le transport TLS, le chiffrement par destinataire (clé éphémère unique par transfert) et le chiffrement de bout en bout des fichiers.

### Routeurs de données indépendants

Lorsqu’un fichier est découpé en fragments, il est transmis via des routeurs réseau gérés par des tiers indépendants. Aucun opérateur ne peut voir la taille réelle ou le nom du fichier. Même si un routeur est compromis, il ne peut voir que des fragments chiffrés de taille fixe. Les fragments de fichier sont mis en cache par les routeurs réseau pendant environ 48 heures.

[Lire la spécification du protocole XFTP →](https://github.com/simplex-chat/simplexmq/blob/stable/protocol/xftp.md)
