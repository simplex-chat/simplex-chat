---
title: Vie privée et sécurité
---
# Vie privée et sécurité

La configuration par défaut de SimpleX Chat vise à équilibrer la confidentialité, la sécurité et la praticité. Il se peut que vous souhaitiez modifier les options par défaut.

Cette page répertorie toutes les fonctionnalités et options qui affectent la confidentialité et la sécurité.

## Paramètres de confidentialité et de sécurité

Ces paramètres sont disponibles dans les [Paramètres de confidentialité et de sécurité] (./app-settings.md#privacy-and-security).

## Vérification par code de sécurité

<img src="../../blog/images/20230103-verification.png" width="288">

Bien que SimpleX Chat établisse toujours une connexion via un lien transmis par un canal indépendant, et qu'il soit donc déjà plus protégé que d'autres applications, il existe des scénarios dans lesquels les liens d'invitation peuvent être remplacés en cours de route (attaque MITM). Pour vous protéger contre de telles attaques, vous devez vérifier le code de sécurité avec vos contacts :

- ouvrez la conversation avec le contact
- appuyez sur le nom du contact en haut de la conversation
- appuyez sur "Vérifier le code de sécurité"
- demandez à votre contact de faire de même
- la connexion est sécurisée si vous et votre contact avez le même code de sécurité.

Ce code peut être validé de l'une des manières suivantes :

- l'un de vous peut scanner le code de sécurité à partir de son appareil ; si les codes correspondent, le contact sera marqué comme vérifié sur l'appareil qui a scanné le code.
- appuyez simplement sur marquer vérifié si vous faites confiance à la confirmation de votre contact que le code est vérifié.
- Vous pouvez également le lire lors d'un appel vocal.

En savoir plus dans [cet article](../../blog/20230103-simplex-chat-v4.4-messages-disparus.md#connexion-sécurité-vérification)

## Phrase secrète de la base de données

Une fois installée, l'application génère une phrase secrète aléatoire pour la base de données du chat et la stocke de manière sécurisée dans KeyChain (iOS) ou en utilisant KeyStore (Android, le module TPM est utilisé lorsqu'il est disponible). Vous pouvez définir votre propre phrase secrète et la supprimer de l'appareil, auquel cas vous devrez la saisir à chaque démarrage de l'application, et les notifications peuvent être limitées, dans les paramètres [phrase secrète et exportation de la base de données](./managing-data.md#database-passphrase).

## Mode Incognito

Cette fonction génère un nom de profil aléatoire pour chaque nouveau contact. En savoir plus dans [mode Incognito](./app-settings.md#incognito).

## Profils cachés

Cette fonction permet de cacher certains profils de chat avec un mot de passe. Pour en savoir plus, lisez [Cacher et désactiver des profils de chat](./chat-profiles.md#hiding-and-muting-chat-profiles).

## Paramètres réseau

[Isolement du transport (BETA)](./app-settings.md#transport-isolation-beta) permet d'isoler votre trafic avec chaque contact dans une connexion TCP différente (et un circuit Tor).

## Utilisation de Tor

<img src="../../blog/images/20220808-tor1.png" width="330"> &nbsp ; <img src="../../blog/images/20220808-tor2.png" width="330">

Pour se connecter aux relais SMP (serveurs de messagerie) via Tor, vous devez installer l'application Orbot.

Android : utilisez l'application Orbot comme proxy SOCKS sur le port 9050 (par défaut) et activez [Use SOCKS proxy](./app-settings.md#use-socks-proxy-android-only).

iOS : utiliser l'application Orbot comme fournisseur de VPN et activer le VPN.

Vous pouvez également modifier les adresses de serveur utilisées avec l'option [Use .onion hosts](./app-settings.md#use-onion-hosts).
