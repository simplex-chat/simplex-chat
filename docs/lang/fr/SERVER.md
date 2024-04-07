---
title: Héberger votre propre serveur SMP
revision: 05.06.2023
---
| 05.06.2023 | FR, [EN](/docs/SERVER.md), [CZ](/docs/lang/cs/SERVER.md), [PL](/docs/lang/pl/CLI.md) |

# Héberger votre propre serveur SMP

## Présentation générale

Un serveur SMP est un serveur relais utilisé pour transmettre les messages sur le réseau SimpleX. Les apps SimpleX Chat ont des serveurs prédéfinis (pour les apps mobiles, smp11, smp12 et smp14.simplex.im), mais vous pouvez facilement modifier la configuration de l'app pour utiliser d'autres serveurs.

Seuls les utilisateurs de SimpleX déterminent quel serveur est utilisé pour recevoir les messages, séparément pour chaque contact (ou pour chaque connexion à un membre d'un groupe), et ces serveurs ne sont que temporaires, car l'adresse de réception peut changer.

_Veuillez noter_ : lorsque vous modifiez les serveurs dans la configuration de l'app, cela n'affecte que les serveurs qui seront utilisés pour les nouveaux contacts. Les contacts existants ne seront pas automatiquement déplacés vers les nouveaux serveurs, mais vous pouvez les déplacer manuellement à l'aide du bouton ["Changer d'adresse de réception"](/blog/20221108-simplex-chat-v4.2-security-audit-new-website.md#change-your-delivery-address-beta) sur les fiches d'information de contact/membre – ce sera bientôt automatisé.

## Installation

0. Tout d'abord, installez `smp-server`:

   - Déploiement manuel :

     - [Compiler à partir de la source](https://github.com/simplex-chat/simplexmq#using-your-distribution)
     - [Utiliser une version pré-compilée](https://github.com/simplex-chat/simplexmq#install-binaries)

   - Sinon, vous pouvez déployer votre `smp-server` en utilisant :
     - [Docker container](https://github.com/simplex-chat/simplexmq#using-docker-1)
     - [Linode StackScript](https://github.com/simplex-chat/simplexmq#deploy-smp-server-on-linode)

L'installation manuelle nécessite quelques actions préalables :

1. Créer un utilisateur et un groupe pour votre `smp-server`:

   ```sh
   sudo useradd -m smp
   ```

2. Créer les répertoires nécessaires et y attribuer les permissions :

   ```sh
   sudo mkdir -p /var/opt/simplex /etc/opt/simplex
   sudo chown smp:smp /var/opt/simplex /etc/opt/simplex
   ```

3. Autoriser le port du `smp-server` dans le pare-feu:

   ```sh
   # Pour Ubuntu
   sudo ufw allow 5223/tcp
   # Pour Fedora
   sudo firewall-cmd --permanent --add-port=5223/tcp && \
   sudo firewall-cmd --reload
   ```

4. **Optionnel** - Si vous utilisez une distribution avec `systemd`, créez le fichier `/etc/systemd/system/smp-server.service` avec le contenu suivant :

   ```sh
   [Unit]
   Description=Serveur SMP
   [Service]
   User=smp
   Group=smp
   Type=simple
   ExecStart=smp-server start
   ExecStopPost=/usr/bin/env sh -c '[ -e "/var/opt/simplex/smp-server-store.log" ] && cp "/var/opt/simplex/smp-server-store.log" "/var/opt/simplex/smp-server-store.log.bak"'
   KillSignal=SIGINT
   TimeoutStopSec=infinity
   Restart=always
   RestartSec=10
   LimitNOFILE=65535
   [Install]
   WantedBy=multi-user.target
   ```

   Et exécutez `sudo systemctl daemon-reload`.

## Configuration

Pour voir les options disponibles, exécutez `smp-server` sans paramètres :

```sh
sudo su smp -c smp-server

...
Available commands:
  init                     Initialize server - creates /etc/opt/simplex and
                           /var/opt/simplex directories and configuration files
  start                    Start server (configuration:
                           /etc/opt/simplex/smp-server.ini)
  delete                   Delete configuration and log files
```

Vous pouvez obtenir de l'aide supplémentaire en exécutant `sudo su smp -c "smp-server <command> -h"`

Ensuite, nous devons configurer `smp-server`:

### Interactif

Exécutez la commande suivante :

```sh
sudo su smp -c "smp-server init"
```

Il y a plusieurs options disponibles :

- `Enable store log to restore queues and messages on server restart (Yn):`

  Entrez `y` pour permettre la sauvegarde et la restauration des connexions et des messages lorsque le serveur est redémarré.

  _Veuillez noter_ : il est important d'utiliser SIGINT pour redémarrer le serveur, sinon les messages non distribués ne seront pas restaurés. Les connexions seront restaurées indépendamment de la façon dont le serveur est redémarré, car contrairement aux messages, elles sont ajoutées aux logs (append-only) seulement à chaque changement.

- `Enable logging daily statistics (yN):`

  Entrez `y` pour activer l'enregistrement des statistiques au format CSV, par exemple, elles peuvent être utilisées pour afficher des graphiques d'utilisation globale dans `Grafana`.

Ces statistiques incluent le nombre quotidien de files d'attente créées, sécurisées et supprimées, de messages envoyés et reçus, ainsi que le nombre quotidien, hebdomadaire et mensuel de files d'attente actives (c'est-à-dire les files d'attente qui ont été utilisées pour des messages). Nous estimons que ces informations ne comportent aucun élément permettant de corréler différentes files d'attente comme appartenant aux mêmes utilisateurs, mais si vous pensez que cela peut être exploité de quelque manière que ce soit, veuillez nous le faire savoir, de manière confidentielle.

- `Require a password to create new messaging queues?`

  Entrez `r` ou votre mot de passe pour protéger votre `smp-server`, ou `n` pour désactiver la protection par mot de passe.

- `Enter server FQDN or IP address for certificate (127.0.0.1):`

  Entrez votre domaine ou l'adresse IP sur laquelle votre serveur smp fonctionne - elle sera incluse dans les certificats du serveur et également indiquée dans l'adresse du serveur.

### Option via ligne de commande

Exécutez la commande suivante :

```sh
sudo su smp -c "smp-server init -h"

...
Available options:
  -l,--store-log           Enable store log for persistence
  -s,--daily-stats         Enable logging daily server statistics
  -a,--sign-algorithm ALG  Signature algorithm used for TLS certificates:
                           ED25519, ED448 (default: ED448)
  --ip IP                  Server IP address, used as Common Name for TLS online
                           certificate if FQDN is not supplied
                           (default: "127.0.0.1")
  -n,--fqdn FQDN           Server FQDN used as Common Name for TLS online
                           certificate
  --no-password            Allow creating new queues without password
  --password PASSWORD      Set password to create new messaging queues
  -y,--yes                 Non-interactive initialization using command-line
                           options
  -h,--help                Show this help text
```

Vous devriez déterminer quels paramètres sont nécessaires pour votre cas d'utilisation et ensuite exécuter `smp-server init` avec le paramètre `-y` pour une initialisation non-interactive :

```sh
sudo su smp -c "smp-server init -y -<your flag> <your option>"
```

Par exemple, exécutez :

```sh
sudo su smp -c "smp-server init -y -l --ip 192.168.1.5 --password test"
```

pour initialiser votre `smp-server` avec comme configuration :

- restauration des connexions et des messages lors du redémarrage du serveur (paramètre `-l`),
- adresse IP `192.168.1.5`,
- protection du `smp-server` avec comme mot de passe `test`.

---

Après cela, votre installation est terminée et vous devriez voir dans votre teminal quelque chose comme ceci :

```sh
Certificate request self-signature ok
subject=CN = 127.0.0.1
Server initialized, you can modify configuration in /etc/opt/simplex/smp-server.ini.
Run `smp-server start` to start server.
----------
You should store CA private key securely and delete it from the server.
If server TLS credential is compromised this key can be used to sign a new one, keeping the same server identity and established connections.
CA private key location: /etc/opt/simplex/ca.key
----------
SMP server v3.4.0
Fingerprint: d5fcsc7hhtPpexYUbI2XPxDbyU2d3WsVmROimcL90ss=
Server address: smp://d5fcsc7hhtPpexYUbI2XPxDbyU2d3WsVmROimcL90ss=:V8ONoJ6ICwnrZnTC_QuSHfCEYq53uLaJKQ_oIC6-ve8=@<hostnames>
```

L'adresse du serveur ci-dessus doit être utilisée dans la configuration de votre client et si vous avez ajouté le mot de passe du serveur, il ne doit être partagé qu'avec les personnes que vous souhaitez autoriser à utiliser votre serveur pour recevoir les messages (tous vos contacts pourront envoyer des messages, car cela ne nécessite pas de mot de passe). Si vous avez fourni une adresse IP ou un nom d'hôte lors de l'initialisation, ils seront indiqués dans l'adresse du serveur, sinon remplacez `<hostnames>` par les adresses réelles du serveur.

## Documentation

Tous les fichiers requis pour `smp-server` sont situés dans le dossier `/etc/opt/simplex/`.

Les messages stockés, les connexions, les statistiques et les logs du serveur sont situés dans le dossier `/var/opt/simplex/`.

### Adresse de serveur SMP

Une adresse de serveur SMP a le format suivant :

```
smp://<fingerprint>[:<password>]@<public_hostname>[,<onion_hostname>]
```

- `<fingerprint>`

  Empreinte du certificat de votre `smp-server`. Vous pouvez vérifier l'empreinte de votre certificat dans `/etc/opt/simplex/fingerprint`.

- **optionnel** `<password>`

  Votre mot de passe configuré pour `smp-server`. Vous pouvez vérifier votre mot de passe configuré dans `/etc/opt/simplex/smp-server.ini`, sous la section `[AUTH]` dans le champ `create_password:`.

- `<public_hostname>`, **optionnel** `<onion_hostname>`

  Votre(vos) nom(s) d'hôte configuré(s) de `smp-server`. Vous pouvez vérifier vos hôtes configurés dans `/etc/opt/simplex/smp-server.ini`, dans la section `[AUTH]` dans le champ `host:`.

### Systemd commandes

Pour démarrer `smp-server` au démarrage de l'hôte, exécutez :

```sh
sudo systemctl enable smp-server.service

Created symlink /etc/systemd/system/multi-user.target.wants/smp-server.service → /etc/systemd/system/smp-server.service.
```

Pour démarrer `smp-server`, exécutez :

```sh
sudo systemctl start smp-server.service
```

Pour vérifier l'état du `smp-server`, exécutez :

```sh
sudo systemctl status smp-server.service

● smp-server.service - SMP server
     Loaded: loaded (/etc/systemd/system/smp-server.service; enabled; vendor preset: enabled)
     Active: active (running) since Sat 2022-11-23 19:23:21 UTC; 1min 48s ago
   Main PID: 30878 (smp-server)
     CGroup: /docker/5588ab759e80546b4296a7c50ffebbb1fb7b55b8401300e9201313b720989aa8/system.slice/smp-server.service
             └─30878 smp-server start

Nov 23 19:23:21 5588ab759e80 systemd[1]: Started SMP server.
Nov 23 19:23:21 5588ab759e80 smp-server[30878]: SMP server v3.4.0
Nov 23 19:23:21 5588ab759e80 smp-server[30878]: Fingerprint: d5fcsc7hhtPpexYUbI2XPxDbyU2d3WsVmROimcL90ss=
Nov 23 19:23:21 5588ab759e80 smp-server[30878]: Server address: smp://d5fcsc7hhtPpexYUbI2XPxDbyU2d3WsVmROimcL90ss=:V8ONoJ6ICwnrZnTC_QuSHfCEYq53uLaJKQ_oIC6-ve8=@<hostnames>
Nov 23 19:23:21 5588ab759e80 smp-server[30878]: Store log: /var/opt/simplex/smp-server-store.log
Nov 23 19:23:21 5588ab759e80 smp-server[30878]: Listening on port 5223 (TLS)...
Nov 23 19:23:21 5588ab759e80 smp-server[30878]: not expiring inactive clients
Nov 23 19:23:21 5588ab759e80 smp-server[30878]: creating new queues requires password
```

Pour arrêter `smp-server`, exécutez :

```sh
sudo systemctl stop smp-server.service
```

Pour vérifier la "tail" de `smp-server` des logs, exécutez :

```sh
sudo journalctl -fu smp-server.service

Nov 23 19:23:21 5588ab759e80 systemd[1]: Started SMP server.
Nov 23 19:23:21 5588ab759e80 smp-server[30878]: SMP server v3.4.0
Nov 23 19:23:21 5588ab759e80 smp-server[30878]: Fingerprint: d5fcsc7hhtPpexYUbI2XPxDbyU2d3WsVmROimcL90ss=
Nov 23 19:23:21 5588ab759e80 smp-server[30878]: Server address: smp://d5fcsc7hhtPpexYUbI2XPxDbyU2d3WsVmROimcL90ss=:V8ONoJ6ICwnrZnTC_QuSHfCEYq53uLaJKQ_oIC6-ve8=@<hostnames>
Nov 23 19:23:21 5588ab759e80 smp-server[30878]: Store log: /var/opt/simplex/smp-server-store.log
Nov 23 19:23:21 5588ab759e80 smp-server[30878]: Listening on port 5223 (TLS)...
Nov 23 19:23:21 5588ab759e80 smp-server[30878]: not expiring inactive clients
Nov 23 19:23:21 5588ab759e80 smp-server[30878]: creating new queues requires password
```

### Suivi de l'activité

Vous pouvez activer les statistiques de `smp-server` sur le tableau de bord `Grafana` en mettant la valeur `on` dans `/etc/opt/simplex/smp-server.ini`, sous la section `[STORE_LOG]` dans le champ `log_stats:`.

Les journaux seront stockés dans un fichier `csv` dans `/var/opt/simplex/smp-server-stats.daily.log`. Les champs pour le fichier `csv` sont :

```sh
fromTime,qCreated,qSecured,qDeleted,msgSent,msgRecv,dayMsgQueues,weekMsgQueues,monthMsgQueues
```

- `fromTime` - horodatage; date et heure de l'événement

- `qCreated` - int; files d'attente créées

- `qSecured` - int; files d'attente établies

- `qDeleted` - int; files d'attente supprimées

- `msgSent` - int; messages envoyés

- `msgRecv` - int; messages reçus

- `dayMsgQueues` - int; files d'attente actives en un jour

- `weekMsgQueues` - int; files d'attente actives en une semaine

- `monthMsgQueues` - int; files d'attente actives en un mois

Pour importer du `csv` dans `Grafana` il faut :

1. Installer le plugin Grafana : [Grafana - CSV datasource](https://grafana.com/grafana/plugins/marcusolsson-csv-datasource/)

2. Autoriser le mode local en ajoutant ce qui suit :

   ```sh
   [plugin.marcusolsson-csv-datasource]
   allow_local_mode = true
   ```

   ... to `/etc/grafana/grafana.ini`

3. Ajouter une source de données CSV :

   - Dans le menu latéral, cliquez sur l'onglet Configuration (icône en forme de rouage).
   - Cliquez sur Ajouter une source de données dans le coin supérieur droit de l'onglet Sources de données.
   - Entrez "CSV" dans le champ de recherche pour trouver la source de données CSV.
   - Cliquez sur le résultat de la recherche qui indique "CSV".
   - Dans URL, entrez un fichier qui pointe vers le contenu CSV.

4. C'est fait ! Vous devriez être en mesure de créer votre propre tableau de bord avec des statistiques.

Pour plus de documentation, voir : [CSV Data Source pour Grafana - Documentation](https://grafana.github.io/grafana-csv-datasource/)

### Configurer l'app pour utiliser le serveur

Pour configurer l'app afin d'utiliser votre serveur de messagerie, copiez son adresse complète, y compris le mot de passe, et ajoutez-la à l'application. Vous avez la possibilité d'utiliser votre serveur avec les serveurs prédéfinis ou sans eux - vous pouvez les supprimer ou les désactiver.

Il est également possible de partager l'adresse de votre serveur avec vos amis en leur permettant de scanner le code QR dans les paramètres du serveur. Ce code inclura le mot de passe du serveur, ce qui leur permettra aussi de recevoir des messages via votre serveur.

_Veuillez noter_ : vous avez besoin de la version 4.0 du serveur SMP pour avoir le support du mot de passe. Si vous avez déjà déployé un serveur, vous pouvez ajouter le mot de passe en l'ajoutant au fichier INI du serveur.

<img src="/docs/server_config_1.png" width="288"> &nbsp;&nbsp; <img src="/docs/server_config_2.png" width="288"> &nbsp;&nbsp; <img src="/docs/server_config_3.png" width="288">
