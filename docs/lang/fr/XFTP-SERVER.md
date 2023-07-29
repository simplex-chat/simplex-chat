---
title: Héberger son propre serveur XFTP
revision: 29.07.2023
---
# Héberger son propre serveur XFTP

## Aperçu général

XFTP est un nouveau protocole de transfert de fichiers axé sur la protection des métadonnées. Il repose sur les mêmes principes que le protocole de messagerie SimpleX utilisé dans le messager SimpleX Chat :

   - livraison asynchrone de fichiers - l'expéditeur n'a pas besoin d'être en ligne pour recevoir le fichier, qui est stocké sur les relais XFTP pendant une durée limitée (actuellement 48 heures) ou jusqu'à ce qu'il soit supprimé par l'expéditeur.
   - chiffrement e2e du contenu du fichier.
   - remplissage du contenu et morceaux de taille fixe envoyés via différents relais XFTP, assemblés dans le fichier d'origine par le client destinataire.
   - envoi efficace à plusieurs destinataires (le fichier ne doit être téléchargé qu'une seule fois).
   - aucun identifiant ou texte chiffré commun entre le trafic de relais envoyé et reçu, comme pour les messages délivrés par les relais SMP.
   - protection de l'adresse IP de l'expéditeur contre les destinataires.

## Installation

1. Téléchargez le fichier `xftp-server` :

   ```sh
   sudo curl -L https://github.com/simplex-chat/simplexmq/releases/latest/download/xftp-server-ubuntu-20_04-x86-64 -o /usr/local/bin/xftp-server && sudo chmod +x /usr/local/bin/xftp-server
   ```

2. Créez un utilisateur et un groupe pour `xftp-server`:

   ```sh
   sudo useradd -m xftp
   ```

3. Créez les répertoires nécessaires et attribuez les autorisations :

   ```sh
   sudo mkdir -p /var/opt/simplex-xftp /etc/opt/simplex-xftp /srv/xftp
   sudo chown xftp:xftp /var/opt/simplex-xftp /etc/opt/simplex-xftp /srv/xftp
   ```

4. Autoriser le port du serveur xftp dans le pare-feu :

   ```sh
   # For Ubuntu
   sudo ufw allow 443/tcp
   # For Fedora
   sudo firewall-cmd --permanent --add-port=443/tcp && \
   sudo firewall-cmd --reload
   ```

5. **Optionnel** - Si vous utilisez une distribution avec `systemd`, créez le fichier `/etc/systemd/system/xftp-server.service` avec le contenu suivant :

   ```sh
   [Unit]
   Description=XFTP server systemd service
  
   [Service]
   User=xftp
   Group=xftp
   Type=simple
   ExecStart=/usr/local/bin/xftp-server start +RTS -N -RTS
   ExecStopPost=/usr/bin/env sh -c '[ -e "/var/opt/simplex-xftp/file-server-store.log" ] && cp "/var/opt/simplex-xftp/file-server-store.log" "/var/opt/simplex-xftp/file-server-store.log.$(date +'%FT%T')"'
   LimitNOFILE=65535
   KillSignal=SIGINT
   TimeoutStopSec=infinity
   AmbientCapabilities=CAP_NET_BIND_SERVICE
      
   [Install]
   WantedBy=multi-user.target
   ```

   Et exécutez `sudo systemctl daemon-reload`.

## Configuration

Pour voir quelles sont les options disponibles, exécutez `xftp-server` sans paramètre :

```sh
sudo su xftp -c xftp-server

...
Commandes disponibles :
  init                     Initialise le serveur - crée les répertoires /etc/opt/simplex-xftp et 
                           /var/opt/simplex-xftp et les fichiers de configuration
  start                    Démarre le serveur (configuration:
                           /etc/opt/simplex-xftp/file-server.ini)
  delete                   Supprimer les fichiers de configuration et les fichiers journaux

```

Vous pouvez obtenir de l'aide supplémentaire en exécutant `su xftp -c "xftp-server <command> -h"`

Ensuite, nous devons configurer `xftp-server`:

```sh
sudo su xftp -c "xftp-server init -h"

...
Available options:
  -l,--store-log           Active la persistance du journal de stockage
  -a,--sign-algorithm ALG  Algorithme de signature utilisé pour les certificats TLS:
                           ED25519, ED448 (par défaut: ED448)
  --ip IP                  Adresse IP du serveur, utilisée comme nom commun pour le
                           certificat TLS en ligne si le FQDN n est pas fourni
                           (par défaut: "127.0.0.1")
  -n,--fqdn FQDN           FQDN du serveur utilisé comme nom 
                           commun pour TLS en ligne en ligne
  -p,--path PATH           Chemin d accès au répertoire de stockage des fichiers
  -q,--quota QUOTA         Quota de stockage de fichiers (par exemple 100gb)
  -h,--help                Affiche ce texte d aide
```

Vous devez déterminer quels paramètres sont nécessaires pour votre cas d'utilisation, puis exécuter la commande `xftp-server init`:

```sh
sudo su xftp -c "xftp-server init -<your flag> <your option>"
```

Par exemple, exécutez :

```sh
sudo su xftp -c "xftp-server init -l --ip 192.168.1.5 -q '20gb' -p /srv/xftp/"
```

pour initialiser votre configuration `xftp-server` :

- restaurer les connexions lorsque le serveur est redémarré (flag `-l`),
- l'adresse IP `192.168.1.5` (flag `--ip`),
- fixer le quota global de stockage à 10 Go (flag `-q`),
- stocker les fichiers dans le répertoire `/srv/xftp` (flag `-p`).

Pour protéger votre `xftp-server` par un mot de passe, modifiez-le dans la configuration :

   1. Ouvrez la configuration avec :
      
      ```sh
      sudo su xftp -c "vim /etc/opt/simplex-xftp/file-server.ini"
      ```
   2. Dans la section `[AUTH]`, décommentez `create_password` et modifiez-le :
  
      ```sh
      ...
      [AUTH]
      # Set new_files option to off to completely prohibit uploading new files.
      # This can be useful when you want to decommission the server, but still allow downloading the existing files.
      new_files: on
      
      # Use create_password option to enable basic auth to upload new files.
      # The password should be used as part of server address in client configuration:
      # xftp://fingerprint:password@host1,host2
      # The password will not be shared with file recipients, you must share it only
      # with the users who you want to allow uploading files to your server.
      create_password: your_very_secure_password
      ...
      ```
---

Après cela, l'installation est terminée et vous devriez voir dans la fenêtre de terminal quelque chose comme ceci 

```sh
Certificate request self-signature ok
subject=CN = 192.168.1.5
Server initialized, you can modify configuration in /etc/opt/simplex-xftp/file-server.ini.
Run `file-server start` to start server.
----------
You should store CA private key securely and delete it from the server.
If server TLS credential is compromised this key can be used to sign a new one, keeping the same server identity and established connections.
CA private key location:
/etc/opt/simplex-xftp/ca.key
----------
SimpleX XFTP server v0.1.0
Fingerprint: ioyYeRyy4SqJkNvb_7nM04MuLasOM4c-acVyVnqw248=
Server address: xftp://ioyYeRyy4SqJkNvb_7nM04MuLasOM4c-acVyVnqw248=@<hostnames>
```

L'adresse du serveur ci-dessus doit être utilisée dans la configuration de votre client et si vous avez ajouté le mot de passe du serveur, il ne doit être partagé avec les autres personnes que lorsque vous voulez leur permettre d'utiliser votre serveur pour télécharger des fichiers. Si vous avez passé l'adresse IP ou les noms d'hôtes lors de l'initialisation, ils seront imprimés comme faisant partie de l'adresse du serveur, sinon remplacez `<hostnames>` par les adresses réelles du serveur.

## Documentation

Tous les fichiers nécessaires au serveur `xftp` sont situés dans le dossier `/etc/opt/simplex-xftp/`.

Les messages stockés, les connexions, les statistiques et le journal du serveur sont situés dans le dossier `/var/opt/simplex-xftp/`.

L'emplacement des fichiers téléchargés est configuré par l'utilisateur. Dans notre guide, nous utilisons `/srv/xftp/`

### Adresse du serveur XFTP

XFTP server address has the following format:

```
xftp://<fingerprint>[:<password>]@<public_hostname>[,<onion_hostname>]
```

- `<fingerprint>`

  L'empreinte du certificat de votre serveur `xftp`. Vous pouvez vérifier l'empreinte de votre certificat dans `/etc/opt/simplex-xftp/fingerprint`.

- **optionnel** `<password>`

  Votre mot de passe configuré pour `xftp-server`. Vous pouvez vérifier votre mot de passe configuré dans `/etc/opt/simplex-xftp/file-server.ini`, dans la section `[AUTH]` dans le champ `create_password:`.

- `<public_hostname>`, **optionnel** `<onion_hostname>`

  Le(s) nom(s) d'hôte configuré(s) de `xftp-server`. Vous pouvez vérifier les hôtes configurés dans `/etc/opt/simplex-xftp/file-server.ini`, dans la section `[TRANSPORT]` dans le champ `host:`.

### Commandes Systemd

Pour démarrer `xftp-server` au démarrage de l'hôte, exécutez :

```sh
sudo systemctl enable xftp-server.service

Created symlink /etc/systemd/system/multi-user.target.wants/xftp-server.service → /etc/systemd/system/xftp-server.service.
```

Pour démarrer `xftp-server`, exécutez :

```sh
sudo systemctl start xftp-server.service
```

Pour vérifier l'état de `xftp-server`, exécutez :

```sh
sudo systemctl status xftp-server.service

● xftp-server.service - XFTP server systemd service
     Loaded: loaded (/etc/systemd/system/xftp-server.service; enabled; vendor preset: enabled)
     Active: active (running) since Sat 2023-03-11 13:11:55 UTC; 1 months 10 days ago
   Main PID: 110770 (xftp-server)
      Tasks: 14 (limit: 4611)
     Memory: 2.4G
     CGroup: /system.slice/xftp-server.service
             └─110770 /usr/local/bin/xftp-server start +RTS -N -RTS

Feb 27 19:21:11 localhost systemd[1]: Started XFTP server systemd service.
Feb 27 19:21:11 localhost xftp-server[2350]: SimpleX XFTP server v0.1.0
Feb 27 19:21:11 localhost xftp-server[2350]: Fingerprint: ioyYeRyy4SqJkNvb_7nM04MuLasOM4c-acVyVnqw248=
Feb 27 19:21:11 localhost xftp-server[2350]: Server address: xftp://ioyYeRyy4SqJkNvb_7nM04MuLasOM4c-acVyVnqw248=@<hostnames>
Feb 27 19:21:11 localhost xftp-server[2350]: Store log: /var/opt/simplex-xftp/file-server-store.log
Feb 27 19:21:11 localhost xftp-server[2350]: Uploading new files allowed.
Feb 27 19:21:11 localhost xftp-server[2350]: Listening on port 443...
Feb 27 19:21:11 localhost xftp-server[2350]: [INFO 2023-02-27 19:21:11 +0000 src/Simplex/FileTransfer/Server/Env.hs:85] Total / available storage: 64424509440 / 64424509440
```

Pour arrêter `xftp-server`, exécutez :

```sh
sudo systemctl stop xftp-server.service
```

Pour vérifier la queue du journal du serveur `xftp`, exécutez :

```sh
sudo journalctl -fu xftp-server.service

Feb 27 19:21:11 localhost systemd[1]: Started XFTP server systemd service.
Feb 27 19:21:11 localhost xftp-server[2350]: SimpleX XFTP server v0.1.0
Feb 27 19:21:11 localhost xftp-server[2350]: Fingerprint: ioyYeRyy4SqJkNvb_7nM04MuLasOM4c-acVyVnqw248=
Feb 27 19:21:11 localhost xftp-server[2350]: Server address: xftp://ioyYeRyy4SqJkNvb_7nM04MuLasOM4c-acVyVnqw248=@<hostnames>
Feb 27 19:21:11 localhost xftp-server[2350]: Store log: /var/opt/simplex-xftp/file-server-store.log
Feb 27 19:21:11 localhost xftp-server[2350]: Uploading new files allowed.
Feb 27 19:21:11 localhost xftp-server[2350]: Listening on port 443...
Feb 27 19:21:11 localhost xftp-server[2350]: [INFO 2023-02-27 19:21:11 +0000 src/Simplex/FileTransfer/Server/Env.hs:85] Total / available storage: 64424509440 / 64424509440
````

### Suivi

Vous pouvez activer les statistiques du serveur `xftp` pour le tableau de bord `Grafana` en mettant la valeur `on` dans `/etc/opt/simplex-xftp/file-server.ini`, dans la section `[STORE_LOG]` dans le champ `log_stats:`.

Les logs seront stockés dans un fichier `csv` dans `/var/opt/simplex-xftp/file-server-stats.daily.log`. Les champs du fichier `csv` sont :

```sh
fromTime,filesCreated,fileRecipients,filesUploaded,filesDeleted,dayCount,weekCount,monthCount,fileDownloads,fileDownloadAcks,filesCount,filesSize
```

- `fromTime` - timestamp; date et heure de l'événement

- `filesCreated` - int; morceaux créés

- `fileRecipients` - int; nombre de destinataires de morceaux de fichiers

- `filesUploaded` - int; morceaux uploadés

- `filesDeleted` - int; morceaux supprimés

- `dayCount` - int; morceaux uploadés en un jour

- `weekCount` - int; morceaux uploadés en une semaine

- `monthCount` - int; morceaux uploadés en un mois

- `fileDownloads` - int; morceaux téléchargés

- `filesCount` - int; nombre de morceaux de fichiers stockés

- `filesSize` - int; taille totale des morceaux du fichier uploadé


Pour importer `csv` dans `Grafana`, il faut :

1. Installer le plugin Grafana : [Grafana - CSV datasource](https://grafana.com/grafana/plugins/marcusolsson-csv-datasource/)

2. Autoriser le mode local en ajoutant ce qui suit :

   ```sh
   [plugin.marcusolsson-csv-datasource]
   allow_local_mode = true
   ```

   ... to `/etc/grafana/grafana.ini`

3. Ajoutez une source de données CSV :

   - Dans le menu latéral, cliquez sur l'onglet Configuration (icône en forme de roue dentée).
   - Cliquez sur Ajouter une source de données dans le coin supérieur droit de l'onglet Sources de données.
   - Saisissez "CSV" dans le champ de recherche pour trouver la source de données CSV.
   - Cliquez sur le résultat de la recherche qui indique "CSV".
   - Dans l'URL, entrez un fichier qui pointe vers le contenu CSV.

4. Vous avez terminé ! Vous devriez être en mesure de créer votre propre tableau de bord avec des statistiques.

Pour plus de documentation, voir : [Source de données CSV pour Grafana - Documentation](https://grafana.github.io/grafana-csv-datasource/)

### Configurer l'application pour utiliser le serveur

Veuillez consulter : [Serveur SMP : Configurer l'application pour utiliser le serveur](./SERVER.md#configurer-lapp-pour-utiliser-le-serveur).
