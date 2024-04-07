---
title: Utilisation de serveurs WebRTC ICE personnalisés dans SimpleX Chat
revision: 31.01.2023
---
| 31.01.2023 | FR, [EN](/docs/WEBRTC.md), [CZ](/docs/lang/cs/WEBRTC.md), [PL](/docs/lang/pl/CLI.md) |

# Utilisation de serveurs WebRTC ICE personnalisés dans SimpleX Chat

## Déployer un serveur STUN/TURN

Pour ce guide, nous utiliserons l'implémentation de serveur STUN/TURN la plus complète et la plus testée - [`coturn`](https://github.com/coturn/coturn) et la distribution Linux [`Ubuntu 20.04 LTS`](https://ubuntu.com/download/server).

0. Obtenez les certificats `stun.$VOTRE_DOMAINE` et `turn.$VOTRE_DOMAINE`.

   Nous utilisons [Let's Encrypt](https://letsencrypt.org/fr/getting-started/).

1. Installez le package `coturn` depuis le dépôt principal.

```sh
apt update && apt install coturn`
```

2. Décommentez `TURNSERVER_ENABLED=1` de `/etc/default/coturn` :

```sh
sed -i '/TURN/s/^#//g' /etc/default/coturn
```

3. Configurez `coturn` dans `/etc/turnserver.conf` :

   Veuillez également consulter les commentaires relatifs pour chacune des options.

```sh
# tls sur le port 443
alt-tls-listening-port=443
# Utiliser les empreintes dans les messages TURN
fingerprint
# Utiliser le mécanisme d'identification à long terme
lt-cred-mech
# Vos informations d'identification
user=$VOTRE_LOGIN:$VOTRE_MOT_DE_PASSE
# Le domaine de votre serveur
server-name=$VOTRE_DOMAINE
# Le domaine par défaut à utiliser pour les utilisateurs lorsqu'aucune relation explicite origine/domaine n'a été trouvée
realm=$VOTRE_DOMAINE
# Chemin vers vos certificats. Assurez-vous qu'ils sont lisibles par l'utilisateur/groupe du processus cotun
cert=/var/lib/turn/cert.pem
pkey=/var/lib/turn/key.pem
# Utiliser la clé DH TLS prédéfinie de 2066 bits
dh2066
# Log sur journalctl
syslog
# Utilisateur/groupe qui exécutera le service coturn
proc-user=turnserver
proc-group=turnserver
# Désactiver le chiffrement faible
no-tlsv1
no-tlsv1_1
no-tlsv1_2
```

4. Démarrez et activez le service `coturn` :

```sh
systemctl enable coturn && systemctl start coturn
```

5. Optionnellement, si vous utilisez le pare-feu `ufw`, ouvrez les ports appropriés :

- **3478** – "simple" TURN/STUN;
- **5349** – TURN/STUN over TLS;
- **443** – TURN/STUN over TLS, qui peuvent contourner les pare-feu;
- **49152:65535** – plage de ports que Coturn utilisera par défaut pour le relais TURN.

```sh
# Pour Ubuntu
sudo ufw allow 3478 && \
sudo ufw allow 443 && \
sudo ufw allow 5349 && \
sudo ufw allow 49152:65535/tcp && \
sudo ufw allow 49152:65535/udp

# Pour Fedora
sudo firewall-cmd --permanent --add-port=443/tcp && \
sudo firewall-cmd --permanent --add-port=443/udp && \
sudo firewall-cmd --permanent --add-port=5349/tcp && \
sudo firewall-cmd --permanent --add-port=5349/udp && \
sudo firewall-cmd --permanent --add-port=49152:65535/tcp && \
sudo firewall-cmd --permanent --add-port=49152:65535/udp && \
sudo firewall-cmd --reload
```

## Configurer l'app mobile

Pour configurer votre application mobile afin d'utiliser votre serveur :

1. Ouvrez `Paramètres / Réseau & Serveurs / Serveurs WebRTC ICE` et activez la case `Configurer les serveurs ICE`.

2. Entrez toutes les adresses des serveurs dans le champ, une par ligne, par exemple si vos serveurs sont sur le port 5349 :

```
stun:stun.example.com:5349
turn:username:password@turn.example.com:5349
```

Voilà, vous pouvez désormais passer des appels audio et vidéo via votre propre serveur, sans partager aucune donnée avec nos serveurs (autre que l'échange de clés avec votre contact dans les messages cryptés E2E).

## Dépannage

- **Déterminer si un serveur est disponible** :

  Exécutez cette commande dans votre terminal :

  ```sh
  ping <votre_ip_ou_domaine>
  ```

  Si des paquets sont transmis, le serveur est opérationnel !

- **Déterminez si les ports sont ouverts** :

  Exécutez cette commande dans votre terminal :

  ```sh
  nc -zvw10 <votre_ip_ou_domaine> 443 5349
  ```

  Vous devriez voir :

  ```
  Connection to <votre_ip_ou_domaine> 443 port [tcp/https] succeeded!
  Connection to <votre_ip_ou_domaine> 5349 port [tcp/*] succeeded!
  ```

- **Testez la connectivité STUN/TURN** :

  1. Allez sur [IceTest](https://icetest.info/).

  2. Dans la section **Build up ICE Server List**, ajoutez :

     <img src="/docs/stun_1.png">

     - `STUN: stun:<votre_ip_ou_domaine>:<port>` et appuyez sur `Add STUN`
     - `TURN: turn:<votre_ip_ou_domaine>:<port>`, `Username: <votre_login>`, `Credential: <votre_pass>` et appuyez sur `Add TURN`

     Où `<port>` est 443 ou 5349.

  3. Vous devriez voir vos serveurs dans la section **ICE server list**. Si tout est correctement configuré, cliquez sur `Start test` :

     <img src="/docs/stun_2.png">

  4. Dans la section **Results**, vous devriez obtenir quelque chose comme ceci :

     <img src="/docs/stun_3.png">

     Si les résultats montrent des candidats `srflx` et `relay`, tout est correctement configuré !

