---
title: Hosting your own SMP Server
revision: 03.06.2024
---

| Updated 28.05.2024 | Languages: EN, [FR](/docs/lang/fr/SERVER.md), [CZ](/docs/lang/cs/SERVER.md), [PL](/docs/lang/pl/SERVER.md) |

### Table of Contents

- [Hosting your own SMP server](#hosting-your-own-smp-server)
  - [Overview](#overview)
  - [Installation](#installation)
  - [Configuration](#configuration)
    - [Interactively](#interactively)
    - [Via command line options](#via-command-line-options)
  - [Further configuration](#further-configuration)
  - [Server security](#server-security)
    - [Initialization](#initialization)
    - [Private keys](#private-keys)
    - [Online certificate rotation](#online-certificate-rotation)
  - [Tor: installation and configuration](#tor-installation-and-configuration)
    - [Installation for onion address](#installation-for-onion-address)
    - [SOCKS port for SMP PROXY](#socks-port-for-smp-proxy)
  - [Server information page](#server-information-page)
  - [Documentation](#documentation)
    - [SMP server address](#smp-server-address)
    - [Systemd commands](#systemd-commands)
    - [Monitoring](#monitoring)
  - [Updating your SMP server](#updating-your-smp-server)
  - [Configuring the app to use the server](#configuring-the-app-to-use-the-server)

# Hosting your own SMP Server

## Overview

SMP server is the relay server used to pass messages in SimpleX network. SimpleX Chat apps have preset servers (for mobile apps these are smp11, smp12 and smp14.simplex.im), but you can easily change app configuration to use other servers.

SimpleX clients only determine which server is used to receive the messages, separately for each contact (or group connection with a group member), and these servers are only temporary, as the delivery address can change.

_Please note_: when you change the servers in the app configuration, it only affects which servers will be used for the new contacts, the existing contacts will not automatically move to the new servers, but you can move them manually using ["Change receiving address"](../blog/20221108-simplex-chat-v4.2-security-audit-new-website.md#change-your-delivery-address-beta) button in contact/member information pages – it will be automated in the future.

## Installation

1. First, install `smp-server`:

   - Manual deployment (see below)

   - Semi-automatic deployment:
     - [Installation script](https://github.com/simplex-chat/simplexmq#using-installation-script)
     - [Docker container](https://github.com/simplex-chat/simplexmq#using-docker)
     - [Linode Marketplace](https://www.linode.com/marketplace/apps/simplex-chat/simplex-chat/)

Manual installation requires some preliminary actions:

1. Install binary:

   - Using pre-compiled binaries:

     ```sh
     curl -L https://github.com/simplex-chat/simplexmq/releases/latest/download/smp-server-ubuntu-20_04-x86-64 -o /usr/local/bin/smp-server && chmod +x /usr/local/bin/smp-server
     ```

   - Compiling from source:

     Please refer to [Build from source: Using your distribution](https://github.com/simplex-chat/simplexmq#using-your-distribution)

2. Create user and group for `smp-server`:

   ```sh
   sudo useradd -m smp
   ```

3. Create necessary directories and assign permissions:

   ```sh
   sudo mkdir -p /var/opt/simplex /etc/opt/simplex
   sudo chown smp:smp /var/opt/simplex /etc/opt/simplex
   ```

4. Allow `smp-server` port in firewall:

   ```sh
   # For Ubuntu
   sudo ufw allow 5223/tcp
   # For Fedora
   sudo firewall-cmd --permanent --add-port=5223/tcp && \
   sudo firewall-cmd --reload
   ```

5. **Optional** — If you're using distribution with `systemd`, create `/etc/systemd/system/smp-server.service` file with the following content:

   ```sh
   [Unit]
   Description=SMP server systemd service

   [Service]
   User=smp
   Group=smp
   Type=simple
   ExecStart=/usr/local/bin/smp-server start +RTS -N -RTS
   ExecStopPost=/usr/bin/env sh -c '[ -e "/var/opt/simplex/smp-server-store.log" ] && cp "/var/opt/simplex/smp-server-store.log" "/var/opt/simplex/smp-server-store.log.bak"'
   LimitNOFILE=65535
   KillSignal=SIGINT
   TimeoutStopSec=infinity

   [Install]
   WantedBy=multi-user.target
   ```

   And execute `sudo systemctl daemon-reload`.

## Configuration

To see which options are available, execute `smp-server` without flags:

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

You can get further help by executing `sudo su smp -c "smp-server <command> -h"`

After that, we need to configure `smp-server`:

### Interactively

Execute the following command:

```sh
sudo su smp -c "smp-server init"
```

There are several options to consider:

- `Enable store log to restore queues and messages on server restart (Yn):`

  Enter `y` to enable saving and restoring connections and messages when the server is restarted.

  _Please note_: it is important to use SIGINT to restart the server, as otherwise the undelivered messages will not be restored. The connections will be restored irrespective of how the server is restarted, as unlike messages they are added to append-only log on every change.

- `Enable logging daily statistics (yN):`

  Enter `y` to enable logging statistics in CSV format, e.g. they can be used to show aggregate usage charts in `Grafana`.

These statistics include daily counts of created, secured and deleted queues, sent and received messages, and also daily, weekly, and monthly counts of active queues (that is, the queues that were used for any messages). We believe that this information does not include anything that would allow correlating different queues as belonging to the same users, but please [let us know](./SECURITY.md), confidentially, if you believe that this can be exploited in any way.

- `Require a password to create new messaging queues?`

  Press `Enter` or enter your arbitrary password to password-protect `smp-server`, or `n` to disable password protection.

- `Enter server FQDN or IP address for certificate (127.0.0.1):`

  Enter your domain or ip address that your smp-server is running on - it will be included in server certificates and also printed as part of server address.

### Via command line options

Execute the following command:

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

You should determine which flags are needed for your use-case and then execute `smp-server init` with `-y` flag for non-interactive initialization:

```sh
sudo su smp -c "smp-server init -y -<your flag> <your option>"
```

For example, run:

```sh
sudo su smp -c "smp-server init -y -l --ip 192.168.1.5 --password test"
```

to initialize your `smp-server` configuration with:

- restoring connections and messages when the server is restarted (`-l` flag),
- IP address `192.168.1.5`,
- protect `smp-server` with a password `test`.

---

After that, your installation is complete and you should see in your teminal output something like this:

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

The server address above should be used in your client configuration, and if you added server password it should only be shared with the other people who you want to allow using your server to receive the messages (all your contacts will be able to send messages - it does not require a password). If you passed IP address or hostnames during the initialisation, they will be printed as part of server address, otherwise replace `<hostnames>` with the actual server hostnames.

## Further configuration

All generated configuration, along with a description for each parameter, is available inside configuration file in `/etc/opt/simplex/smp-server.ini` for further customization. Depending on the smp-server version, the configuration file looks something like this:

```ini
[INFORMATION]
# AGPLv3 license requires that you make any source code modifications
# available to the end users of the server.
# LICENSE: https://github.com/simplex-chat/simplexmq/blob/stable/LICENSE
# Include correct source code URI in case the server source code is modified in any way.
# If any other information fields are present, source code property also MUST be present.

source_code: https://github.com/simplex-chat/simplexmq

# Declaring all below information is optional, any of these fields can be omitted.

# Server usage conditions and amendments.
# It is recommended to use standard conditions with any amendments in a separate document.
# usage_conditions: https://github.com/simplex-chat/simplex-chat/blob/stable/PRIVACY.md
# condition_amendments: link

# Server location and operator.
server_country: <YOUR_SERVER_LOCATION>
operator: <YOUR_NAME>
operator_country: <YOUR_LOCATION>
website: <WEBSITE_IF_AVAILABLE>

# Administrative contacts.
#admin_simplex: SimpleX address
admin_email: <EMAIL>
# admin_pgp:
# admin_pgp_fingerprint:

# Contacts for complaints and feedback.
# complaints_simplex: SimpleX address
complaints_email: <COMPLAINTS_EMAIL>
# complaints_pgp:
# complaints_pgp_fingerprint:

# Hosting provider.
hosting: <HOSTING_PROVIDER_NAME>
hosting_country: <HOSTING_PROVIDER_LOCATION>

[STORE_LOG]
# The server uses STM memory for persistence,
# that will be lost on restart (e.g., as with redis).
# This option enables saving memory to append only log,
# and restoring it when the server is started.
# Log is compacted on start (deleted objects are removed).
enable: on

# Undelivered messages are optionally saved and restored when the server restarts,
# they are preserved in the .bak file until the next restart.
restore_messages: on
expire_messages_days: 21

# Log daily server statistics to CSV file
log_stats: on

[AUTH]
# Set new_queues option to off to completely prohibit creating new messaging queues.
# This can be useful when you want to decommission the server, but not all connections are switched yet.
new_queues: on

# Use create_password option to enable basic auth to create new messaging queues.
# The password should be used as part of server address in client configuration:
# smp://fingerprint:password@host1,host2
# The password will not be shared with the connecting contacts, you must share it only
# with the users who you want to allow creating messaging queues on your server.
# create_password: password to create new queues (any printable ASCII characters without whitespace, '@', ':' and '/')

[TRANSPORT]
# host is only used to print server address on start
host: <your server domain/ip>
port: 5223
log_tls_errors: off
websockets: off
# control_port: 5224

[PROXY]
# Network configuration for SMP proxy client.
# `host_mode` can be 'public' (default) or 'onion'.
# It defines prefferred hostname for destination servers with multiple hostnames.
# host_mode: public
# required_host_mode: off

# The domain suffixes of the relays you operate (space-separated) to count as separate proxy statistics.
# own_server_domains: <your domain suffixes>

# SOCKS proxy port for forwarding messages to destination servers.
# You may need a separate instance of SOCKS proxy for incoming single-hop requests.
# socks_proxy: localhost:9050

# `socks_mode` can be 'onion' for SOCKS proxy to be used for .onion destination hosts only (default)
# or 'always' to be used for all destination hosts (can be used if it is an .onion server).
# socks_mode: onion

# Limit number of threads a client can spawn to process proxy commands in parrallel.
# client_concurrency: 32

[INACTIVE_CLIENTS]
# TTL and interval to check inactive clients
disconnect: off
# ttl: 43200
# check_interval: 3600

[WEB]
# Set path to generate static mini-site for server information and qr codes/links
static_path: <WRITABLE_PATH_TO_STORE_WEBSITE>

# Run an embedded server on this port
# Onion sites can use any port and register it in the hidden service config.
# Running on a port 80 may require setting process capabilities.
# http: 8000

# You can run an embedded TLS web server too if you provide port and cert and key files.
# Not required for running relay on onion address.
# https: 443
# cert: /etc/opt/simplex/web.cert
# key: /etc/opt/simplex/web.key
```

## Server security

### Initialization

Although it's convenient to initialize smp-server configuration directly on the server, operators **ARE ADVISED** to initialize smp-server fully offline to protect your SMP server CA private key.

Follow the steps to quickly initialize the server offline:

1. Install Docker on your system.

2. Deploy [smp-server](https://github.com/simplex-chat/simplexmq#using-docker) locally.

3. Destroy the container. All relevant configuration files and keys will be available at `$HOME/simplex/smp/config`.

4. Move your `CA` private key (`ca.key`) to the safe place. For further explanation, see the next section: [Server security: Private keys](#private-keys).

5. Copy all other configuration files **except** the CA key to the server:

   ```sh
   rsync -hzasP $HOME/simplex/smp/config/ <server_user>@<server_address>:/etc/opt/simplex/
   ```

### Private keys

Connection to the smp server occurs via a TLS connection. During the TLS handshake, the client verifies smp-server CA and server certificates by comparing its fingerprint with the one included in server address. If server TLS credential is compromised, this key can be used to sign a new one, keeping the same server identity and established connections. In order to protect your smp-server from bad actors, operators **ARE ADVISED** to move CA private key to a safe place. That could be:

- [Tails](https://tails.net/) live usb drive with [persistent and encrypted storage](https://tails.net/doc/persistent_storage/create/index.en.html).
- Offline Linux laptop.
- Bitwarden.
- Any other safe storage that satisfy your security requirements.

Follow the steps to secure your CA keys:

1. Login to your server via SSH.

2. Copy the CA key to a safe place from this file:

   ```sh
   /etc/opt/simplex/ca.key
   ```

3. Delete the CA key from the server. **Please make sure you've saved you CA key somewhere safe. Otherwise, you would lose the ability to [rotate the online certificate](#online-certificate-rotation)**:

   ```sh
   rm /etc/opt/simplex/ca.key
   ```

### Online certificate rotation

Operators of smp servers **ARE ADVISED** to rotate online certificate regularly (e.g., every 3 months). In order to do this, follow the steps:

1. Create relevant folders:

   ```sh
   mkdir -p $HOME/simplex/smp/config
   ```

1. Copy the configuration files from the server to the local machine (if not yet):

   ```sh
   rsync -hzasP <server_user>@<server_address>:/etc/opt/simplex/ $HOME/simplex/smp/config/
   ```

2. **Copy** your CA private key from a safe place to the local machine and name it `ca.key`.

3. Download latest `smp-server` binary [from Github releases](https://github.com/simplex-chat/simplexmq/releases):

   ```sh
   curl -L 'https://github.com/simplex-chat/simplexmq/releases/latest/download/smp-server-ubuntu-20_04-x86-64' -o smp-server
   ```

4. Put the `smp-server` binary to your `$PATH` and make it executable:

   ```sh
   sudo mv smp-server /usr/local/bin/ && chmod +x /usr/local/bin/smp-server
   ```

5. Export a variable to configure your path to smp-server configuration:

   ```sh
   export SMP_SERVER_CFG_PATH=$HOME/simplex/smp/config
   ```

6. Execute the following command:

   ```sh
   smp-server cert
   ```

   This command should print:

   ```sh
   Certificate request self-signature ok
   subject=CN = <your domain or IP>
   Generated new server credentials
   ----------
   You should store CA private key securely and delete it from the server.
   If server TLS credential is compromised this key can be used to sign a new one, keeping the same server identity and established connections.
   CA private key location:
   $HOME/simplex/smp/config/ca.key
   ----------
   ```

7. Remove the CA key from the config folder (make sure you have a backup!):

   ```sh
   rm $HOME/simplex/smp/config/ca.key
   ```

8. Upload new certificates to the server:

   ```sh
   rsync -hzasP $HOME/simplex/smp/config/ <server_user>@<server_address>:/etc/opt/simplex/
   ```

9. Connect to the server via SSH and restart the service:

   ```sh
   ssh <server_user>@<server_address> "systemctl restart smp-server"
   ```

10. Done!

## Tor: installation and configuration

### Installation for onion address

SMP-server can also be deployed to be available via [Tor](https://www.torproject.org) network. Run the following commands as `root` user.

1. Install tor:

   We're assuming you're using Ubuntu/Debian based distributions. If not, please refer to [offical tor documentation](https://community.torproject.org/onion-services/setup/install/) or your distribution guide.

   - Configure offical Tor PPA repository:

     ```sh
     CODENAME="$(lsb_release -c | awk '{print $2}')"
     echo "deb [signed-by=/usr/share/keyrings/tor-archive-keyring.gpg] https://deb.torproject.org/torproject.org ${CODENAME} main
     deb-src [signed-by=/usr/share/keyrings/tor-archive-keyring.gpg] https://deb.torproject.org/torproject.org ${CODENAME} main" > /etc/apt/sources.list.d/tor.list
     ```

   - Import repository key:

     ```sh
     curl --proto '=https' --tlsv1.2 -sSf https://deb.torproject.org/torproject.org/A3C4F0F979CAA22CDBA8F512EE8CBC9E886DDD89.asc | gpg --dearmor | tee /usr/share/keyrings/tor-archive-keyring.gpg >/dev/null
     ```

   - Update repository index:

     ```sh
     apt update
     ```

   - Install `tor` package:

     ```sh
     apt install -y tor deb.torproject.org-keyring
     ```

2. Configure tor:

   - File configuration:
  
     Open tor configuration with your editor of choice (`nano`,`vim`,`emacs`,etc.):

     ```sh
     vim /etc/tor/torrc
     ```

     And insert the following lines to the bottom of configuration. Please note lines starting with `#`: this is comments about each individual options.

     ```sh
     # Enable log (otherwise, tor doesn't seem to deploy onion address)
     Log notice file /var/log/tor/notices.log
     # Enable single hop routing (2 options below are dependencies of the third) - It will reduce the latency at the cost of lower anonimity of the server - as SMP-server onion address is used in the clients together with public address, this is ok. If you deploy SMP-server with onion-only address, you may want to keep standard configuration instead.
     SOCKSPort 0
     HiddenServiceNonAnonymousMode 1
     HiddenServiceSingleHopMode 1
     # smp-server hidden service host directory and port mappings
     HiddenServiceDir /var/lib/tor/simplex-smp/
     HiddenServicePort 5223 localhost:5223
     ```

   - Create directories:

     ```sh
     mkdir /var/lib/tor/simplex-smp/ && chown debian-tor:debian-tor /var/lib/tor/simplex-smp/ && chmod 700 /var/lib/tor/simplex-smp/
     ```

3. Start tor:

   Enable `systemd` service and start tor. Offical `tor` is a bit flaky on the first start and may not create onion host address, so we're restarting it just in case.

   ```sh
   systemctl enable --now tor && systemctl restart tor
   ```

4. Display onion host:

   Execute the following command to display your onion host address:

   ```sh
   cat /var/lib/tor/simplex-smp/hostname
   ```

### SOCKS port for SMP PROXY

SMP-server versions starting from `v5.8.0-beta.0` can be configured to PROXY smp servers available exclusively through [Tor](https://www.torproject.org) network to be accessible to the clients that do not use Tor. Run the following commands as `root` user.

1. Install tor as described in the [previous section](#installation-for-onion-address).

2. Execute the following command to creatae a new Tor daemon instance:

   ```sh
   tor-instance-create tor2
   ```

3. Open the `tor2` configuration and replace its content with the following lines:

   ```sh
   vim /etc/tor/instances/tor2/torrc
   ```

   ```sh
   # Log tor to systemd daemon
   Log notice syslog
   # Listen to local 9050 port for socks proxy
   SocksPort 9050
   ```

3. Enable service at startup and start the daemon:

   ```sh
   systemctl enable --now tor@tor2
   ```

   You can check `tor2` logs with the following command:

   ```sh
   journalctl -u tor@tor2
   ```

4. After [server initialization](#configuration), configure the `PROXY` section like so:

   ```ini
   ...
   [PROXY]
   socks_proxy: 127.0.0.1:9050
   own_server_domains: <your domain suffixes if using `log_stats: on`>
   ...
   ```

## Server information page

SMP-server versions starting from `v5.8.0` can be configured to serve Web page with server information that can include admin info, server info, provider info, etc. Run the following commands as `root` user.

1. Create folder to store webserver static files and assign correct permissions:

   ```sh
   mkdir -p /var/www/smp-server-web && chown smp:smp /var/www/smp-server-web
   ```

2. Add the following to your smp-server configuration (please modify fields in [INFORMATION] section to include relevant information):

   ```sh
   vim /etc/opt/simplex/smp-server.ini
   ```

   ```ini
   [WEB]
   static_path: /var/www/smp-server-web

   [INFORMATION]
   # AGPLv3 license requires that you make any source code modifications
   # available to the end users of the server.
   # LICENSE: https://github.com/simplex-chat/simplexmq/blob/stable/LICENSE
   # Include correct source code URI in case the server source code is modified in any way.
   # If any other information fields are present, source code property also MUST be present.

   source_code: https://github.com/simplex-chat/simplexmq

   # Declaring all below information is optional, any of these fields can be omitted.

   # Server usage conditions and amendments.
   # It is recommended to use standard conditions with any amendments in a separate document.
   # usage_conditions: https://github.com/simplex-chat/simplex-chat/blob/stable/PRIVACY.md
   # condition_amendments: link

   # Server location and operator.
   server_country: <YOUR_SERVER_LOCATION>
   operator: <YOUR_NAME>
   operator_country: <YOUR_LOCATION>
   website: <WEBSITE_IF_AVAILABLE>
  
   # Administrative contacts.
   #admin_simplex: SimpleX address
   admin_email: <EMAIL>
   # admin_pgp:
   # admin_pgp_fingerprint:

   # Contacts for complaints and feedback.
   # complaints_simplex: SimpleX address
   complaints_email: <COMPLAINTS_EMAIL>
   # complaints_pgp:
   # complaints_pgp_fingerprint:

   # Hosting provider.
   hosting: <HOSTING_PROVIDER_NAME>
   hosting_country: <HOSTING_PROVIDER_LOCATION> 
   ```

3. Install the webserver. For easy deployment we'll describe the installtion process of [Caddy](https://caddyserver.com) webserver on Ubuntu server:

   1. Install the packages:

      ```sh
      sudo apt install -y debian-keyring debian-archive-keyring apt-transport-https curl
      ```

   2. Install caddy gpg key for repository:

      ```sh
      curl -1sLf 'https://dl.cloudsmith.io/public/caddy/stable/gpg.key' | sudo gpg --dearmor -o /usr/share/keyrings/caddy-stable-archive-keyring.gpg
      ```

   3. Install Caddy repository:

      ```sh
      curl -1sLf 'https://dl.cloudsmith.io/public/caddy/stable/debian.deb.txt' | sudo tee /etc/apt/sources.list.d/caddy-stable.list
      ```

   4. Install Caddy:

      ```sh
      sudo apt update && sudo apt install caddy
      ```

   [Full Caddy instllation instructions](https://caddyserver.com/docs/install)

4. Replace Caddy configuration with the following (don't forget to replace `<YOUR_DOMAIN>`):

   ```sh
   vim /etc/caddy/Caddyfile
   ```

   ```caddy
   <YOUR_DOMAIN> {
     root * /var/www/simplex
     file_server
   }
   ```

5. Enable and start Caddy service:

   ```sh
   systemctl enable --now caddy
   ```

6. Upgrade your smp-server to latest version - [Updating your smp server](#updating-your-smp-server)

7. Access the webpage you've deployed from your browser. You should see the smp-server information that you've provided in your ini file.

## Documentation

All necessary files for `smp-server` are located in `/etc/opt/simplex/` folder.

Stored messages, connections, statistics and server log are located in `/var/opt/simplex/` folder.

### SMP server address

SMP server address has the following format:

```
smp://<fingerprint>[:<password>]@<public_hostname>[,<onion_hostname>]
```

- `<fingerprint>`

  Your `smp-server` fingerprint of certificate. You can check your certificate fingerprint in `/etc/opt/simplex/fingerprint`.

- **optional** `<password>`

  Your configured password of `smp-server`. You can check your configured pasword in `/etc/opt/simplex/smp-server.ini`, under `[AUTH]` section in `create_password:` field.

- `<public_hostname>`, **optional** `<onion_hostname>`

  Your configured hostname(s) of `smp-server`. You can check your configured hosts in `/etc/opt/simplex/smp-server.ini`, under `[TRANSPORT]` section in `host:` field.

### Systemd commands

To start `smp-server` on host boot, run:

```sh
sudo systemctl enable smp-server.service

Created symlink /etc/systemd/system/multi-user.target.wants/smp-server.service → /etc/systemd/system/smp-server.service.
```

To start `smp-server`, run:

```sh
sudo systemctl start smp-server.service
```

To check status of `smp-server`, run:

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

To stop `smp-server`, run:

```sh
sudo systemctl stop smp-server.service
```

To check tail of `smp-server` log, run:

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

### Monitoring

You can enable `smp-server` statistics for `Grafana` dashboard by setting value `on` in `/etc/opt/simplex/smp-server.ini`, under `[STORE_LOG]` section in `log_stats:` field.

Logs will be stored as `csv` file in `/var/opt/simplex/smp-server-stats.daily.log`. Fields for the `csv` file are:

```sh
fromTime,qCreated,qSecured,qDeleted,msgSent,msgRecv,dayMsgQueues,weekMsgQueues,monthMsgQueues,msgSentNtf,msgRecvNtf,dayCountNtf,weekCountNtf,monthCountNtf,qCount,msgCount,msgExpired,qDeletedNew,qDeletedSecured,pRelays_pRequests,pRelays_pSuccesses,pRelays_pErrorsConnect,pRelays_pErrorsCompat,pRelays_pErrorsOther,pRelaysOwn_pRequests,pRelaysOwn_pSuccesses,pRelaysOwn_pErrorsConnect,pRelaysOwn_pErrorsCompat,pRelaysOwn_pErrorsOther,pMsgFwds_pRequests,pMsgFwds_pSuccesses,pMsgFwds_pErrorsConnect,pMsgFwds_pErrorsCompat,pMsgFwds_pErrorsOther,pMsgFwdsOwn_pRequests,pMsgFwdsOwn_pSuccesses,pMsgFwdsOwn_pErrorsConnect,pMsgFwdsOwn_pErrorsCompat,pMsgFwdsOwn_pErrorsOther,pMsgFwdsRecv,qSub,qSubAuth,qSubDuplicate,qSubProhibited,msgSentAuth,msgSentQuota,msgSentLarge
```

| Field number  | Field name                   | Field Description          |
| ------------- | ---------------------------- | -------------------------- |
| 1             | `fromTime`                   | Date of statistics         |
| Messaging queue:                                                          |
| 2             | `qCreated`                   | Created                    |
| 3             | `qSecured`                   | Established                |
| 4             | `qDeleted`                   | Deleted                    |
| Messages:                                                                 |
| 5             | `msgSent`                    | Sent                       |
| 6             | `msgRecv`                    | Received                   |
| 7             | `dayMsgQueues`               | Active queues in a day     |
| 8             | `weekMsgQueues`              | Active queues in a week    |
| 9             | `monthMsgQueues`             | Active queues in a month   |
| Messages with "notification" flag                                         |
| 10            | `msgSentNtf`                 | Sent                       |
| 11            | `msgRecvNtf`                 | Received                   |
| 12            | `dayCountNtf`                | Active queues in a day     |
| 13            | `weekCountNtf`               | Active queues in a week    |
| 14            | `monthCountNtf`              | Active queues in a month   |
| Additional statistics:                                                    |
| 15            | `qCount`                     | Stored queues              |
| 16            | `msgCount`                   | Stored messages            |
| 17            | `msgExpired`                 | Expired messages           |
| 18            | `qDeletedNew`                | New deleted queues         |
| 19            | `qDeletedSecured`            | Secured deleted queues     |
| Requested sessions with all relays:                                       |
| 20            | `pRelays_pRequests`          | - requests                 |
| 21            | `pRelays_pSuccesses`         | - successes                |
| 22            | `pRelays_pErrorsConnect`     | - connection errors        |
| 23            | `pRelays_pErrorsCompat`      | - compatability errors     |
| 24            | `pRelays_pErrorsOther`       | - other errors             |
| Requested sessions with own relays:                                       |
| 25            | `pRelaysOwn_pRequests`       | - requests                 |
| 26            | `pRelaysOwn_pSuccesses`      | - successes                |
| 27            | `pRelaysOwn_pErrorsConnect`  | - connection errors        |
| 28            | `pRelaysOwn_pErrorsCompat`   | - compatability errors     |
| 29            | `pRelaysOwn_pErrorsOther`    | - other errors             |
| Message forwards to all relays:                                           |
| 30            | `pMsgFwds_pRequests`         | - requests                 |
| 31            | `pMsgFwds_pSuccesses`        | - successes                |
| 32            | `pMsgFwds_pErrorsConnect`    | - connection errors        |
| 33            | `pMsgFwds_pErrorsCompat`     | - compatability errors     |
| 34            | `pMsgFwds_pErrorsOther`      | - other errors             |
| Message forward to own relays:                                            |
| 35            | `pMsgFwdsOwn_pRequests`      | - requests                 |
| 36            | `pMsgFwdsOwn_pSuccesses`     | - successes                |
| 37            | `pMsgFwdsOwn_pErrorsConnect` | - connection errors        |
| 38            | `pMsgFwdsOwn_pErrorsCompat`  | - compatability errors     |
| 39            | `pMsgFwdsOwn_pErrorsOther`   | - other errors             |
| Received message forwards:                                                |
| 40            | `pMsgFwdsRecv`               |                            |
| Message queue subscribtion errors:                                        |
| 41            | `qSub`                       | All                        |
| 42            | `qSubAuth`                   | Authentication erorrs      |
| 43            | `qSubDuplicate`              | Duplicate SUB errors       |
| 44            | `qSubProhibited`             | Prohibited SUB errors      |
| Message errors:                                                           |
| 45            | `msgSentAuth`                | Authentication errors      |
| 46            | `msgSentQuota`               | Quota errors               |
| 47            | `msgSentLarge`               | Large message errors       |

To import `csv` to `Grafana` one should:

1. Install Grafana plugin: [Grafana - CSV datasource](https://grafana.com/grafana/plugins/marcusolsson-csv-datasource/)

2. Allow local mode by appending following:

  ```sh
  [plugin.marcusolsson-csv-datasource]
  allow_local_mode = true
  ```

  ... to `/etc/grafana/grafana.ini`

3. Add a CSV data source:

  - In the side menu, click the Configuration tab (cog icon)
  - Click Add data source in the top-right corner of the Data Sources tab
  - Enter "CSV" in the search box to find the CSV data source
  - Click the search result that says "CSV"
  - In URL, enter a file that points to CSV content

4. You're done! You should be able to create your own dashboard with statistics.

For further documentation, see: [CSV Data Source for Grafana - Documentation](https://grafana.github.io/grafana-csv-datasource/)

## Updating your SMP server

To update your smp-server to latest version, choose your installation method and follow the steps:

   - Manual deployment
     1. Stop the server:
        ```sh
        sudo systemctl stop smp-server
        ```
     2. Update the binary:
        ```sh
         curl -L https://github.com/simplex-chat/simplexmq/releases/latest/download/smp-server-ubuntu-20_04-x86-64 -o /usr/local/bin/smp-server && chmod +x /usr/local/bin/smp-server
        ```
     3. Start the server:
        ```sh
        sudo systemctl start smp-server
        ```

   - [Offical installation script](https://github.com/simplex-chat/simplexmq#using-installation-script)
     1. Execute the followin command:
        ```sh
        sudo simplex-servers-update
        ```
     2. Done!

   - [Docker container](https://github.com/simplex-chat/simplexmq#using-docker)
     1. Stop and remove the container:
        ```sh
        docker rm $(docker stop $(docker ps -a -q --filter ancestor=simplexchat/smp-server --format="\{\{.ID\}\}"))
        ```
     2. Pull latest image:
        ```sh
        docker pull simplexchat/smp-server:latest
        ```
     3. Start new container:
        ```sh
        docker run -d \
          -p 5223:5223 \
          -v $HOME/simplex/smp/config:/etc/opt/simplex:z \
          -v $HOME/simplex/smp/logs:/var/opt/simplex:z \
          simplexchat/smp-server:latest
        ```

   - [Linode Marketplace](https://www.linode.com/marketplace/apps/simplex-chat/simplex-chat/)
     1. Pull latest images:
        ```sh
        docker-compose --project-directory /etc/docker/compose/simplex pull
        ```
     2. Restart the containers:
        ```sh
        docker-compose --project-directory /etc/docker/compose/simplex up -d --remove-orphans
        ```
     3. Remove obsolete images:
        ```sh
        docker image prune
        ```

## Configuring the app to use the server

To configure the app to use your messaging server copy it's full address, including password, and add it to the app. You have an option to use your server together with preset servers or without them - you can remove or disable them.

It is also possible to share the address of your server with your friends by letting them scan QR code from server settings - it will include server password, so they will be able to receive messages via your server as well.

_Please note_: you need SMP server version 4.0 to have password support. If you already have a deployed server, you can add password by adding it to server INI file.

<img src="./server_config_1.png" width="288"> &nbsp;&nbsp; <img src="./server_config_2.png" width="288"> &nbsp;&nbsp; <img src="./server_config_3.png" width="288">
