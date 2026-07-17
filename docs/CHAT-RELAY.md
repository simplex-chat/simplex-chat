---
title: Hosting your own Chat Relay
revision: 16.07.2026
---

# Hosting your own Chat Relay

Chat relays are used to deliver channel messages in SimpleX Network. Read more about channels in this [whitepaper](https://github.com/simplex-chat/simplex-chat/blob/master/docs/protocol/channels-overview.md) and this [blog post](https://simplex.chat/blog/20260430-simplex-channels-v6-5-consortium-crowdfunding-freedom-of-speech.html).

A chat relay is the SimpleX Chat CLI (`simplex-chat`) running in relay mode (`--relay`). It has its own profile (a display name and a picture), its own address, and in addition to delivering messages, it can generate data for web previews of the channels it delivers.

This guide explains how to set up a chat relay on a Linux server, how to run it, and (optionally) how to configure [Caddy](https://caddyserver.com) to serve data for channel web previews.

> **Please note**: This guide applies only to SimpleX Chat v7.0.0-beta.4 and later.

## Table of Contents

- [Install the CLI](#install-the-cli)
- [Run the relay](#run-the-relay)
   - [Relay options](#relay-options)
   - [Get the relay address](#get-the-relay-address)
   - [Run relay commands](#run-relay-commands)
- [Channel web previews](#channel-web-previews)
   - [Relay web options](#relay-web-options)
   - [Serve the previews with Caddy](#serve-the-previews-with-caddy)
   - [Reload CORS automatically](#reload-cors-automatically)
   - [Verify](#verify)

## Install the CLI

The relay is the standard `simplex-chat` CLI binary. Install or update it with the install script:

```sh
curl -o- https://raw.githubusercontent.com/simplex-chat/simplex-chat/stable/install.sh | bash
```

Other options (manual binary download, building from source) are in the [CLI guide](./CLI.md#download-chat-client).

Copy the installed `simplex-chat` binary to `/usr/local/bin/simplex-chat-relay`. The guide uses that name so the relay is separate from any interactive `simplex-chat` you also run on the server.

Create a dedicated user for the relay (called `relay` below), so it does not run as root and keeps its database in one place:

```sh
sudo useradd -m relay
```

The `useradd -m` flag creates its home directory `/home/relay`, where the guide keeps the database and picture. Run the relay commands (the `-e` commands below) as this user, for example with `sudo -u relay ...`; run the `systemd` and Caddy steps as root.

## Run the relay

Run the relay as a `systemd` service. With `--headless` it starts without any interactive prompts. It creates its profile and address on the first start, and writes its output to the journal.

Create a run script `/usr/local/bin/relay-run`:

```sh
#!/bin/sh
exec /usr/local/bin/simplex-chat-relay \
  --relay \
  --headless \
  --user-display-name "My Relay" \
  --user-image-file /home/relay/avatar.png \
  -d /home/relay/relay
```

```sh
chmod +x /usr/local/bin/relay-run
```

Create `/etc/systemd/system/simplex-relay.service`:

```ini
[Unit]
Description=SimpleX Chat relay
After=network.target

[Service]
User=relay
ExecStart=/usr/local/bin/relay-run
Restart=always
StandardInput=null

[Install]
WantedBy=multi-user.target
```

Enable and start it:

```sh
systemctl daemon-reload
systemctl enable --now simplex-relay
```

The first start creates the relay profile (with the given name and picture) and its address; later starts reuse them. Both are written to the journal:

```
Current user: My Relay
Chat relay address is created:
https://smp4.simplex.im/r#73iEnnvCqPTVGArCAWUcRaj5hxRb7TbPCSZ2JY2VjCQ
```

### Relay options

| Option | Purpose |
| --- | --- |
| `--relay` | Run as a chat relay. Required. |
| `--headless` | Don't ask interactive questions; create the profile and address automatically. On first start it also needs `--user-display-name`. |
| `--user-display-name NAME` | The relay's display name. Creates the profile on first start; on later starts it must match the existing profile. |
| `--user-image-file FILE` | The relay's picture, from a `.png`, `.jpg` or `.jpeg` file. Applied **only when the profile is created**; ignored afterwards. |
| `--relay-address-server SERVER` | Create the relay address on a specific SMP server, e.g. `smp://<fingerprint>@smp.example.com`. By default a preset server is used. Requires `--relay`. |

### Get the relay address

The address is created and logged on the first start. Read it from the journal at any time:

```sh
journalctl -u simplex-relay | grep -A1 "address is created"
```

### Run relay commands

The service runs headless, so there is no attached terminal to type into. To run a one-off command, stop the service, run the command against the relay's database with `-e`, then start it again. For example, to change the picture (`--user-image-file` only sets it when the profile is first created):

```sh
systemctl stop simplex-relay
simplex-chat-relay -d /home/relay/relay -e "/set profile image file /home/relay/new-avatar.png"
systemctl start simplex-relay
```

## Channel web previews

Chat relays can render recent messages of its public channels as JSON files, which can be served over HTTPS using a web server to create channel web previews. This is optional.

### Relay web options

Add these to the run script (`--relay-web-domain` and `--relay-web-dir` must be given together):

```sh
  --relay-web-domain relay.example.com \
  --relay-web-dir /var/www/relay-web-channels/channel \
  --relay-web-cors-file /var/www/relay-web-channels/cors.conf \
  --relay-web-interval 30 \
```

| Option | Purpose |
| --- | --- |
| `--relay-web-domain DOMAIN` | Domain the previews are served from. |
| `--relay-web-dir DIR` | Directory the relay writes channel JSON files to. |
| `--relay-web-cors-file FILE` | File the relay writes the generated Caddy CORS config to. |
| `--relay-web-interval SECONDS` | How often previews are regenerated (default `300`). |
| `--relay-web-item-count COUNT` | Recent messages per channel preview (default `50`). |

Create the web directory, owned by the relay user:

```sh
mkdir -p /var/www/relay-web-channels/channel
chmod 0755 /var/www/relay-web-channels
chown -R relay:relay /var/www/relay-web-channels
```

Restart the relay so the new flags take effect:

```sh
systemctl restart simplex-relay
```

### Serve the previews with Caddy

This section uses [Caddy](https://caddyserver.com) as the web server. Install it (Debian/Ubuntu):

```sh
sudo apt install -y debian-keyring debian-archive-keyring apt-transport-https curl &&\
curl -1sLf 'https://dl.cloudsmith.io/public/caddy/stable/gpg.key' | sudo gpg --dearmor -o /usr/share/keyrings/caddy-stable-archive-keyring.gpg &&\
curl -1sLf 'https://dl.cloudsmith.io/public/caddy/stable/debian.deb.txt' | sudo tee /etc/apt/sources.list.d/caddy-stable.list &&\
sudo apt update && sudo apt install caddy
```

The relay writes files to `/var/www/relay-web-channels/channel/<id>.json`. Serve them, and import the relay's generated CORS rules, in your `Caddyfile`:

```
relay.example.com {
  encode zstd gzip

  handle /channel/* {
    root * /var/www/relay-web-channels    # files resolve to .../channel/<id>.json
    file_server
    import /etc/caddy/simplex-cors.conf
  }
}
```

Keep `root` at the parent directory with a non-stripping `handle`. That is what makes `/channel/<id>.json` resolve to `.../channel/<id>.json`. Do not point `root` at the `channel` subdirectory: the relay's generated CORS matchers are `/channel/*.json`, which only match when the prefix is kept.

```sh
touch /etc/caddy/simplex-cors.conf      # so the import doesn't fail before the first write
usermod -aG relay caddy                 # let caddy read the relay user's files
systemctl restart caddy                 # restart (not reload) to pick up the group
```

### Reload CORS automatically

The relay updates its CORS file as channels change. Copy it into Caddy's config and reload Caddy whenever it changes.

Create `/usr/local/bin/simplex-cors-sync.sh`:

```sh
#!/bin/sh
set -eu
SRC=/var/www/relay-web-channels/cors.conf
DST=/etc/caddy/simplex-cors.conf
[ -f "$SRC" ] || exit 0
cmp -s "$SRC" "$DST" 2>/dev/null && exit 0
install -m 0644 "$SRC" "$DST"
systemctl reload caddy
logger -t simplex-cors "reloaded caddy"
```

Create `/etc/systemd/system/simplex-cors-sync.service`:

```ini
[Unit]
Description=Sync SimpleX relay CORS config to Caddy
StartLimitIntervalSec=30
StartLimitBurst=10
[Service]
Type=oneshot
ExecStartPre=/bin/sleep 2
ExecStart=/usr/local/bin/simplex-cors-sync.sh
```

Create `/etc/systemd/system/simplex-cors-sync.path` to run the service whenever the relay's CORS file changes:

```ini
[Unit]
Description=Watch SimpleX relay CORS config
After=caddy.service
[Path]
PathChanged=/var/www/relay-web-channels/cors.conf
Unit=simplex-cors-sync.service
[Install]
WantedBy=multi-user.target
```

Enable it:

```sh
chmod +x /usr/local/bin/simplex-cors-sync.sh
systemctl daemon-reload
systemctl enable --now simplex-cors-sync.path
```

### Verify

```sh
systemctl status simplex-cors-sync.path              # active (waiting)
ls /var/www/relay-web-channels/channel               # a JSON file appears once a public channel renders
curl -sI https://relay.example.com/channel/<id>.json | grep -i access-control
```

The `curl` should return `access-control-*` headers, and the channel link should open a web preview in a browser.
