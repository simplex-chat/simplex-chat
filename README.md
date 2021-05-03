# SimpleX chat

[![GitHub build](https://github.com/simplex-chat/simplex-chat/workflows/build/badge.svg)](https://github.com/simplex-chat/simplex-chat/actions?query=workflow%3Abuild)
[![GitHub release](https://img.shields.io/github/v/release/simplex-chat/simplex-chat)](https://github.com/simplex-chat/simplex-chat/releases)

## Federated chat - private, secure, decentralized

See [simplex.chat](https://simplex.chat) website for chat demo and the explanations of the system and how SMP protocol works.

SimpleX chat prototype is a "thin" terminal UI on top of [SimpleXMQ](https://github.com/simplex-chat/simplexmq) message broker, that uses [SMP protocol](https://github.com/simplex-chat/simplexmq/tree/master/protocol) and SMP agent protocol.

These features are implemented:
- 1-to-1 chat with multiple people in the same terminal window.
- auto-populated recipient name - just type your messages to reply to the sender.
- default server is available to use - `smp1.simplex.im:5223` - you can deploy your own server (`smp-server` executable in [simplexmq](https://github.com/simplex-chat/simplexmq) repo).
- no global identity or any names visible to the server(s) - for the full privacy of your contacts and conversations.
- E2E encryption, with RSA public key that has to be passed out-of-band (see below).
- message signing and verification with automatically generated RSA keys.
- message integrity validation (via including the digests of the previous messages).
- authentication of each command/message by SMP servers with automatically generated RSA key pairs.
- TCP transport encryption using SMP transport protocol.

RSA keys are not used as identity, they are randomly generated for each contact. 2048 bit keys are used, it can be changed to 4096-bit in code via [rsaKeySize setting](https://github.com/simplex-chat/simplex-chat/blob/master/apps/dog-food/Main.hs).

The chat client is stable, but it is quite new and unaudited, so you probably should NOT use it yet for high security communications - unless you know what you are doing.

## SimpleX chat roadmap for 2021-22

1. Switch to application level chat protocol. This will allow to separate physical server connection management from logical chat contacts, and to support all common chat functions.
2. SMP queue redundancy and rotation in SMP agent protocol.
3. Symmetric groups support in SMP agent protocol, as a foundation for chat groups.
4. Delivery confirmation in SMP agent protocol.
5. Multi-agent/device data synchronisation - to use chat on multiple devices.
6. Synchronous streams support in SMP and SMP agent protocols, to support file transfer.
7. Terminal chat UI and mobile apps.
8. Scripts for simple SMP server deployment to hosting providers: Linode, Digital Ocean and Heroku.
9. Public broadcast channels.
10. Optional public contact/group addresses using DNS to establish connections, but not using it to send and receive messages - in this way you will keep all your contacts and groups even if you lose the control of the domain.

## Installing the chat client

You can:
- download the executable file built on Github CI
- build it from source

### Downloading the executable chat client

You can download the executable binary file for your platform from the [latest stable release](https://github.com/simplex-chat/simplex-chat/releases) and make it executable:

  - Linux and MacOS: `chmod +x <binary>; mv <binary> ~/.local/bin/dog-food` (or any other preferred location on PATH). On MacOS you need to [allow Gatekeeper to run it](https://support.apple.com/en-us/HT202491).
  - Windows: `move <binary> %APPDATA%\local\bin\dog-food.exe`.

### Building from source

#### Using Docker

On Linux, you can build the chat executable using [docker build with custom output](https://docs.docker.com/engine/reference/commandline/build/#custom-build-outputs):

```shell
$ git clone git@github.com:simplex-chat/simplex-chat.git
$ cd simplex-chat
$ DOCKER_BUILDKIT=1 docker build --output ~/.local/bin .
$ dog-food
```

> **Please note:** If you encounter ``version `GLIBC_2.28' not found`` error, rebuild it with `haskell:8.8.4-stretch` base image (change it in your local [Dockerfile](Dockerfile)).

#### Using Haskell stack

Install [Haskell stack](https://docs.haskellstack.org/en/stable/README/):

```shell
curl -sSL https://get.haskellstack.org/ | sh
```

and build the project:

```shell
$ git clone git@github.com:simplex-chat/simplex-chat.git
$ cd simplex-chat
$ stack install
$ dog-food
```

## Running the chat client

Run `dog-food` (as in "eating your own dog food"), or the downloaded file (if you did not move it to bin folder), to start the chat client.

By default, app data directory is created in the home directory (`~/.simplex`, or `%APPDATA%/simplex` on Windows), and SQLite database file `smp-chat.db` is initialized in it. The default SMP server is `smp1.simplex.im:5223#pLdiGvm0jD1CMblnov6Edd/391OrYsShw+RgdfR0ChA=` (base-64 encoded string after server port is the transport key digest) - it is pre-configured in the app.

To specify a different file path for the chat database use `-d` command line option:

```shell
$ dog-food -d my-chat.db
```

If you deployed your own SMP server you can set client to use it via `-s` option:

```shell
$ dog-food -s smp.example.com:5223#KXNE1m2E1m0lm92WGKet9CL6+lO742Vy5G6nsrkvgs8=
```

The base-64 encoded string in server address is the digest of RSA transport handshake key that the server will generate on the first run and output its digest.

You can still talk to people using default or any other server, it only affects the location of the message queue when you initiate the connection (and the reply queue can be on another server, as set by the other party's client).

Run `dog-food --help` to see all available options.

## Using chat client

Once chat client is started, use `/add <name1>` to create a new connection and generate an invitation to send to your contact via any other communication channel (`<name1>` - is any name you want to use for that contact).

Invitation has format `smp::<server>::<queue_id>::<rsa_public_key_for_this_queue_only>` - this needs to be shared with another party, via any other channel. This invitation can only be used once - even if this is intercepted, the attacker would not be able to use it to send you the messages via this queue once your contact confirms that the connection is established.

The party that received the invitation should use `/connect <name2> <invitation>` to accept the connection (`<name2>` is any name that the accepting party wants to use for you).

For example, if Alice and Bob want to chat, with Alice initiating, Alice would use [in her chat client]:

```
/add bob
```

And then send the generated invitation to Bob out-of-band. Bob then would use [in his chat client]:

```
/connect alice <alice's invitation>
```

They would then use `@<name> <message>` commands to send messages. One may also press Space or just start typing a message to send a message to the contact that was the last.

If your contact is disconnected, restart the chat client - it may happen if you lose internet connection.

Use `/help` in chat to see the list of available commands.

## Accessing chat history

SimpleX chat stores all your contacts and conversations in the local database file, making it private and portable by design, fully owned and controlled by you.

You can search your chat history via SQLite database file:

```
sqlite3 ~/.simplex/smp-chat.db
```

Now you can query `messages` table, for example:

```sql
select * from messages
where conn_alias = cast('alice' as blob)
  and body like '%cats%'
order by internal_id desc;
```

> **Please note:** SQLite foreign key constraints are disabled by default, and must be **[enabled separately for each database connection](https://sqlite.org/foreignkeys.html#fk_enable)**. The latter can be achieved by running `PRAGMA foreign_keys = ON;` command on an open database connection. By running data altering queries without enabling foreign keys prior to that, you may risk putting your database in an inconsistent state.

## License

[AGPL v3](./LICENSE)
