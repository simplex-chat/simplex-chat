<img align="right" src="images/logo.svg" alt="SimpleX logo" height="90">

# SimpleX chat

## Private, secure, decentralized

[![GitHub build](https://github.com/simplex-chat/simplex-chat/workflows/build/badge.svg)](https://github.com/simplex-chat/simplex-chat/actions?query=workflow%3Abuild)
[![GitHub release](https://img.shields.io/github/v/release/simplex-chat/simplex-chat)](https://github.com/simplex-chat/simplex-chat/releases)

The motivation for SimpleX chat is [presented here](./simplex.md).

SimpleX chat prototype is a thin terminal UI on top of [SimpleXMQ](https://github.com/simplex-chat/simplexmq) message broker that uses [SMP protocols](https://github.com/simplex-chat/simplexmq/blob/master/protocol), implementing direct and group messaging and file transfer.

See [simplex.chat](https://simplex.chat) website for chat demo and the explanations of the system and how SMP protocol works.

<!-- TODO update gif -->

![simplex-chat](./images/simplex-chat.gif)

## Table of contents

- [Disclaimer](#disclaimer)
- [Network topology](#network-topology)
- [Current features of the terminal chat](#current-features-of-the-terminal-chat)
- [Installation](#installation)
  - [Download chat client](#download-chat-client)
  - [Build from source](#build-from-source)
    - [Using Docker](#using-docker)
    - [Using Haskell stack](#using-haskell-stack)
- [Usage](#usage)
  - [Running the chat client](#running-the-chat-client)
  - [How to use SimpleX chat](#how-to-use-simplex-chat)
  - [File Transfer and Groups](#file-transfer-and-groups)
  - [Access chat history](#access-chat-history)
- [Roadmap](#roadmap)
- [License](#license)

## Disclaimer

This is WIP implementation of SimpleX chat that implements a new network topology for asynchronous communication combining the advantages and avoiding the disadvantages of federated and P2P networks.

If you expect a software being reliable most of the time and doing something useful, then this is probably not ready for you yet. We do use it for terminal chat though, and it seems to work most of the time - we would really appreciate if you try it and give us your feedback.

**Please note:** The main differentiation of SimpleX network is the approach to internet message routing rather than encryption; for that reason no sufficient attention was paid to either TCP transport level encryption or to E2E encryption protocols - they are implemented in an ad hoc way based on RSA and AES algorithms. See [SMP protocol](https://github.com/simplex-chat/simplexmq/blob/master/protocol/simplex-messaging.md#appendix-a) on TCP transport encryption protocol (AEAD-GCM scheme, with an AES key negotiation based on RSA key hash known to the client in advance) and [this section](https://github.com/simplex-chat/simplexmq/blob/master/rfcs/2021-01-26-crypto.md#e2e-encryption) on E2E encryption protocol (an ad hoc hybrid scheme a la PGP). These protocols will change in a consumer ready version to something more robust.

## Network topology

SimpleX is a decentralized client-server network that uses redundant, disposable nodes to asynchronously pass the messages via message queues, providing receiver and sender anonymity.

Unlike P2P networks, all messages are passed through one or several (for redundancy) servers, that do not even need to have persistence (in fact, the current [SMP server implementation](https://github.com/simplex-chat/simplexmq#smp-server) uses in-memory message storage, persisting only the queue records) - it provides better metadata protection than P2P designs, as no global participant ID is required, and avoids many [problems of P2P networks](https://github.com/simplex-chat/simplex-chat/blob/master/simplex.md#comparison-with-p2p-messaging-protocols).

Unlike federated networks, the participating server nodes do NOT have records of the users, do NOT communicate with each other, do NOT store messages after they are delivered to the recipients, and there is no way to discover the full list of participating servers - it avoids the problem of metadata visibility that federated networks suffer from and better protects the network, as servers do not communicate with each other. Each server node provides unidirectional "dumb pipes" to the users, that do authorization without authentication, having no knowledge of the the users or their contacts. Each queue is assigned two RSA keys - one for receiver and one for sender - and each queue access is authorized with a signature created using a respective key's private counterpart.

The routing of messages relies on the knowledge of client devices how user contacts and groups map at any given moment of time to these disposable queues on server nodes.

## Current features of the terminal chat

- 1-to-1 chat with multiple people in the same terminal window.
- Auto-populated recipient name - just type your messages to reply to the sender once the connection is established.
- Demo SMP server available at `smp1.simplex.im:5223` - you can deploy your own server (`smp-server` executable in [simplexmq](https://github.com/simplex-chat/simplexmq) repo).
- No global identity or any names visible to the server(s), ensuring full privacy of your contacts and conversations.
- E2E encryption, with RSA public key that has to be passed out-of-band (see [How to use SimpleX chat](#how-to-use-simplex-chat)).
- Message signing and verification with automatically generated RSA keys.
- Message integrity validation (via including the digests of the previous messages).
- Authentication of each command/message by SMP servers with automatically generated RSA key pairs.
- TCP transport encryption using SMP transport protocol.

RSA keys are not used as identity, they are randomly generated for each contact.

## Installation

### Download chat client

Download the chat binary for your system from the [latest stable release](https://github.com/simplex-chat/simplex-chat/releases) and make it executable as shown below.

#### Linux and MacOS

```sh
chmod +x <binary>
mv <binary> ~/.local/bin/simplex-chat
```

(or any other preferred location on PATH).

On MacOS you also need to [allow Gatekeeper to run it](https://support.apple.com/en-us/HT202491).

#### Windows

```sh
move <binary> %APPDATA%/local/bin/simplex-chat.exe
```

### Build from source

#### Using Docker

On Linux, you can build the chat executable using [docker build with custom output](https://docs.docker.com/engine/reference/commandline/build/#custom-build-outputs):

```shell
$ git clone git@github.com:simplex-chat/simplex-chat.git
$ cd simplex-chat
$ DOCKER_BUILDKIT=1 docker build --output ~/.local/bin .
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
```

## Usage

### Running the chat client

To start the chat client, run `simplex-chat` from the terminal.

By default, app data directory is created in the home directory (`~/.simplex`, or `%APPDATA%/simplex` on Windows), and two SQLite database files `simplex.chat.db` and `simplex.agent.db` - for chat client and agent respectively - are initialized in it.

To specify a different file path prefix for the database files use `-d` command line option:

```shell
$ simplex-chat -d alice
```

Running above, for example, would create `alice.chat.db` and `alice.agent.db` database files in current directory. Client's database is dependent on specific agent database, so it wouldn't make sense to mix them with different installation's database files.

The default SMP server is `smp1.simplex.im#pLdiGvm0jD1CMblnov6Edd/391OrYsShw+RgdfR0ChA=` (base-64 encoded string after server host is the transport key digest) - it is pre-configured in the app.

If you deployed your own SMP server(s) you can configure client via `-s` option:

```shell
$ simplex-chat -s smp.example.com:5223#KXNE1m2E1m0lm92WGKet9CL6+lO742Vy5G6nsrkvgs8=
```

The base-64 encoded string in server address is the digest of RSA transport handshake key that the server will generate on the first run and output its digest.

You can still talk to people using default or any other server - it only affects the location of the message queue when you initiate the connection (and the reply queue can be on another server, as set by the other party's client).

Run `simplex-chat --help` to see all available options.

### How to use SimpleX chat

Once you have started the chat, you will be prompted to specify your "display name" and "full name" as part of creation of your local user profile. Your display name is just a way for your contacts to refer to you - it is not unique and does not serve as a global identity (duplicate display names are postfixed to be unique on client). Full name is optional and is just another alias to be printed next to your display name.

This diagram shows how to connect and message a contact:

<!-- TODO fix commands in image -->

<div align="center">
  <img align="center" src="images/how-to-use-simplex.svg">
</div>

Once you've set up your local profile, use `/connect` to create a new connection and generate an invitation. The `connect` command will output an invitation. Send this invitation to your contact via any other channel.

The invitation has the format `smp::<server>::<queue_id>::<rsa_public_key_for_this_queue_only>`. The invitation can only be used once and even if this is intercepted, the attacker would not be able to use it to send you the messages via this queue once your contact confirms that the connection is established.

The contact who received the invitation should use `/connect <invitation>` to accept the connection. This establishes the connection, and both parties are notified.

They would then use `@<name> <message>` commands to send messages. One may also press Space or just start typing a message to send a message to the contact that was the last.

Use `/help` in chat to see the list of available commands.

### File Transfer and Groups

New in v0.4 is support for file transfers and groups functionality.

You can initiate file transfer to your contact by running `/file @<contact> <file_path>`, after which he would be able to asynchronously accept it. For checking the status of a file transfer use `/fstatus <file_id>`. See `/help files` for other commands.

To create a group use `/group <group>` command - afterwards you can add your contacts to it and you can exchange messages with `#<group> <message>` commands. See more group related commands with `/help groups`. You can also initialize a file transfer to a group by running `/file #<group> <file_path>`.

### Access chat history

<!-- TODO update -->

SimpleX chat stores all your contacts and conversations in a local database file, making it private and portable by design, fully owned and controlled by you.

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

## Roadmap

The consumer ready system will have these parts implemented:

1. Application level chat protocol. This will allow to separate physical server connection management from logical chat contacts, and to support all common chat functions. Currently in progress in [v4 branch](https://github.com/simplex-chat/simplex-chat/tree/v4).
2. Symmetric groups support in SMP agent protocol, as a foundation for chat groups.
3. SMP queue redundancy and rotation in SMP agent protocol.
4. Message delivery confirmation in SMP agent protocol.
5. Multi-agent/device data synchronization - to use chat on multiple devices.
6. Synchronous streams support in SMP and SMP agent protocols, to support file transfer.
7. Desktop and mobile apps.
8. Scripts for simple SMP server deployment to hosting providers: Linode ([done](https://github.com/simplex-chat/simplexmq#deploy-smp-server-on-linode)), Digital Ocean and Heroku.
9. Public broadcast channels.
10. Optional public contact/group addresses using DNS-based contact addresses (like email) to establish connections, but not using it to route messages - in this way you will keep all your contacts and groups even if you lose the control of the domain.

## License

[AGPL v3](./LICENSE)
