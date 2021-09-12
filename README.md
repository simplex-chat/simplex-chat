<img align="right" src="images/logo.svg" alt="SimpleX logo" height="90">

# SimpleX chat

## Private, secure, decentralized

[![GitHub build](https://github.com/simplex-chat/simplex-chat/workflows/build/badge.svg)](https://github.com/simplex-chat/simplex-chat/actions?query=workflow%3Abuild)
[![GitHub release](https://img.shields.io/github/v/release/simplex-chat/simplex-chat)](https://github.com/simplex-chat/simplex-chat/releases)

> **NEW in v0.4: [groups](#groups) and [sending files](#sending-files)!**

The motivation for SimpleX chat is [presented here](./simplex.md).

SimpleX chat prototype is a thin terminal UI on top of [SimpleXMQ](https://github.com/simplex-chat/simplexmq) message broker that uses [SMP protocols](https://github.com/simplex-chat/simplexmq/blob/master/protocol).

See [simplex.chat](https://simplex.chat) website for chat demo and the explanations of the system and how SMP protocol works.

![simplex-chat](./images/connection.gif)

## Table of contents

- [Disclaimer](#disclaimer)
- [Network topology](#network-topology)
- [Terminal chat features](#terminal-chat-features)
- [Installation](#installation)
  - [Download chat client](#download-chat-client)
  - [Build from source](#build-from-source)
    - [Using Docker](#using-docker)
    - [Using Haskell stack](#using-haskell-stack)
- [Usage](#usage)
  - [Running the chat client](#running-the-chat-client)
  - [How to use SimpleX chat](#how-to-use-simplex-chat)
  - [Groups](#groups)
  - [Sending files](#sending-files)
  - [Access chat history](#access-chat-history)
- [Future roadmap](#future-roadmap)
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

## Terminal chat features

- 1-to-1 chat with multiple people in the same terminal window.
- Group messaging.
- Sending files to contacts and groups.
- Auto-populated recipient name - just type your messages to reply to the sender once the connection is established.
- Demo SMP servers available and pre-configured in the app - or you can [deploy your own server](https://github.com/simplex-chat/simplexmq#using-smp-server-and-smp-agent).
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

By default, app data directory is created in the home directory (`~/.simplex`, or `%APPDATA%/simplex` on Windows), and two SQLite database files `simplex.chat.db` and `simplex.agent.db` are initialized in it.

To specify a different file path prefix for the database files use `-d` command line option:

```shell
$ simplex-chat -d alice
```

Running above, for example, would create `alice.chat.db` and `alice.agent.db` database files in current directory.

Default SMP servers are hosted on Linode (London, UK and Fremont, CA) - they are [pre-configured in the app](https://github.com/simplex-chat/simplex-chat/blob/master/src/Simplex/Chat/Options.hs#L40). Base-64 encoded string after server host is the transport key digest.

If you deployed your own SMP server(s) you can configure client via `-s` option:

```shell
$ simplex-chat -s smp.example.com:5223#KXNE1m2E1m0lm92WGKet9CL6+lO742Vy5G6nsrkvgs8=
```

The base-64 encoded string in server address is the digest of RSA transport handshake key that the server will generate on the first run and output its digest.

You can still talk to people using default or any other server - it only affects the location of the message queue when you initiate the connection (and the reply queue can be on another server, as set by the other party's client).

Run `simplex-chat -h` to see all available options.

### How to use SimpleX chat

Once you have started the chat, you will be prompted to specify your "display name" and an optional "full name" to create a local chat profile. Your display name is an alias for your contacts to refer to you by - it is not unique and does not serve as a global identity. In case different contacts chose the same display name, the chat client adds a numeric suffix to their local display names.

This diagram shows how to connect and message a contact:

<div align="center">
  <img align="center" src="images/how-to-use-simplex.svg">
</div>

Once you've set up your local profile, enter `/c` (for `/connect`) to create a new connection and generate an invitation. Send this invitation to your contact via any other channel.

The invitation has the format `smp::<server>::<queue_id>::<rsa_public_key_for_this_queue_only>`. The invitation can only be used once and even if this is intercepted, the attacker would not be able to use it to send you the messages via this queue once your contact confirms that the connection is established.

The contact who received the invitation should enter `/c <invitation>` to accept the connection. This establishes the connection, and both parties are notified.

They would then use `@<name> <message>` commands to send messages. You may also just start typing a message to send it to the contact that was the last.

Use `/help` in chat to see the list of available commands.

### Groups

To create a group use `/g <group>`, then add contacts to it with `/a <group> <name>`and send messages with `#<group> <message>`. Use `/help groups` for other commands.

![simplex-chat](./images/groups.gif)

> **Please note**: the groups are not stored on any server, they are maintained as a list of members in the app database to whom the messages will be sent.

### Sending files

You can send a file to your contact with `/f @<contact> <file_path>` - the recipient will have to accept it before it is sent. Use `/help files` for other commands.

![simplex-chat](./images/files.gif)

You can send files to a group with `/f #<group> <file_path>`.

### Access chat history

> 🚧 **Section currently out of date - will be updated soon** 🏗

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

## Future roadmap

1. Mobile and desktop apps (in progress).
2. SMP protocol improvements:
    - SMP queue redundancy and rotation.
    - Message delivery confirmation.
    - Support multiple devices.
3. Privacy-preserving identity server for optional DNS-based contact/group addresses to simplify connection and discovery, but not used to deliver messages:
    - keep all your contacts and groups even if you lose the domain.
    - the server doesn't have information about your contacts and groups.
4. Media server to optimize sending large files to groups.
5. Channels server for large groups and broadcast channels.

## License

[AGPL v3](./LICENSE)
