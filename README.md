# simplex-messaging

[![GitHub build](https://github.com/simplex-chat/simplex-messaging/workflows/build/badge.svg)](https://github.com/simplex-chat/simplex-messaging/actions?query=workflow%3Abuild)
[![GitHub release](https://img.shields.io/github/v/release/simplex-chat/simplex-messaging)](https://github.com/simplex-chat/simplex-messaging/releases)

## Federated chat - private, secure, decentralised

See [simplex.chat](https://simplex.chat) website for chat demo and the explanations of the system and how SMP protocol works.

SMP protocol is semi-formally defined [here](https://github.com/simplex-chat/protocol).

Currently only these features are available:
- simple 1-2-1 chat with multiple people in the same terminal window.
- auto-populated recipient name - just type your messages.
- default server is available to play with - `smp.simplex.im:5223` - and you can deploy your own (`smp-server` executable in this repo).
- no global identity or names visible to the server(s) - for the privacy of contacts and conversations.
- E2E encryption, with public key that has to be passed out-of-band (see below)
- authentication of each command/message with automatically generated RSA key pairs, separate for each conversation, the keys are not used as identity (2048 bit keys are used, it can be changed in [code via rsaKeySize setting](https://github.com/simplex-chat/simplex-messaging/blob/master/apps/dog-food/Main.hs))

Limitations/disclaimers:
- no support for chat groups. It is coming in the next major version (i.e., not very soon:)
- no delivery notifications - coming soon
- no TCP transport encryption - coming soon (messages are encrypted e2e though, only random connection IDs and server commands are visible, but not the contents of the message)
- system and protocol security was not audited yet, so you probably should NOT use it yet for high security communications - unless you know what you are doing.

## How to run chat client locally

Install [Haskell stack](https://docs.haskellstack.org/en/stable/README/):

```shell
curl -sSL https://get.haskellstack.org/ | sh
```

and build the project:

```shell
$ git clone git@github.com:simplex-chat/simplex-messaging.git
$ cd simplex-messaging
$ stack install
$ dog-food
```

If you'd prefer to not set up Haskell locally, on Linux you may instead build the chat client executable using [docker build with custom output](https://docs.docker.com/engine/reference/commandline/build/#custom-build-outputs):

```shell
$ git clone git@github.com:simplex-chat/simplex-messaging.git
$ cd simplex-messaging
$ DOCKER_BUILDKIT=1 docker build --output ~/.local/bin .
$ dog-food
```

> **NOTE:** When running chat client executable built with the latter approach, if you encounter ``version `GLIBC_2.28' not found`` error, rebuild it with `haskell:8.8.4-stretch` base image instead (you'd have to change it in your local [Dockerfile](Dockerfile)).

`dog-food` (as in "eating your own dog food" - it is an early prototype) starts chat client with default parameters. By default, SQLite database file is created in the working directory (`smp-chat.db`), and the default SMP server is `smp.simplex.im:5223`.

To specify a different chat database location run:

```shell
$ mkdir ~/simplex
$ dog-food -d ~/simplex/my-chat.db
```

You can deploy your own server and set client to use it via command line option:

```shell
$ dog-food -s smp.example.com:5223
```

You can still talk to people using default or any other server, it only affects the location of the message queue when you initiate the connection (and the reply queue can be on another server, as set by the other party's client).

Run `dog-food --help` to see all available options.

### Using chat client

Once chat client is started, use `/add <name1>` to create a new connection and generate an invitation to send to your contact via any other communication channel (`<name1>` - is any name you want to use for that contact).

Invitation has format `smp::<server>::<queue_id>::<public_key_for_this_queue_only>` - this needs to be shared with another party, via any other chat. It can only be used once - even if this is intercepted, the attacker would not be able to use it to send you the messages via this queue once your contact confirms that the connection is established.

The party that received the invitation should use `/accept <name2> <invitation>` to accept the connection (`<name2>` is any name that the accepting party wants to use for you).

For example, if Alice and Bob want to chat, with Alice initiating, Alice would use [in her chat client]:

```
/add bob
```

And then send the generated invitation to Bob out-of-band. Bob then would use [in his chat client]:

```
/accept alice <alice's invitation>
```

They would then use `@<name> <message>` commands to send messages. One may also press Space or just start typing a message to send a message to the contact that was the last.

If you exit from chat client (or if internet connection is interrupted) you need to use `/chat <name>` to activate conversation with respective contact - it is not resumed automatically (it will improve soon).

Since SMP doesn't use global identity (all account information is managed by clients), you should configure your name to use in invitations for your contacts:

```
/name alice
```

Now Alice's invitations would be generated with her name in it for others' convenience.

Use `/help` in chat to see the list of available commands and their explanation.

### Accessing chat history

You can access your chat history by opening a connection to your SQLite database file and querying `messages` table, for example:

```sql
select * from messages
where conn_alias = cast('alice' as blob)
order by internal_id desc;

select * from messages
where conn_alias = cast('alice' as blob)
and body like '%cats%';
```

> **NOTE:** Beware that SQLite foreign key constraints are disabled by default, and must be **[enabled separately for each database connection](https://sqlite.org/foreignkeys.html#fk_enable)**. The latter can be achieved by running `PRAGMA foreign_keys = ON;` command on an open database connection. By running data altering queries without enabling foreign keys prior to that, you may risk putting your database in an inconsistent state.

## 🚧 [further README not up to date] SMP server demo 🏗

This is a demo implementation of SMP ([simplex messaging protocol](https://github.com/simplex-chat/protocol/blob/master/simplex-messaging.md)) server.

It has a very limited utility (if any) for real applications, as it lacks the following protocol features:

- cryptographic signature verification, instead it simply compares provided "signature" with stored "public key", effectively treating them as plain text passwords.
- there is no transport encryption

Because of these limitations, it is easy to experiment with the protocol logic via telnet.

You can either run it locally or try with the deployed demo server:

```bash
telnet smp.simplex.im 5223
```

## Run locally

[Install stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/) and `stack run`.

## Usage example

Lines you should send are prefixed with `>` character, you should not type them.

Comments are prefixed with `--`, they are not part of transmissions.

`>` on its own means you need to press `return` - telnet should be configured to send it as CRLF.

1. Create simplex message queue:

```telnet
>
> abcd -- correlation ID, any string
>
> NEW 1234 -- 1234 is recipient's key

abcd

IDS QuCLU4YxgS7wcPFA YB4CCATREHkaQcEh -- recipient and sender IDs for the queue
```

2. Sender can send their "key" to the queue:

```telnet
> -- no signature (just press enter)
> bcda -- correlation ID, any string
> YB4CCATREHkaQcEh -- sender ID for the queue
> SEND :key abcd

bcda
YB4CCATREHkaQcEh
OK
```

3. Secure queue with sender's "key"

```telnet
> 1234 -- recipient's "signature" - same as "key" in the demo
> cdab
> QuCLU4YxgS7wcPFA -- recipient ID
> KEY abcd -- "key" provided by sender

cdab
QuCLU4YxgS7wcPFA
OK
```

4. Sender can now send messages to the queue

```telnet
> abcd -- sender's "signature" - same as "key" in the demo
> dabc -- correlation ID
> YB4CCATREHkaQcEh -- sender ID
> SEND :hello

dabc
YB4CCATREHkaQcEh
OK
```

5. Recipient recieves the message and acknowledges it to receive further messages

```telnet

-- no correlation ID for messages delivered without client command
QuCLU4YxgS7wcPFA
MSG ECA3w3ID 2020-10-18T20:19:36.874Z 5
hello
> 1234
> abcd
> QuCLU4YxgS7wcPFA
> ACK

abcd
QuCLU4YxgS7wcPFA
OK
```

## Design

![server design](design/server.svg)
