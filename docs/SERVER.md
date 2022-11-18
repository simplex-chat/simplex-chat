# Hosting your own SMP Server

## Overview

SMP server is the relay server used to pass messages in SimpleX network. SimpleX Chat apps have preset servers (for mobile apps these are smp8, smp9 and smp10.simplex.im), but you can easily change app configuration to use other servers.

SimpleX clients only determine which server is used to receive the messages, separately for each contact (or group connection with a group member), and these servers are only temporary, as the delivery address can change.

_Please note_: when you change the servers in the app configuration, it only affects which server will be used for the new contacts, the existing contacts will not automatically move to the new servers, but you can move them manually using BETA feature ["Change receiving address"](../blog/20221108-simplex-chat-v4.2-security-audit-new-website.md#change-your-delivery-address-beta) – it will be automated soon.

## SMP Server

### Installation

Can link to the existing documents

- Compiling from source
- Using pre-compiled binaries
- Compile in docker container
- Installing via Linode StackScript

### Running the server

systemd commands, how to configure systemd

### Monitoring the server

### Server settings

- password (on by default)
- store log

Include step-by-step description of the new interactive mode

Include supported command line parameters and scripted execution

## Using the server

### Server address

SMP server address has the following format:

```
smp://<fingerprint>[:<password>]@hostname1,hostname2
```

Explain each component of the address and what it does.

### Configuring the app to use the server

TODO Inlcude screenshot of the new UI
