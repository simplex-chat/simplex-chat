---
layout: layouts/article.html
title: "SimpleX File Transfer Protocol - a new protocol for sending large files efficiently, privately and securely."
date: 2023-03-01
preview: CLI and relays implementing the new XFTP protocol are released - you can use them now!
image: images/20230301-xftp.jpg
permalink: "/blog/20230301-simplex-file-transfer-protocol.html"
---

# SimpleX File Transfer Protocol – a new protocol for sending large files efficiently, privately and securely.

**Published:** Mar 1, 2023

- [Quick start: how to send a file using XFTP CLI](#zap-quick-start-how-to-send-a-file-using-xftp-cli)
- [What's the problem](#whats-the-problem)
- [Why didn't we just use some existing solution?](#why-didnt-we-just-use-some-existing-solution)
- [What is XFTP and how does it work?](#what-is-xftp-and-how-does-it-work)
- [What is next?](#what-is-next)

## :zap: Quick start: how to send a file using XFTP CLI

1. Download XFTP binary for Linux from the release or install it with this command: ... You can also compile it from source for any platform.

2. Choose file of any size up to 1Gb.

3. To send the file to one recipient:

```
xftp send filename.ext
```

You can also send the file that can be received by multiple recipients using `-n` option:

```
xftp send filename.ext -n 10
```

Executing this command will encrypt and upload you file fragments to multiple XFTP relays. Once it is completed (it takes several minutes for 1gb file), the CLI will create as many file descriptions as you specified, plus one more for you that you can use to delete all file fragments from the relays before they expire (currently it is set to expire in 48 hours).

File descriptions will be in the subfolder of the current working directory with the name `filename.ext.xftp` (CLI allows to change the folder for file descriptions via options), with the names `rcvABCD.xftp` for each recipient and `snd.xftp.private` for you. If you also want to be able to download the file, you need one more file description for the recipient.

The number of recipients does not change the traffic for you, and each person will be downloading file fragments via different addresses - each file description can be used only once (but if download fails because of a bad network connection it can be retried).

4. Pass each recipient a different file description securely, e.g. send it as a file via SimpleX Chat. For example, for 1gb file file description will have a size of ~36kb – you can think about it as a very large link to receive the file.

5. To receive the file, the recipient has to execute this command:

```
xftp recv rcvABCD.xftp
```

This will download file fragments from all relays specified in the file description, and decrypt the file into the default Downloads folder (or you can specify another location via CLI options). Once the file is fully downloaded, it won't be possible to use this file description again - CLI will offer removing it.

6. The sender can delete the file from the server before it expires with this command:

```
xftp del ./filename.ext/snd.xftp.private
```

## What's the problem?

If you are using SimpleX Chat apps you know that support of sending files and images is not very good, and sending videos and large files is simply impossible. There are currently these problems:

- the sender has to be online for file transfer to complete, once it was confirmed by the recipient.
- when the file is sent to the group, the sender will have to transfer it separately to each member, creatin a lot of traffic.
- the file transfer is slow, as it is sent in small chunks - approximately 16kb per message.

As a result, we limited the supported size of files in the app to 8mb. Even for supported files, it is quite inefficient for sending any files to large groups.

## Why didn't we just use some existing solution?

We really hoped to find some existing open-source solution that we would integrate in SimpleX Chat.

We decided not to use torrent-like or any other P2P solutions because of their lack of privacy, challenging legality in some jurisdictions and, in many cases, because they are inefficient in groups.

We reviewed several S3-compatible solutions (e.g., [minio](https://github.com/minio/minio), [garage](https://git.deuxfleurs.fr/Deuxfleurs/garage), [SeaweedFS](https://github.com/seaweedfs/seaweedfs)), but they all require the development of a separate service layer, making them unusable as standalone serveces and harder to deploy for the users who want to self-host the file transfer service. As a side note, the solution that we developed can still be composed with S3-compatible storage for higher capacity servers with some privacy/efficiency trade-offs.

We also looked at a few independent implementations of file sharing, with some ad-hoc protocols (e.g., [ceph](https://github.com/ceph/ceph) and [lufi](https://framagit.org/fiat-tux/hat-softwares/lufi)), but neither seemed sufficiently mature, and also not as private as we would like.

So after a lot of searching we decided to design and implement a new protocol for file transfers, that both solved the problems above, and provided a higher level of metadata privacy than any other file transfer solution has.

## What is XFTP and how does it work?

```
           Sender                      Internet                XFTP relays             Internet         Recipient
----------------------------   |   -----------------   |   -------------------   |   ------------   |   ---------- 
                               |                       |                         |                  |
                               |                       |   (can be self-hosted)  |                  |
                               |                       |        +---------+      |                  |
                  chunk 1     ----- HTTP2 over TLS ------       |  XFTP   |     ---- HTTP2 / TLS -----   chunk 1
                |---> SimpleX File Transfer Protocol (XFTP) --> |  Relay  | --->        XFTP         ------------->|
                |             ---------------------------       +---------+     ----------------------             |
                |              |                       |                         |                  |              | 
                |              |                       |                         |                  |              v
          +----------+         |                       |        +---------+      |                  |        +-------------+
          | Sending  | ch. 2  ------- HTTP2 / TLS -------       |  XFTP   |     ---- HTTP2 / TLS ----  ch. 2 |  Receiving  |
file ---> |   XFTP   | ------>           XFTP            ---->  |  Relay  | --->        XFTP         ------> |     XFTP    | ---> file
          |  Client  |        ---------------------------       +---------+     ----------------------       |    Client   |
          +----------+         |                       |                         |                  |        +-------------+
                |              |                       |                         |                  |              ^
                |              |                       |        +---------+      |                  |              |
                |             ------- HTTP2 / TLS -------       |  XFTP   |     ---- HTTP2 / TLS ----              |
                |------------->           XFTP           ---->  |  Relay  | --->        XFTP         ------------->|
                   chunk N    ---------------------------       +---------+     ---------------------    chunk N
                               |                       |   (store file chunks)   |                  |
                               |                       |                         |                  |
                               |                       |                         |                  |
```

XFTP stands for SimpleX File Transfer Protocol. Its design is based on the same ideas and has some of the qualities of SimpleX Messaging Protocol, that is used in SimpleX Chat:

- recipient cannot see sender's IP address, as the file fragments (chunks) are temporarily stored on multiple XFTP relays.
- file can be sent asynchronously, without requiring the sender to be online for file to be received.
- there is no network of peers that can observe this transfer - sender chooses which XFTP relays to use, and can self-host their own.
- XFTP relays do not have any file metadata - they only see individual chunks, with access to each chunk authorised with anonymous credentials that are random per chunk.
- chunks have one of the sizes allowed by the servers - currently we allow 256kb, 1mb and 4mb chunks, so if you send, say 1gb file, to XFTP relays it will look indistinguishable from sending many small files, and they would only know that chunks are sent by the same user only via the transport information, but none of the relays will see all chunks. Also, once this feature is available in mobile apps you can use transport isolation per chunk, when each file fragment will be uploaded via a separate TCP connection (and Tor circuit, if you use Tor) – the CLI we released does not yet support per-chunk transport isolation.
- each chunk can be downloaded by multiple recipients, but each recipient uses their own key and chunk ID to authorise access, and the chunk is encrypted by a different key agreed via ephemeral DH keys on the way from the server to each recipient. XFTP protocol as a result has the same quality as SMP protocol - there are no identifiers and ciphertext in common between sent and recived traffic inside TLS connection, so even if TLS is compromised, it complicates traffic correlation attacks.
- XFTP protocol also supports redundancy - each file chunk can be sent via multiple relays, and the recipient can choose the one that is availbale. The released CLI does not support redundancy though.
- the file as a whole is encrypted with a random symmetric key.

So, how would any recipient know where to get all these file fragments from and how to put them back together into the original file? Normally, when you send a file via any file-sharing service it provides you a link that you can pass to the recipient. The link allows to download the original file, but it also provides the server a lot of file meta-data, that often includes file name and exact size, and in many cases also file content.

Instead of using a link, XFTP protocol includes a special format for a "file description" - it is a small text file containing the locations, access keys and digests for all chunks, and also an encryption key and digest for the whole file. This file description does not contain the original file name or exact file size, so if it is used after the file fragments expired or removed from XFTP relays, this information is not accessible.

CLI generates a separate file description for each intended recipient - you need to specify how many people you want to receive this file. It is recommended to specify a larger number of recipients to hide the real number of recipients from XFTP relays. Mobile apps, when this protocol is integrated, will do it automatically choosing some large random number of possible recipients, so while the relays will be able to observe how many people actually downloaded the file, they won't know how many intended recipients you actually had - sending to a group of 10 people and to 1 recipient will look the same to the relays.

File description is a security-sensitive document that contains private keys and addresses necessary to receive the file, and also a symmetric key to decrypt the file. So the channel you use to send this file description must be secure from passive attacks (unlike SimpleX Chat invitation links that only need to be secure against active attacks) - e.g., it can be sent via SimpleX Chat. But once the recipient downloaded the file, CLI invalidates the file fragment addresses on relays and the same file description cannot be used again to download the same file.

## What is next?

We've just released and deployed several XFTP relays for you to experiment with (they are hardcoded in the XFTP CLI), and you can deploy your own relay either from [downloadable binary](https://github.com/simplex-chat/simplexmq/releases/tag/v5.0.0-beta.3) or by compiling from source. We've also released XFTP CLI - also available in the same release.

We are currently integrating support for sending large files using this brand-new XFTP protocol into SimpleX Chat clients. v5.0 of SimpleX Chat will already have support for receiving files sent via XFTP protocol (you will be able to send a file description via a SimpleX Chat CLI app in a way that mobile apps will be able to receive it as a normal file), and v5.1 will have support for sending large files from the mobile apps.

We will also publish a formal specification for XFTP protocol and overview of its security qualities. For now you can learn more about protocol design and motivations from this internal [XFTP RFC](https://github.com/simplex-chat/simplexmq/blob/stable/rfcs/2022-12-26-simplex-file-transfer.md).

Using and sending files with the available XFTP CLI will hugely help us stabilizing both the protocol and implementations. What we really like about it is that it is completely independent from SimpleX Chat - you can use it on its own, sending files and passing file descriptions to your contacts via any other messenger - e.g. via Signal, – without this messenger being able to observe that you are in fact sending a large file.

We did not yet decide whether we would be conducting a separate security audit of XFTP, or if we combine it with the next security audit of SimpleX Chat. The latter seems more likely, as XFTP uses the same cryptographic primitives that were reviewed during [SimpleX Chat security assessment by Trail of Bits](./20221108-simplex-chat-v4.2-security-audit-new-website.md) in November 2022.

## SimpleX platform

Some links to answer the most common questions:

[How can SimpleX deliver messages without user identifiers](./20220511-simplex-chat-v2-images-files.md#the-first-messaging-platform-without-user-identifiers).

[What are the risks to have identifiers assigned to the users](./20220711-simplex-chat-v3-released-ios-notifications-audio-video-calls-database-export-import-protocol-improvements.md#why-having-users-identifiers-is-bad-for-the-users).

[Technical details and limitations](https://github.com/simplex-chat/simplex-chat#privacy-technical-details-and-limitations).

[How SimpleX is different from Session, Matrix, Signal, etc.](https://github.com/simplex-chat/simplex-chat/blob/stable/README.md#frequently-asked-questions).

Please also see our [website](https://simplex.chat).

## Help us with donations

Huge thank you to everybody who donated to SimpleX Chat!

We are prioritizing users privacy and security - it would be impossible without your support.

Our pledge to our users is that SimpleX protocols are and will remain open, and in public domain, - so anybody can build the future implementations of the clients and the servers. We are building SimpleX platform based on the same principles as email and web, but much more private and secure.

Your donations help us raise more funds – any amount, even the price of the cup of coffee, makes a big difference for us.

See [this section](https://github.com/simplex-chat/simplex-chat/tree/master#help-us-with-donations) for the ways to donate.

Thank you,

Evgeny

SimpleX Chat founder
