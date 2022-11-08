---
layout: layouts/article.html
title: "SimpleX announces SimpleX Chat v0.4"
date: 2021-09-14
preview: Terminal app now supports groups and file transfers.
permalink: "/blog/20210914-simplex-chat-v0.4-released.html"
---

# SimpleX announces SimpleX Chat v0.4

**Published:** Sep 14, 2021

## Open-source decentralized chat that uses privacy-preserving message routing protocol

We are building a new platform for distributed Internet applications where privacy of the messages _and_ the network matter. [SimpleX Chat](https://github.com/simplex-chat/simplex-chat) is our first application, a chat application built on the SimpleX platform that serves as an example of the power of the platform and as a reference application.

## What is SimpleX?

We recognised that there is currently no messaging application which respects user privacy and guarantees metadata privacy -- in other words, messages could be private, but a third party can always see who is communicating with whom by examining a central service and the connection graph. SimpleX, at it's core, is designed to be truly distributed with no central server. This allows for enormous scalability at low cost, and also makes it virtually impossible to snoop on the network graph.

The first application built on the platform is Simplex Chat, which for now is terminal (command line) based with mobile apps in the pipeline. The platform can easily support a private social network feed and a multitude of other services, which can be developed by the Simplex team or third party developers.

## What's new in v0.5?

We're exicted to announce that SimpleX Chat now supports group chat and file transfer!

### Chat groups

To create a group use the `/g <group>` command. You can then invite contacts to the group by entering the `/a <group> <name>` command. Your contact(s) will need to use the `/j accept` command to accept the invitation to the group. To send messages to the group, simply enter `#<group> <message>`.

**Please note:** Groups are not stored on any server; they are maintained as a list of members in the app database. Sending a message to the group sends a message to each member of the group.

![simplex-chat](../images/groups.gif)

### File transfer

Sharing files is simple! To send a file to a contact, use the `/f @<contact> <file_path>` command. The recipient will have to accept before the file is sent.

![simplex-chat](../images/files.gif)

## We're always looking for help!

We'd really appreciate your comments, criticism and support - a star on the GitHub repo, downloading and testing the chat or any contribution to the project will help a lot – thank you for all your support!

**Please note:** SimpleX Chat is in early stage development: we are still iterating protocols, improving privacy and security, so if you have communication scenarios requiring high security, you should consider some other options for now.

Our goal is to create a new kind of chat platform that lets you control your chat!

Originally published at [https://www.reddit.com/r/selfhosted/comments/poal79/simplex_chat_an_opensource_decentralized_chat/](https://www.reddit.com/r/selfhosted/comments/poal79/simplex_chat_an_opensource_decentralized_chat/)
