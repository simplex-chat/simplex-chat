---
title: SimpleX for business
revision: 03.12.2024
---

# Using SimpleX Chat in business

SimpleX Chat (aka SimpleX) is a decentralized communication network that provides private and secure messaging. Its users are rapidly growing, and providing customer services via SimpleX can offer you a unique opportunity to engage people who are the most enthusiastic about trying out early stage technology products and services.

This document aims to help you make the best use of SimpleX Chat if you choose to engage with its users.

## Communcate with customers via business address

In the same way you can connect to our "SimpleX Chat team" profile via the app, you can provide the address for your existing and prospective customers:
- to buy your product and services via chat,
- to ask any questions, make suggestions and provide feedback,
- to discover more information about your business.

Customers who value privacy and security, and want to engage with you without sharing any personal data and minimizing any metadata that is shared with you, will be really happy to use this communication channel.

From v6.2 SimpleX Chat supports business addresses. Their design allows you to accept requests from multiple customers, with the app creating a new business chat with each of them.

Business chats operate in a way similar to dedicated customer support systems by combining features of direct conversations and groups, and the only widely used messenger that provides such functionality is WeChat with Chinese business accounts.

When a customer connects to your business via the business contact address, a new conversation is created. Similarly to how direct chats work, the customer will see the name and logo of your business, and you will see the name and avatar of your customer.

But the business conversation works as a group - once the customer is connected, other people from the business can be added to the conversation, and the customers will see who are they talking with. This can be used to transfer business conversation to another person, or for escalation - in the same way as with the dedicated support systems.

SimpleX Chat profile with the business address can be used in one of these ways:
- for small teams it can be managed by one person running the app on their desktop computer, who would respond to customer questions and manually add to the conversation other people in the business, as required.
- if you have multiple support agents, you can run business profile in CLI client running in cloud VM or on any machine with high speed Internet (see Technical advice below), and they can connect to this client from desktop client, in turns. This is how we use our business profile ourselves, even though it requires some configuration. You can manage 100s of thousands of connected customers in this way.
- For larger teams, it would be appropriate to have this profile managed by chat bot that can reply to some simple questions, and to add support agents, based on their availability and the questions asked. These scenarios would require programming a chat bot, and we are currently working to simplify it.

In any case, it is important that the client application remains running and connected to the Internet for you to receive support requests.

## Customer broadcasts

While currently supported only via CLI clients (or via chat console in desktop and mobile clients), it can be used to broadcast important announcements to all connected customers. We will be adding this feature to desktop clients soon. We use it to broadcast release updates to a very large number of users who are connected to our own support profile.

## Community groups and promotion in group directory

In addition to providing support to clients individually, you can create a community group, and promote it via our experimental and growing [directory of public groups](./DIRECTORY.md). Community groups require ongoing moderation.

## Limitations

With all advantages in privacy and security of e2e encryption in SimpleX Chat, there are some important limitations:
- **protecting your data from loss is your responsibility**. This is the price of privacy - if you lose your device, or database passphrase, there is absolutely no way we would be able to support you to recover access. There are ways to work around these limitations.
- **you cannot access the same profile from multiple devices**. For all communication products it's a basic expectation, and yet there is not a single one that delivered it without some very serious privacy and security compromises. Better solutions are possible, and we will be implementing it, but reasonably secure approach is much more complex to implement than what is affordable at the current stage. You can access mobile or CLI profile from desktop, and the latter allows to use one profile by multiple people in turns, as we explain below.
- **your owner role in the groups cannot be restored if you lose the device**. The solution is to create owner profiles on multiple devices for all your important groups. This way if you lose device or data for one of profiles, you won't lose control of the group, and you can add a new one. Think about it as about keys to your cryptowallet.
- **current groups are highly experimental**. Message delivery can be delayed or fail in some cases, lists of members can be out of sync. There are approaches to make them more stable we use for our groups.

## Technical advice

### Running SimpleX Chat in the cloud

To install SimpleX Chat CLI in the cloud, follow this:

1. Create dedicated user for CLI:

   ```sh
   useradd -m -s /bin/bash simplex-cli
   ```

2. Create new tmux session

   ```sh
   tmux new -s simplex-cli
   ```

3. Login to dedicated user:

   ```sh
   su - simplex-cli
   ```

4. Install CLI:

   ```sh
   curl -o- https://raw.githubusercontent.com/simplex-chat/simplex-chat/stable/install.sh | bash
   ```

5. Run the CLI:

   ```sh
   simplex-chat
   ```

To deattach from running CLI simply press `Ctrl+B` and then `D`.

To reattach back to CLI, run: `tmux attach -t simplex-cli`.

### Using remote profiles via Desktop app

To use CLI from Desktop app, follow this:

1. Enable Developer tools in desktop app.

2. In the Desktop app, click - `Linked mobile` -> `+ Link a mobile`, choose local address `127.0.0.1`, enter some fixed port (can be any free port, e.g. 12345), and copy the link.

3. In the same machine where Desktop app is running, execute:

   Change `PORT` to port, chosen in the previous step in Desktop app and `SERVER_IP` to your server.

   ```sh
   ssh -R PORT:127.0.0.1:PORT -N root@SERVER_IP
   ```

4. In the CLI on the server:

   Change `LINK` to link, copied in the first step and enter the following:

   ```sh
   /crc LINK
   ```

   CLI will print verification code:

   ```sh
   Compare session code with controller and use:
   /verify remote ctrl ...
   ```
  
   Simply copy the whole line starting with `/verify ...` from the terminal and paste it. Now you can control the CLI from your Desktop app.

## Organizations using SimpleX Chat for customer service, support and sales

Please let us know if you use SimpleX Chat to communicate with your customers and want to be included in this list.
