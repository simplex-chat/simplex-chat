---
layout: layouts/privacy.html
permalink: /privacy/index.html
---

# SimpleX Chat Operators Privacy Policy and Conditions of Use

## Summary

[Introduction](#introduction) and [General principles](#general-principles) cover SimpleX Chat network design, the network operators, and the principles of privacy and security provided by SimpleX network.

[Privacy policy](#privacy-policy) covers:
- data stored only on your device - [your profiles](#user-profiles), delivered [messages and files](#messages-and-files). You can transfer this information to another device, and you are responsible for its preservation - if you delete the app it will be lost.
- [private message delivery](#private-message-delivery) that protects your IP address and connection graph from the destination servers.
- [undelivered messages and files](#storage-of-messages-and-files-on-the-servers) stored on the servers.
- [how users connect](#connections-with-other-users) without any user profile identifiers.
- [iOS push notifications](#ios-push-notifications) privacy limitations.
- [user support](#user-support), [SimpleX directory](#simplex-directory) and [any other data](#another-information-stored-on-the-servers) that may be stored on the servers.
- [preset server operators](#preset-server-operators) and the [information they may share](#information-preset-server-operators-may-share).
- [source code license](#source-code-license) and [updates to this document](#updates).

[Conditions of Use](#conditions-of-use-of-software-and-infrastructure) are the conditions you need to accept to use SimpleX Chat applications and the relay servers of preset operators. Their purpose is to protect the users and preset server operators.

*Please note*: this summary and any links in this document are provided for information only - they are not a part of the Privacy Policy and Conditions of Use.

## Introduction

SimpleX Chat (also referred to as SimpleX) is the first communication network based on a new protocol stack that builds on the same ideas of complete openness and decentralization as email and web, with the focus on providing security and privacy of communications, and without compromising on usability.

SimpleX messaging protocol is the first protocol that has no user profile IDs of any kind, not even random numbers, cryptographic keys or hashes that identify the users. SimpleX apps allow their users to send messages and files via relay server infrastructure. Relay server owners and operators do not have any access to your messages, thanks to double-ratchet end-to-end encryption algorithm (also known as Signal algorithm - do not confuse with Signal protocols or platform) and additional encryption layers, and they also have no access to your profile and contacts - as they do not host user accounts.

Double ratchet algorithm has such important properties as [forward secrecy](/docs/GLOSSARY.md#forward-secrecy), sender [repudiation](/docs/GLOSSARY.md#) and break-in recovery (also known as [post-compromise security](/docs/GLOSSARY.md#post-compromise-security)).

If you believe that any part of this document is not aligned with SimpleX network mission or values, please raise it via [email](mailto:chat@simplex.chat) or [chat](https://simplex.chat/contact#/?v=1&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2FK1rslx-m5bpXVIdMZg9NLUZ_8JBm8xTt%23%2F%3Fv%3D1%26dh%3DMCowBQYDK2VuAyEALDeVe-sG8mRY22LsXlPgiwTNs9dbiLrNuA7f3ZMAJ2w%253D%26srv%3Dbylepyau3ty4czmn77q4fglvperknl4bi2eb2fdy2bh4jxtf32kf73yd.onion).

## Privacy Policy

### General principles

SimpleX network software uses the best industry practices for security and encryption to provide client and server software for secure [end-to-end encrypted](/docs/GLOSSARY.md#end-to-end-encryption) messaging via private connections. This encryption is protected from being compromised by the relays servers, even if they are modified or compromised, via [man-in-the-middle attack](/docs/GLOSSARY.md#man-in-the-middle-attack).

SimpleX software is built on top of SimpleX messaging and application protocols, based on a new message routing protocol allowing to establish private connections without having identifiers assigned to its users - it does not use emails, phone numbers, usernames, identity keys or any other user profile identifiers to pass messages between the user applications.

SimpleX software is similar in its design approach to email clients and browsers - it allows you to have full control of your data and freely choose the relay server operators, in the same way you choose which website or email provider to use, or use your own relay servers, simply by changing the configuration of the client software. The only current restriction to that is Apple push notifications - at the moment they can only be delivered via the servers operated by SimpleX Chat Ltd, as explained below. We are exploring the solutions to deliver push notifications to iOS devices via other providers or users' own servers.

SimpleX network operators are not communication service provider, and provide public relays "as is", as experimental, without any guarantees of availability or data retention. The operators of the relay servers preset in the app ("Preset Server Operators"), including SimpleX Chat Ltd, are committed to maintain a high level of availability, reliability and security. SimpleX client apps can have multiple preset relay server operators that you can opt-in or opt-out of using. You are and will continue to be able to use any other operators or your own servers.

SimpleX network design is based on the principles of users and data sovereignty, and device and operator portability.

The implementation security assessment of SimpleX cryptography and networking was done in October 2022 by [Trail of Bits](https://www.trailofbits.com/about), and most fixes were released in v4.2 – see [the announcement](/blog/20221108-simplex-chat-v4.2-security-audit-new-website.md).

The cryptographic review of SimpleX protocols design was done in July 2024 by Trail of Bits – see [the announcement](/blog/20241014-simplex-network-v6-1-security-review-better-calls-user-experience.md).

### Your information

#### User profiles

Servers used by SimpleX Chat apps do not create, store or identify user chat profiles. The profiles you can create in the app are local to your device, and can be removed at any time via the app.

When you create the local profile, no records are created on any of the relay servers, and infrastructure operators, whether preset in the app or any other, have no access to any part of your information, and even to the fact that you created a profile - it is a local record stored only on your device. That means that if you delete the app, and have no backup, you will permanently lose all your data and the private connections you created with other software users.

You can transfer the profile to another device by creating a backup of the app data and restoring it on the new device, but you cannot use more than one device with the copy of the same profile at the same time - it will disrupt any active conversations on either or both devices, as a security property of end-to-end encryption.

#### Messages and Files

SimpleX relay servers cannot decrypt or otherwise access the content or even the size of your messages and files you send or receive. Each message is padded to a fixed size of 16kb. Each file is sent in chunks of 64kb, 256kb, 1mb or 4mb via all or some of the configured file relay servers. Both messages and files are sent end-to-end encrypted, and the servers do not have technical means to compromise this encryption, because part of the [key exchange](/docs/GLOSSARY.md#key-exchange) happens out-of-band.

Your message history is stored only on your own device and the devices of your contacts. While the recipients' devices are offline, messaging relay servers temporarily store end-to-end encrypted messages – you can configure which relay servers are used to receive the messages from the new contacts, and you can manually change them for the existing contacts too.

#### Private message delivery 

You do not have control over which servers are used to send messages to your contacts - these servers are chosen by your contacts. To send messages your client by default uses configured servers to forward messages to the destination servers, thus protecting your IP address from the servers chosen by your contacts.

In case you use preset servers of more than one operator, the app will prefer to use a server of an operator different from the operator of the destination server to forward messages, preventing destination server to correlate messages as belonging to one client.

You can additionally use VPN or some overlay network (e.g., Tor) to hide your IP address from the servers chosen by you.

*Please note*: the clients allow changing configuration to connect to the destination servers directly. It is not recommended - if you make such change, your IP address will be visible to the destination servers.

#### Storage of messages and files on the servers

The messages are removed from the relay servers as soon as all messages of the file they were stored in are delivered and saving new messages switches to another file, as long as these servers use unmodified published code. Undelivered messages are also marked as delivered after the time that is configured in the messaging servers you use (21 days for preset messaging servers).

The files are stored on file relay servers for the time configured in the relay servers you use (48 hours for preset file servers).

The encrypted messages can be stored for some time after they are delivered or expired (because servers use append-only logs for message storage). This time varies, and may be longer in connections with fewer messages, but it is usually limited to 1 month, including any backup storage.

#### Connections with other users

When you create a connection with another user, two messaging queues (you can think about them as mailboxes) are created on messaging relay servers (chosen by you and your contact each), that can be the preset servers or the servers that you and your contact configured in the app. SimpleX messaging protocol uses separate queues for direct and response messages, and the apps prefer to create these queues on two different relay servers, or, if available, the relays of two different operators, for increased privacy, in case you have more than one relay server configured in the app, which is the default.

Preset and unmodified SimpleX relay servers do not store information about which queues are linked to your profile on the device, and they do not collect any information that would allow infrastructure owners and operators to establish that these queues are related to your device or your profile - the access to each queue is authorized by two anonymous unique cryptographic keys, different for each queue, and separate for sender and recipient of the messages.

#### Connection links privacy

When you create a connection with another user, the app generates a link/QR code that can be shared with the user to establish the connection via any channel (email, any other messenger, or a video call). This link is safe to share via insecure channels, as long as you can identify the recipient and also trust that this channel did not replace this link (to mitigate the latter risk you can validate the security code via the app).

While the connection "links" contain SimpleX Chat Ltd domain name `simplex.chat`, this site is never accessed by the app, and is only used for these purposes:
- to direct the new users to the app download instructions,
- to show connection QR code that can be scanned via the app,
- to "namespace" these links,
- to open links directly in the installed app when it is clicked outside of the app.

You can always safely replace the initial part of the link `https://simplex.chat/` either with `simplex:/` (which is a URI scheme provisionally registered with IANA) or with any other domain name where you can self-host the app download instructions and show the connection QR code (but in case it is your domain, it will not open in the app). Also, while the page renders QR code, all the information needed to render it is only available to the browser, as the part of the "link" after `#` symbol is not sent to the website server.

#### iOS Push Notifications

This section applies only to the notification servers operated by SimpleX Chat Ltd.

When you choose to use instant push notifications in SimpleX iOS app, because the design of push notifications requires storing the device token on notification server, the notifications server can observe how many messaging queues your device has notifications enabled for, and approximately how many messages are sent to each queue.

Preset notification server cannot observe the actual addresses of these queues, as a separate address is used to subscribe to the notifications. It also cannot observe who sends messages to you. Apple push notifications servers can only observe how many notifications are sent to you, but not from how many contacts, or from which messaging relays, as notifications are delivered to your device end-to-end encrypted by one of the preset notification servers - these notifications only contain end-to-end encrypted metadata, not even encrypted message content, and they look completely random to Apple push notification servers.

You can read more about the design of iOS push notifications [here](./blog/20220404-simplex-chat-instant-notifications.md#our-ios-approach-has-one-trade-off).

#### Another information stored on the servers

Additional technical information can be stored on the network servers, including randomly generated authentication tokens, keys, push tokens, and other material that is necessary to transmit messages. SimpleX network design limits this additional technical information to the minimum required to operate the software and servers. To prevent server overloading or attacks, the servers can temporarily store data that can link to particular users or devices, including IP addresses, geographic location, or information related to the transport sessions. This information is not stored for the absolute majority of the app users, even for those who use the servers very actively.

#### SimpleX Directory

This section applies only to the experimental group directory operated by SimpleX Chat Ltd.

[SimpleX Directory](/docs/DIRECTORY.md) stores: your search requests, the messages and the members profiles in the registered groups. You can connect to SimpleX Directory via [this address](https://simplex.chat/contact#/?v=1-4&smp=smp%3A%2F%2Fu2dS9sG8nMNURyZwqASV4yROM28Er0luVTx5X1CsMrU%3D%40smp4.simplex.im%2FeXSPwqTkKyDO3px4fLf1wx3MvPdjdLW3%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEAaiv6MkMH44L2TcYrt_CsX3ZvM11WgbMEUn0hkIKTOho%253D%26srv%3Do5vmywmrnaxalvz6wi3zicyftgio6psuvyniis6gco6bp6ekl4cqj4id.onion).

#### Public groups and content channels

You may participate in a public group and receive content from a public channel (Group). In case you send messages or comments to the Group, you grant a license:
- to all recipients:
  - to share your messages with the new Group members and outside of the group, e.g. via quoting (replying), forwarding and copy-pasting your message. When your message is deleted or marked as deleted, the copies of your message will not be deleted.
  - to retain a copy of your messages according to the Group settings (e.g., the Group may allow irreversible message deletion from the recipient devices for a limited period of time, or it may only allow to edit and mark messages as deleted on recipient devices). Deleting message from the recipient devices or marking message as deleted revokes the license to share the message.
- to Group owners: to share your messages with the new Group members as history of the Group. Currently, the Group history shared with the new members is limited to 100 messages.

Group owners may use chat relays or automated bots (Chat Relays) to re-broadcast member messages to all members, for efficiency. The Chat Relays may be operated by the group owners, by preset operators or by 3rd parties. The Chat Relays have access to and will retain messages in line with Group settings, for technical functioning of the Group. Neither you nor group owners grant any content license to Chat Relay operators.

#### User Support

The app includes support contact operated by SimpleX Chat Ltd. If you contact support, any personal data you share is kept only for the purposes of researching the issue and contacting you about your case. We recommend contacting support [via chat](https://simplex.chat/contact#/?v=1&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2FK1rslx-m5bpXVIdMZg9NLUZ_8JBm8xTt%23%2F%3Fv%3D1%26dh%3DMCowBQYDK2VuAyEALDeVe-sG8mRY22LsXlPgiwTNs9dbiLrNuA7f3ZMAJ2w%253D%26srv%3Dbylepyau3ty4czmn77q4fglvperknl4bi2eb2fdy2bh4jxtf32kf73yd.onion) when it is possible, and avoid sharing any personal information.

### Preset Server Operators

Preset server operators will not share the information on their servers with each other, other than aggregate usage statistics. 

Preset server operators must not provide general access to their servers or the data on their servers to each other.

Preset server operators will provide non-administrative access to control port of preset servers to SimpleX Chat Ltd, for the purposes of removing illegal content identified in publicly accessible resources (contact and group addresses, and downloadable files). This control port access only allows deleting known links and files, and accessing aggregate server-wide statistics, but does NOT allow enumerating any information on the servers or accessing statistics related to specific users.

### Information Preset Server Operators May Share

The preset server operators use third parties. While they do not have access and cannot share any user data, these third parties may access the encrypted user messages (but NOT the actual unencrypted message content or size) as it is stored or transmitted via the servers. Hosting and network providers can also store IP addresses and other transport information as part of their logs.

SimpleX Chat Ltd uses a third party for email services - if you ask for support via email, your and SimpleX Chat Ltd email providers may access these emails according to their privacy policies and terms. When the request is sensitive, please contact us via SimpleX Chat apps or using encrypted email using PGP key published at [openpgp.org](https://keys.openpgp.org/search?q=chat%40simplex.chat).

The cases when the preset server operators may share the data temporarily stored on the servers:

- To meet any applicable law, or enforceable governmental request or court order.
- To enforce applicable terms, including investigation of potential violations.
- To detect, prevent, or otherwise address fraud, security, or technical issues.
- To protect against harm to the rights, property, or safety of software users, operators of preset servers, or the public as required or permitted by law.

By the time of updating this document, the preset server operators were not served with any enforceable requests and did not provide any information from the servers to any third parties. If the preset server operators are ever requested to provide such access or information, they will follow the due legal process to limit any information shared with the third parties to the minimally required by law.

Preset server operators will publish information they are legally allowed to share about such requests in the [Transparency reports](./docs/TRANSPARENCY.md).

### Source code license

As this software is fully open-source and provided under AGPLv3 license, all infrastructure owners and operators, and the developers of the client and server applications who use the SimpleX Chat source code, are required to publish any changes to this software under the same AGPLv3 license - including any modifications to the servers.

In addition to the AGPLv3 license terms, the preset relay server operators are committed to the software users that these servers will always be compiled from the [published open-source code](https://github.com/simplex-chat/simplexmq), without any modifications.

### Updates

This Privacy Policy applies to SimpleX Chat Ltd and all other preset server operators you use in the app.

This Privacy Policy may be updated as needed so that it is current, accurate, and as clear as possible. When it is updated, you will have to review and accept the changed policy within 30 days of such changes to continue using preset relay servers. Even if you fail to accept the changed policy, your continued use of SimpleX Chat software applications and preset relay servers confirms your acceptance of the updated Privacy Policy.

Please also read The Conditions of Use of Software and Infrastructure below.

If you have questions about this Privacy Policy please contact SimpleX Chat Ltd via [email](mailto:chat@simplex.chat) or [chat](https://simplex.chat/contact#/?v=1&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2FK1rslx-m5bpXVIdMZg9NLUZ_8JBm8xTt%23%2F%3Fv%3D1%26dh%3DMCowBQYDK2VuAyEALDeVe-sG8mRY22LsXlPgiwTNs9dbiLrNuA7f3ZMAJ2w%253D%26srv%3Dbylepyau3ty4czmn77q4fglvperknl4bi2eb2fdy2bh4jxtf32kf73yd.onion).

## Conditions of Use of Software and Infrastructure

You accept the Conditions of Use of Software and Infrastructure ("Conditions") by installing or using any of SimpleX Chat software or using any of server infrastructure (collectively referred to as "Applications") operated by the Preset Server Operators, including SimpleX Chat Ltd, whether these servers are preset in the software or not.

**Minimal age**. You must be at least 13 years old to use SimpleX Chat Applications. The minimum age to use SimpleX Applications without parental approval may be higher in your country.

**Infrastructure**. Infrastructure of the preset server operators includes messaging and file relay servers. SimpleX Chat Ltd also provides iOS push notification servers for public use. This infrastructure does not have any modifications from the [published open-source code](https://github.com/simplex-chat/simplexmq) available under AGPLv3 license. Any infrastructure provider, whether commercial or not, is required by the Affero clause (named after Affero Inc. company that pioneered the community-based Q&A sites in early 2000s) to publish any modifications under the same license. The statements in relation to Infrastructure and relay servers anywhere in this document assume no modifications to the published code, even in the cases when it is not explicitly stated.

**Client applications**. SimpleX Chat client application Software (referred to as "app" or "apps") also has no modifications compared with published open-source code, and any developers of the alternative client apps based on SimpleX Chat code are required to publish any modifications under the same AGPLv3 license. Client applications should not include any tracking or analytics code, and do not share any tracking information with SimpleX Chat Ltd, preset server operators or any other third parties. If you ever discover any tracking or analytics code, please report it to SimpleX Chat Ltd, so it can be removed.

**Accessing the infrastructure**. For the efficiency of the network access, the client Software by default accesses all queues your app creates on any relay server within one user profile via the same network (TCP/IP) connection. At the cost of additional traffic this configuration can be changed to use different transport session for each connection. Relay servers do not collect information about which queues were created or accessed via the same connection, so the relay servers cannot establish which queues belong to the same user profile. Whoever might observe your network traffic would know which relay servers you use, and how much data you send, but not to whom it is sent - the data that leaves the servers is always different from the data they receive - there are no identifiers or ciphertext in common, even inside TLS encryption layer. Please refer to the [technical design document](https://github.com/simplex-chat/simplexmq/blob/master/protocol/overview-tjr.md) for more information about the privacy model and known security and privacy risks.

**Privacy of user data**. Servers do not retain any data you transmit for any longer than necessary to deliver the messages between apps. Preset server operators collect aggregate statistics across all their servers, as supported by published code and can be enabled by any infrastructure operator, but not any statistics per-user, or per geographic location, or per IP address, or per transport session. SimpleX Chat Ltd does not have information about how many people use SimpleX Chat applications, it only knows an approximate number of app installations and the aggregate traffic through the preset servers. In any case, preset server operators do not and will not sell or in any way monetize user data. The future business model assumes charging for some optional Software features instead, in a transparent and fair way.

**Operating Infrastructure**. For the purpose of using SimpleX Chat Software, if you continue using preset servers, you agree that your end-to-end encrypted messages are transferred via the preset servers in any countries where preset server operators have or use facilities and service providers or partners. The information about geographic location and hosting providers of the preset messaging servers is available on server pages.

**Software**. You agree to downloading and installing updates to SimpleX Chat Applications when they are available; they would only be automatic if you configure your devices in this way.

**Traffic and device costs**. You are solely responsible for the traffic and device costs that you incur while using SimpleX Chat Applications, and any associated taxes.

**Legal usage**. You agree to use SimpleX Chat Applications only for legal purposes. You will not use (or assist others in using) the Applications in ways that: 1) violate or infringe the rights of Software users, SimpleX Chat Ltd, other preset server operators, or others, including privacy, publicity, intellectual property, or other proprietary rights; 2) involve sending illegal communications, e.g. spam. While server operators cannot access content or identify messages or groups, in some cases the links to the illegal communications can be shared publicly on social media or websites. Preset server operators reserve the right to remove such links from the preset servers and disrupt the conversations that send illegal content via their servers, whether they were reported by the users or discovered by the operators themselves.

**Damage to SimpleX Chat Ltd and Preset Server Operators**. You must not (or assist others to) access, use, modify, distribute, transfer, or exploit SimpleX Chat Applications in unauthorized manners, or in ways that harm Software users, SimpleX Chat Ltd, other preset server operators, their Infrastructure, or any other systems. For example, you must not 1) access preset operators' Infrastructure or systems without authorization, in any way other than by using the Software or by using a 3rd party client applications that satisfies the requirements of the Conditions of use (see the next section); 2) disrupt the integrity or performance of preset operators' Infrastructure; 3) collect information about the users in any manner; or 4) sell, rent, or charge for preset operators' Infrastructure. This does not prohibit you from providing your own Infrastructure to others, whether free or for a fee, as long as you do not violate these Conditions and AGPLv3 license, including the requirement to publish any modifications of the relay server software.

**3rd party client applications**. You may use a 3rd party application (App) to access preset operators' Infrastructure or systems, provided that this App:
- is compatible with the protocol specifications not older than 1 year,
- provides user-to-user messaging only or enables automated chat bots sending messages requested by users (in case of bots, it must be made clear to the users that these are automated bots),
- implements the same limits, rules and restrictions as Software,
- requires that the users accept the same Conditions of use of preset operators' Infrastructure as in Software prior to providing access to this Infrastructure,
- displays the notice that it is the App for using SimpleX network,
- provides its source code under open-source license accessible to the users via the App interface. In case the App uses the source code of Software, the App's source code must be provided under AGPLv3 license, and in case it is developed without using Software code its source code must be provided under any widely recognized free open-source license,
- does NOT use the branding of SimpleX Chat Ltd without the permission,
- does NOT pretend to be Software,
- complies with these Conditions of use.

**Keeping your data secure**. SimpleX Chat is the first communication software that aims to be 100% private by design - server software neither has the ability to access your messages, nor it has information about who you communicate with. That means that you are solely responsible for keeping your device, your user profile and any data safe and secure. If you lose your phone or remove the Software from the device, you will not be able to recover the lost data, unless you made a back up. To protect the data you need to make regular backups, as using old backups may disrupt your communication with some of the contacts. SimpleX Chat Ltd and other preset server operators are not responsible for any data loss.

**Storing the messages on the device**. The messages are stored in the encrypted database on your device. Whether and how database passphrase is stored is determined by the configuration of the Software you use. The databases created prior to 2023 or in CLI (terminal) app may remain unencrypted, and it will be indicated in the app interface. In this case, if you make a backup of the data and store it unencrypted, the backup provider may be able to access the messages. Please note, that the desktop apps can be configured to store the database passphrase in the configuration file in plaintext, and unless you set the passphrase when first running the app, a random passphrase will be used and stored on the device. You can remove it from the device via the app settings.

**Storing the files on the device**. The files currently sent and received in the apps by default (except CLI app) are stored on your device encrypted using unique keys, different for each file, that are stored in the database. Once the message that the file was attached to is removed, even if the copy of the encrypted file is retained, it should be impossible to recover the key allowing to decrypt the file. This local file encryption may affect app performance, and it can be disabled via the app settings. This change will only affect the new files. If you later re-enable the encryption, it will also affect only the new files. If you make a backup of the app data and store it unencrypted, the backup provider will be able to access any unencrypted files. In any case, irrespective of the storage setting, the files are always sent by all apps end-to-end encrypted.

**No Access to Emergency Services**. SimpleX Chat Applications do not provide access to emergency service providers like the police, fire department, hospitals, or other public safety organizations. Make sure you can contact emergency service providers through a mobile, fixed-line telephone, or other service.

**Third-party services**. SimpleX Chat Applications may allow you to access, use, or interact with the websites of SimpleX Chat Ltd, preset server operators or other third-party websites, apps, content, and other products and services. When you use third-party services, their terms and privacy policies govern your use of those services.

**Your Rights**. You own the messages and the information you transmit through SimpleX Applications. Your recipients are able to retain the messages they receive from you; there is no technical ability to delete data from their devices. While there are various app features that allow deleting messages from the recipients' devices, such as _disappearing messages_ and _full message deletion_, their functioning on your recipients' devices cannot be guaranteed or enforced, as the device may be offline or have a modified version of the Software. At the same time, repudiation property of the end-to-end encryption algorithm allows you to plausibly deny having sent the message, like you can deny what you said in a private face-to-face conversation, as the recipient cannot provide any proof to the third parties, by design.

**License**. SimpleX Chat Ltd grants you a limited, revocable, non-exclusive, and non-transferable license to use SimpleX Chat Applications in accordance with these Conditions. The source-code of Applications is available and can be used under [AGPL v3 license](https://github.com/simplex-chat/simplex-chat/blob/stable/LICENSE).

**SimpleX Chat Ltd Rights**. SimpleX Chat Ltd (and, where applicable, preset server operators) owns all copyrights, trademarks, domains, logos, trade secrets, and other intellectual property rights associated with the Applications. You may not use SimpleX Chat Ltd copyrights, trademarks, domains, logos, and other intellectual property rights unless you have SimpleX Chat Ltd written permission, and unless under an open-source license distributed together with the source code. To report copyright, trademark, or other intellectual property infringement, please contact chat@simplex.chat.

**Disclaimers**. YOU USE SIMPLEX APPLICATIONS AT YOUR OWN RISK AND SUBJECT TO THE FOLLOWING DISCLAIMERS. SIMPLEX CHAT LTD PROVIDES APPLICATIONS ON AN “AS IS” BASIS WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, TITLE, NON-INFRINGEMENT, AND FREEDOM FROM COMPUTER VIRUS OR OTHER HARMFUL CODE. SIMPLEX CHAT LTD DOES NOT WARRANT THAT ANY INFORMATION PROVIDED BY THEM IS ACCURATE, COMPLETE, OR USEFUL, THAT THEIR APPLICATIONS WILL BE OPERATIONAL, ERROR-FREE, SECURE, OR SAFE, OR THAT THEIR APPLICATIONS WILL FUNCTION WITHOUT DISRUPTIONS, DELAYS, OR IMPERFECTIONS. SIMPLEX CHAT LTD AND OTHER PRESET OPERATORS DO NOT CONTROL, AND ARE NOT RESPONSIBLE FOR, CONTROLLING HOW OR WHEN THE USERS USE APPLICATIONS. SIMPLEX CHAT LTD AND OTHER PRESET OPERATORS ARE NOT RESPONSIBLE FOR THE ACTIONS OR INFORMATION (INCLUDING CONTENT) OF THEIR USERS OR OTHER THIRD PARTIES. YOU RELEASE SIMPLEX CHAT LTD, OTHER PRESET OPERATORS, AFFILIATES, DIRECTORS, OFFICERS, EMPLOYEES, PARTNERS, AND AGENTS ("SIMPLEX PARTIES") FROM ANY CLAIM, COMPLAINT, CAUSE OF ACTION, CONTROVERSY, OR DISPUTE (TOGETHER, "CLAIM") AND DAMAGES, KNOWN AND UNKNOWN, RELATING TO, ARISING OUT OF, OR IN ANY WAY CONNECTED WITH ANY SUCH CLAIM YOU HAVE AGAINST ANY THIRD PARTIES.

**Limitation of liability**. THE SIMPLEX PARTIES WILL NOT BE LIABLE TO YOU FOR ANY LOST PROFITS OR CONSEQUENTIAL, SPECIAL, PUNITIVE, INDIRECT, OR INCIDENTAL DAMAGES RELATING TO, ARISING OUT OF, OR IN ANY WAY IN CONNECTION WITH OUR CONDITIONS, US, OR SIMPLEX APPLICATIONS, EVEN IF THE SIMPLEX PARTIES HAVE BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES. THE AGGREGATE LIABILITY OF THE SIMPLEX PARTIES RELATING TO, ARISING OUT OF, OR IN ANY WAY IN CONNECTION WITH THESE CONDITIONS, THE SIMPLEX PARTIES, OR THE APPLICATIONS WILL NOT EXCEED ONE DOLLAR ($1). THE FOREGOING DISCLAIMER OF CERTAIN DAMAGES AND LIMITATION OF LIABILITY WILL APPLY TO THE MAXIMUM EXTENT PERMITTED BY APPLICABLE LAW. THE LAWS OF SOME JURISDICTIONS MAY NOT ALLOW THE EXCLUSION OR LIMITATION OF CERTAIN DAMAGES, SO SOME OR ALL OF THE EXCLUSIONS AND LIMITATIONS SET FORTH ABOVE MAY NOT APPLY TO YOU. NOTWITHSTANDING ANYTHING TO THE CONTRARY IN THE CONDITIONS, IN SUCH CASES, THE LIABILITY OF THE SIMPLEX PARTIES WILL BE LIMITED TO THE EXTENT PERMITTED BY APPLICABLE LAW.

**Availability**. The Applications may be disrupted, including for maintenance, upgrades, or network or equipment failures. SimpleX Chat Ltd may discontinue some or all of their Applications, including certain features and the support for certain devices and platforms, at any time. Preset server operators may discontinue providing the servers, at any time.

**Resolving disputes**. You agree to resolve any Claim you have with SimpleX Chat Ltd and/or preset server operators relating to or arising from these Conditions, them, or the Applications in the courts of England and Wales. You also agree to submit to the personal jurisdiction of such courts for the purpose of resolving all such disputes. The laws of England govern these Conditions, as well as any disputes, whether in court or arbitration, which might arise between SimpleX Chat Ltd (or preset server operators) and you, without regard to conflict of law provisions.

**Changes to the conditions**. SimpleX Chat Ltd may update the Conditions from time to time. The updated conditions have to be accepted within 30 days. Even if you fail to accept updated conditions, your continued use of SimpleX Chat Applications confirms your acceptance of the updated Conditions and supersedes any prior Conditions. You will comply with all applicable export control and trade sanctions laws. These Conditions cover the entire agreement between you and SimpleX Chat Ltd, and any preset server operators where applicable, regarding SimpleX Chat Applications. If you do not agree with these Conditions, you should stop using the Applications.

**Enforcing the conditions**. If SimpleX Chat Ltd or preset server operators fail to enforce any of these Conditions, that does not mean they waive the right to enforce them. If any provision of the Conditions is deemed unlawful, void, or unenforceable, that provision shall be deemed severable from the Conditions and shall not affect the enforceability of the remaining provisions. The Applications are not intended for distribution to or use in any country where such distribution or use would violate local law or would subject SimpleX Chat Ltd to any regulations in another country. SimpleX Chat Ltd reserve the right to limit the access to the Applications in any country. Preset operators reserve the right to limit access to their servers in any country. If you have specific questions about these Conditions, please contact SimpleX Chat Ltd at chat@simplex.chat.

**Ending these conditions**. You may end these Conditions with SimpleX Chat Ltd and preset server operators at any time by deleting the Applications from your devices and discontinuing use of the Infrastructure of SimpleX Chat Ltd and preset server operators. The provisions related to Licenses, Disclaimers, Limitation of Liability, Resolving dispute, Availability, Changes to the conditions, Enforcing the conditions, and Ending these conditions will survive termination of your relationship with SimpleX Chat Ltd and/or preset server operators.

Updated March 3, 2025
