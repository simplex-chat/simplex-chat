---
layout: layouts/privacy.html
---

# SimpleX Chat Privacy Policy and Conditions of Use

SimpleX Chat is the first communication network based on a new protocol stack that builds on the same ideas of complete openness and decentralization as email and web, with the focus on providing security and privacy of communications, and without compromising on usability.

SimpleX Chat communication protocol is the first protocol that has no user profile IDs of any kind, not even random numbers, cryptographic keys or hashes that identify the users. SimpleX Chat apps allow their users to send messages and files via relay server infrastructure. Relay server owners and providers do not have any access to your messages, thanks to double-ratchet end-to-end encryption algorithm (also known as Signal algorithm - do not confuse with Signal protocols or platform) and additional encryption layers, and they also have no access to your profile and contacts - as they do not provide any user accounts.

Double ratchet algorithm has such important properties as [forward secrecy](/docs/GLOSSARY.md#forward-secrecy), sender [repudiation](/docs/GLOSSARY.md#) and break-in recovery (also known as [post-compromise security](/docs/GLOSSARY.md#post-compromise-security)).

If you believe that any part of this document is not aligned with our mission or values, please raise it with us via [email](chat@simplex.chat) or [chat](https://simplex.chat/contact#/?v=1&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2FK1rslx-m5bpXVIdMZg9NLUZ_8JBm8xTt%23%2F%3Fv%3D1%26dh%3DMCowBQYDK2VuAyEALDeVe-sG8mRY22LsXlPgiwTNs9dbiLrNuA7f3ZMAJ2w%253D%26srv%3Dbylepyau3ty4czmn77q4fglvperknl4bi2eb2fdy2bh4jxtf32kf73yd.onion).

## Privacy Policy

SimpleX Chat Ltd uses the best industry practices for security and encryption to provide client and server software for secure [end-to-end encrypted](/docs/GLOSSARY.md#end-to-end-encryption) messaging via private connections. This encryption cannot be compromised by the relays servers, even if they are modified or compromised, via [man-in-the-middle attack](/docs/GLOSSARY.md#man-in-the-middle-attack), unlike most other communication platforms, services and networks.

SimpleX Chat software is built on top of SimpleX messaging and application protocols, based on a new message routing protocol allowing to establish private connections without having any kind of addresses or other identifiers assigned to its users - it does not use emails, phone numbers, usernames, identity keys or any other user profile identifiers to pass messages between the user applications.

SimpleX Chat software is similar in its design approach to email clients and browsers - it allows you to have full control of your data and freely choose the relay server providers, in the same way you choose which website or email provider to use, or use your own relay servers, simply by changing the configuration of the client software. The only current restriction to that is Apple push notifications - at the moment they can only be delivered via the preset servers that we operate, as explained below. We are exploring the solutions to deliver push notifications to iOS devices via other providers or users' own servers.

While SimpleX Chat Ltd is not a communication service provider, and provide public preset relays "as is", as experimental, without any guarantees of availability or data retention, we are committed to maintain a high level of availability, reliability and security of these preset relays. We will be adding alternative preset infrastructure providers to the software in the future, and you will continue to be able to use any other providers or your own servers.

We see users and data sovereignty, and device and provider portability as critically important properties for any communication system.

SimpleX Chat security assessment was done in October 2022 by [Trail of Bits](https://www.trailofbits.com/about), and most fixes were released in v4.2 – see [the announcement](/blog/20221108-simplex-chat-v4.2-security-audit-new-website.md).

### Your information

#### User profiles

Servers used by SimpleX Chat apps do not create, store or identify user profiles. The profiles you can create in the app are local to your device, and can be removed at any time via the app.

When you create the local profile, no records are created on any of the relay servers, and infrastructure providers, whether SimpleX Chat Ltd or any other, have no access to any part of your information, and even to the fact that you created a profile - it is a local record stored only on your device. That means that if you delete the app, and have no backup, you will permanently lose all your data and the private connections you created with other software users.

You can transfer the profile to another device by creating a backup of the app data and restoring it on the new device, but you cannot use more than one device with the copy of the same profile at the same time - it will disrupt any active conversations on either or both devices, as a security property of end-to-end encryption.

#### Messages and Files

SimpleX relay servers cannot decrypt or otherwise access the content or even the size of your messages and files you send or receive. Each message is padded to a fixed size of 16kb. Each file is sent in chunks of 64kb, 256kb, 1mb or 8mb via all or some of the configured file relay servers. Both messages and files are sent end-to-end encrypted, and the servers do not have technical means to compromise this encryption, because part of the [key exchange](/docs/GLOSSARY.md#key-exchange) happens out-of-band.

Your message history is stored only on your own device and the devices of your contacts. While the recipients' devices are offline, messaging relay servers temporarily store end-to-end encrypted messages – you can configure which relay servers are used to receive the messages from the new contacts, and you can manually change them for the existing contacts too.

You do not have control over which servers are used to send messages to your contacts - they are chosen by them. To send messages your client needs to connect to these servers, therefore the servers chosen by your contacts can observe your IP address. You can use VPN or some overlay network (e.g., Tor) to hide your IP address from the servers chosen by your contacts. In the near future we will add the layer in the messaging protocol that will route sent message via the relays chosen by you as well.

The messages are permanently removed from the used relay servers as soon as they are delivered, as long as these servers used unmodified published code. Undelivered messages are deleted after the time that is configured in the messaging servers you use (21 days for preset messaging servers).

The files are stored on file relay servers for the time configured in the relay servers you use (48 hours for preset file servers).

If a messaging servers are restarted, the encrypted message can be stored in a backup file until it is overwritten by the next restart (usually within 1 week for preset relay servers).

As this software is fully open-source and provided under AGPLv3 license, all infrastructure providers and owners, and the developers of the client and server applications who use the SimpleX Chat source code, are required to publish any changes to this software under the same AGPLv3 license - including any modifications to the provided servers.

In addition to the AGPLv3 license terms, SimpleX Chat Ltd is committed to the software users that the preset relays that we provide via the apps will always be compiled from the [published open-source code](https://github.com/simplex-chat/simplexmq), without any modifications.

#### Connections with other users

When you create a connection with another user, two messaging queues (you can think about them as mailboxes) are created on messaging relay servers (chosen by you and your contact each), that can be the preset servers or the servers that you and your contact configured in the app. SimpleX messaging protocol uses separate queues for direct and response messages, and the apps prefer to create these queues on two different relay servers for increased privacy, in case you have more than one relay server configured in the app, which is the default.

SimpleX relay servers do not store information about which queues are linked to your profile on the device, and they do not collect any information that would allow infrastructure owners and providers to establish that these queues are related to your device or your profile - the access to each queue is authorized by two anonymous unique cryptographic keys, different for each queue, and separate for sender and recipient of the messages.

#### Connection links privacy

When you create a connection with another user, the app generates a link/QR code that can be shared with the user to establish the connection via any channel (email, any other messenger, or a video call). This link is safe to share via insecure channels, as long as you can identify the recipient and also trust that this channel did not replace this link (to mitigate the latter risk you can validate the security code via the app).

While the connection "links" contain SimpleX Chat Ltd domain name `simplex.chat`, this site is never accessed by the app, and is only used for these purposes:
- to direct the new users to the app download instructions,
- to show connection QR code that can be scanned via the app,
- to "namespace" these links,
- to open links directly in the installed app when it is clicked outside of the app.

You can always safely replace the initial part of the link `https://simplex.chat/` either with `simplex:/` (which is a URI scheme provisionally registered with IANA) or with any other domain name where you can self-host the app download instructions and show the connection QR code (but in case it is your domain, it will not open in the app). Also, while the page renders QR code, all the information needed to render it is only available to the browser, as the part of the "link" after `#` symbol is not sent to the website server.

#### iOS Push Notifications

When you choose to use instant push notifications in SimpleX iOS app, because the design of push notifications requires storing the device token on notification server, the notifications server can observe how many messaging queues your device has notifications enabled for, and approximately how many messages are sent to each queue.

Preset notification server cannot observe the actual addresses of these queues, as a separate address is used to subscribe to the notifications. It also cannot observe who sends messages to you. Apple push notifications servers can only observe how many notifications are sent to you, but not from how many contacts, or from which messaging relays, as notifications are delivered to your device end-to-end encrypted by one of the preset notification servers - these notifications only contain end-to-end encrypted metadata, not even encrypted message content, and they look completely random to Apple push notification servers.

You can read more about the design of iOS push notifications [here](https://simplex.chat/blog/20220404-simplex-chat-instant-notifications.html#our-ios-approach-has-one-trade-off).

#### Another information stored on the servers

Additional technical information can be stored on our servers, including randomly generated authentication tokens, keys, push tokens, and other material that is necessary to transmit messages. SimpleX Chat design limits this additional technical information to the minimum required to operate the software and servers. To prevent server overloading or attacks, the servers can temporarily store data that can link to particular users or devices, including IP addresses, geographic location, or information related to the transport sessions. This information is not stored for the absolute majority of the app users, even for those who use the servers very actively.

#### SimpleX Directory

[SimpleX Directory](/docs/DIRECTORY.md) stores: your search requests, the messages and the members profiles in the registered groups. You can connect to SimpleX Directory via [this address](https://simplex.chat/contact#/?v=1-4&smp=smp%3A%2F%2Fu2dS9sG8nMNURyZwqASV4yROM28Er0luVTx5X1CsMrU%3D%40smp4.simplex.im%2FeXSPwqTkKyDO3px4fLf1wx3MvPdjdLW3%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEAaiv6MkMH44L2TcYrt_CsX3ZvM11WgbMEUn0hkIKTOho%253D%26srv%3Do5vmywmrnaxalvz6wi3zicyftgio6psuvyniis6gco6bp6ekl4cqj4id.onion).

#### User Support

If you contact SimpleX Chat Ltd, any personal data you share with us is kept only for the purposes of researching the issue and contacting you about your case. We recommend contacting support [via chat](https://simplex.chat/contact#/?v=1&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2FK1rslx-m5bpXVIdMZg9NLUZ_8JBm8xTt%23%2F%3Fv%3D1%26dh%3DMCowBQYDK2VuAyEALDeVe-sG8mRY22LsXlPgiwTNs9dbiLrNuA7f3ZMAJ2w%253D%26srv%3Dbylepyau3ty4czmn77q4fglvperknl4bi2eb2fdy2bh4jxtf32kf73yd.onion) when it is possible, and avoid sharing any personal information.

### Information we may share

SimpleX Chat Ltd operates preset relay servers using third parties. While we do not have access and cannot share any user data, these third parties may access the encrypted user messages (but NOT the actual unencrypted message content or size) as it is stored or transmitted via our servers. Hosting providers can also store IP addresses and other transport information as part of their logs.

We use a third party for email services - if you ask for support via email, your and SimpleX Chat Ltd email providers may access these emails according to their privacy policies and terms. When the request is sensitive, we recommend contacting us via SimpleX Chat or using encrypted email using PGP key published at [openpgp.org](https://keys.openpgp.org/search?q=chat%40simplex.chat).

The cases when SimpleX Chat Ltd may share the data temporarily stored on the servers:

- To meet any applicable law, or enforceable governmental request or court order.
- To enforce applicable terms, including investigation of potential violations.
- To detect, prevent, or otherwise address fraud, security, or technical issues.
- To protect against harm to the rights, property, or safety of software users, SimpleX Chat Ltd, or the public as required or permitted by law.

At the time of updating this document, we have never provided or have been requested the access to the preset relay servers or any information from the servers by any third parties. If we are ever requested to provide such access or information, we will follow the due legal process to limit any information shared with the third parties to the minimally required by law.

We will publish information we are legally allowed to share about such requests in the [Transparency reports](/docs/TRANSPARENCY.md).

### Updates

We will update this Privacy Policy as needed so that it is current, accurate, and as clear as possible. Your continued use of our software applications and preset relays infrastructure confirms your acceptance of our updated Privacy Policy.

Please also read our Conditions of Use of Software and Infrastructure below.

If you have questions about our Privacy Policy please contact us via [email](chat@simplex.chat) or [chat](https://simplex.chat/contact#/?v=1&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2FK1rslx-m5bpXVIdMZg9NLUZ_8JBm8xTt%23%2F%3Fv%3D1%26dh%3DMCowBQYDK2VuAyEALDeVe-sG8mRY22LsXlPgiwTNs9dbiLrNuA7f3ZMAJ2w%253D%26srv%3Dbylepyau3ty4czmn77q4fglvperknl4bi2eb2fdy2bh4jxtf32kf73yd.onion).

## Conditions of Use of Software and Infrastructure

You accept the Conditions of Use of Software and Infrastructure ("Conditions") by installing or using any of our software or using any of our server infrastructure (collectively referred to as "Applications"), whether preset in the software or not.

**Minimal age**. You must be at least 13 years old to use our Applications. The minimum age to use our Applications without parental approval may be higher in your country.

**Infrastructure**. Our Infrastructure includes preset messaging and file relay servers, and iOS push notification servers provided by SimpleX Chat Ltd for public use. Our infrastructure does not have any modifications from the [published open-source code](https://github.com/simplex-chat/simplexmq) available under AGPLv3 license. Any infrastructure provider, whether commercial or not, is required by the Affero clause (named after Affero Inc. company that pioneered the community-based Q&A sites in early 2000s) to publish any modifications under the same license. The statements in relation to Infrastructure and relay servers anywhere in this document assume no modifications to the published code, even in the cases when it is not explicitly stated.

**Client applications**. Our client application Software (referred to as "app" or "apps") also has no modifications compared with published open-source code, and any developers of the alternative client apps based on our code are required to publish any modifications under the same AGPLv3 license. Client applications should not include any tracking or analytics code, and do not share any information with SimpleX Chat Ltd or any other third parties. If you ever discover any tracking or analytics code, please report it to us, so we can remove it.

**Accessing the infrastructure**. For the efficiency of the network access, the client Software by default accesses all queues your app creates on any relay server within one user profile via the same network (TCP/IP) connection. At the cost of additional traffic this configuration can be changed to use different transport session for each connection. Relay servers do not collect information about which queues were created or accessed via the same connection, so the relay servers cannot establish which queues belong to the same user profile. Whoever might observe your network traffic would know which relay servers you use, and how much data you send, but not to whom it is sent - the data that leaves the servers is always different from the data they receive - there are no identifiers or ciphertext in common, even inside TLS encryption layer. Please refer to our [technical design document](https://github.com/simplex-chat/simplexmq/blob/master/protocol/overview-tjr.md) for more information about our privacy model and known security and privacy risks.

**Privacy of user data**. Servers do not retain any data we transmit for any longer than necessary to deliver the messages between apps. SimpleX Chat Ltd collects aggregate statistics across all its servers, as supported by published code and can be enabled by any infrastructure provider, but not any statistics per-user, or per geographic location, or per IP address, or per transport session. We do not have information about how many people use SimpleX Chat applications, we only know an approximate number of app installations and the aggregate traffic through the preset servers. In any case, we do not and will not sell or in any way monetize user data. Our future business model assumes charging for some optional Software features instead, in a transparent and fair way.

**Operating our Infrastructure**. For the purpose of using our Software, if you continue using preset servers, you agree that your end-to-end encrypted messages are transferred via the preset servers in any countries where we have or use facilities and service providers or partners. The information about geographic location of the servers will be made available in the apps in the near future.

**Software**. You agree to downloading and installing updates to our Applications when they are available; they would only be automatic if you configure your devices in this way.

**Traffic and device costs**. You are solely responsible for the traffic and device costs that you incur while using our Applications, and any associated taxes.

**Legal usage**. You agree to use our Applications only for legal purposes. You will not use (or assist others in using) our Applications in ways that: 1) violate or infringe the rights of Software users, SimpleX Chat Ltd, or others, including privacy, publicity, intellectual property, or other proprietary rights; 2) involve sending illegal communications, e.g. spam. While we cannot access content or identify messages or groups, in some cases the links to the illegal communications available via our Applications can be shared publicly on social media or websites. We reserve the right to remove such links from the preset servers and disrupt the conversations that send illegal content via our servers, whether they were reported by the users or discovered by our team.

**Damage to SimpleX Chat Ltd**. You must not (or assist others to) access, use, modify, distribute, transfer, or exploit our Applications in unauthorized manners, or in ways that harm Software users, SimpleX Chat Ltd, our Infrastructure, or any other systems. For example, you must not 1) access our Infrastructure or systems without authorization, in any way other than by using the Software; 2) disrupt the integrity or performance of our Infrastructure; 3) collect information about our users in any manner; or 4) sell, rent, or charge for our Infrastructure. This does not prohibit you from providing your own Infrastructure to others, whether free or for a fee, as long as you do not violate these Conditions and AGPLv3 license, including the requirement to publish any modifications of the relay server software.

**Keeping your data secure**. SimpleX Chat is the first communication software that aims to be 100% private by design - server software neither has the ability to access your messages, nor it has information about who you communicate with. That means that you are solely responsible for keeping your device, your user profile and any data safe and secure. If you lose your phone or remove the Software from the device, you will not be able to recover the lost data, unless you made a back up. To protect the data you need to make regular backups, as using old backups may disrupt your communication with some of the contacts.

**Storing the messages on the device**. The messages are stored in the encrypted database on your device. Whether and how database passphrase is stored is determined by the configuration of the Software you use. The databases created prior to 2023 or in CLI (terminal) app may remain unencrypted, and it will be indicated in the app interface. In this case, if you make a backup of the data and store it unencrypted, the backup provider may be able to access the messages. Please note, that the desktop apps can be configured to store the database passphrase in the configuration file in plaintext, and unless you set the passphrase when first running the app, a random passphrase will be used and stored on the device. You can remove it from the device via the app settings.

**Storing the files on the device**. The files currently sent and received in the apps by default (except CLI app) are stored on your device encrypted using unique keys, different for each file, that are stored in the database. Once the message that the file was attached to is removed, even if the copy of the encrypted file is retained, it should be impossible to recover the key allowing to decrypt the file. This local file encryption may affect app performance, and it can be disabled via the app settings. This change will only affect the new files. If you later re-enable the encryption, it will also affect only the new files. If you make a backup of the app data and store it unencrypted, the backup provider will be able to access any unencrypted files. In any case, irrespective of the storage setting, the files are always sent by all apps end-to-end encrypted.

**No Access to Emergency Services**. Our Applications do not provide access to emergency service providers like the police, fire department, hospitals, or other public safety organizations. Make sure you can contact emergency service providers through a mobile, fixed-line telephone, or other service.

**Third-party services**. Our Applications may allow you to access, use, or interact with our or third-party websites, apps, content, and other products and services. When you use third-party services, their terms and privacy policies govern your use of those services.

**Your Rights**. You own the messages and the information you transmit through our Applications. Your recipients are able to retain the messages they receive from you; there is no technical ability to delete data from their devices. While there are various app features that allow deleting messages from the recipients' devices, such as _disappearing messages_ and _full message deletion_, their functioning on your recipients' devices cannot be guaranteed or enforced, as the device may be offline or have a modified version of the Software. At the same time, repudiation property of the end-to-end encryption algorithm allows you to plausibly deny having sent the message, like you can deny what you said in a private face-to-face conversation, as the recipient cannot provide any proof to the third parties, by design.

**License**. SimpleX Chat Ltd grants you a limited, revocable, non-exclusive, and non-transferable license to use our Applications in accordance with these Conditions. The source-code of Applications is available and can be used under [AGPL v3 license](https://github.com/simplex-chat/simplex-chat/blob/stable/LICENSE).

**SimpleX Chat Ltd Rights**. We own all copyrights, trademarks, domains, logos, trade secrets, and other intellectual property rights associated with our Applications. You may not use our copyrights, trademarks, domains, logos, and other intellectual property rights unless you have our written permission, and unless under an open-source license distributed together with the source code. To report copyright, trademark, or other intellectual property infringement, please contact chat@simplex.chat.

**Disclaimers**. YOU USE OUR APPLICATIONS AT YOUR OWN RISK AND SUBJECT TO THE FOLLOWING DISCLAIMERS. WE PROVIDE OUR APPLICATIONS ON AN “AS IS” BASIS WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, TITLE, NON-INFRINGEMENT, AND FREEDOM FROM COMPUTER VIRUS OR OTHER HARMFUL CODE. SIMPLEX CHAT LTD DOES NOT WARRANT THAT ANY INFORMATION PROVIDED BY US IS ACCURATE, COMPLETE, OR USEFUL, THAT OUR APPLICATIONS WILL BE OPERATIONAL, ERROR-FREE, SECURE, OR SAFE, OR THAT OUR APPLICATIONS WILL FUNCTION WITHOUT DISRUPTIONS, DELAYS, OR IMPERFECTIONS. WE DO NOT CONTROL, AND ARE NOT RESPONSIBLE FOR, CONTROLLING HOW OR WHEN OUR USERS USE OUR APPLICATIONS. WE ARE NOT RESPONSIBLE FOR THE ACTIONS OR INFORMATION (INCLUDING CONTENT) OF OUR USERS OR OTHER THIRD PARTIES. YOU RELEASE US, AFFILIATES, DIRECTORS, OFFICERS, EMPLOYEES, PARTNERS, AND AGENTS ("SIMPLEX PARTIES") FROM ANY CLAIM, COMPLAINT, CAUSE OF ACTION, CONTROVERSY, OR DISPUTE (TOGETHER, "CLAIM") AND DAMAGES, KNOWN AND UNKNOWN, RELATING TO, ARISING OUT OF, OR IN ANY WAY CONNECTED WITH ANY SUCH CLAIM YOU HAVE AGAINST ANY THIRD PARTIES.

**Limitation of liability**. THE SIMPLEX PARTIES WILL NOT BE LIABLE TO YOU FOR ANY LOST PROFITS OR CONSEQUENTIAL, SPECIAL, PUNITIVE, INDIRECT, OR INCIDENTAL DAMAGES RELATING TO, ARISING OUT OF, OR IN ANY WAY IN CONNECTION WITH OUR CONDITIONS, US, OR OUR APPLICATIONS, EVEN IF THE SIMPLEX PARTIES HAVE BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES. OUR AGGREGATE LIABILITY RELATING TO, ARISING OUT OF, OR IN ANY WAY IN CONNECTION WITH OUR CONDITIONS, US, OR OUR APPLICATIONS WILL NOT EXCEED ONE DOLLAR ($1). THE FOREGOING DISCLAIMER OF CERTAIN DAMAGES AND LIMITATION OF LIABILITY WILL APPLY TO THE MAXIMUM EXTENT PERMITTED BY APPLICABLE LAW. THE LAWS OF SOME JURISDICTIONS MAY NOT ALLOW THE EXCLUSION OR LIMITATION OF CERTAIN DAMAGES, SO SOME OR ALL OF THE EXCLUSIONS AND LIMITATIONS SET FORTH ABOVE MAY NOT APPLY TO YOU. NOTWITHSTANDING ANYTHING TO THE CONTRARY IN OUR CONDITIONS, IN SUCH CASES, THE LIABILITY OF THE SIMPLEX PARTIES WILL BE LIMITED TO THE EXTENT PERMITTED BY APPLICABLE LAW.

**Availability**. Our Applications may be disrupted, including for maintenance, upgrades, or network or equipment failures. We may discontinue some or all of our Applications, including certain features and the support for certain devices and platforms, at any time.

**Resolving disputes**. You agree to resolve any Claim you have with us relating to or arising from our Conditions, us, or our Applications in the courts of England and Wales. You also agree to submit to the personal jurisdiction of such courts for the purpose of resolving all such disputes. The laws of England govern our Conditions, as well as any disputes, whether in court or arbitration, which might arise between SimpleX Chat Ltd and you, without regard to conflict of law provisions.

**Changes to the conditions**. SimpleX Chat Ltd may update the Conditions from time to time. Your continued use of our Applications confirms your acceptance of our updated Conditions and supersedes any prior Conditions. You will comply with all applicable export control and trade sanctions laws. Our Conditions cover the entire agreement between you and SimpleX Chat Ltd regarding our Applications. If you do not agree with our Conditions, you should stop using our Applications.

**Enforcing the conditions**. If we fail to enforce any of our Conditions, that does not mean we waive the right to enforce them. If any provision of the Conditions is deemed unlawful, void, or unenforceable, that provision shall be deemed severable from our Conditions and shall not affect the enforceability of the remaining provisions. Our Applications are not intended for distribution to or use in any country where such distribution or use would violate local law or would subject us to any regulations in another country. We reserve the right to limit our Applications in any country. If you have specific questions about these Conditions, please contact us at chat@simplex.chat.

**Ending these conditions**. You may end these Conditions with SimpleX Chat Ltd at any time by deleting our Applications from your devices and discontinuing use of our Infrastructure. The provisions related to Licenses, Disclaimers, Limitation of Liability, Resolving dispute, Availability, Changes to the conditions, Enforcing the conditions, and Ending these conditions will survive termination of your relationship with SimpleX Chat Ltd.

Updated April 24, 2024
