# SimpleX Chat Terms & Privacy Policy

SimpleX Chat is the first communication platform that has no user profile IDs of any kind, not even random numbers. Not only it has no access to your messages (thanks to open-source double-ratchet end-to-end encryption protocol and additional encryption layers), it also has no access to your profile and contacts - we cannot observe your connections graph.

If you believe that some of the clauses in this document are not aligned with our mission or principles, please raise it with us via [email](chat@simplex.chat) or [chat](https://simplex.chat/contact#/?v=1&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2FK1rslx-m5bpXVIdMZg9NLUZ_8JBm8xTt%23%2F%3Fv%3D1%26dh%3DMCowBQYDK2VuAyEALDeVe-sG8mRY22LsXlPgiwTNs9dbiLrNuA7f3ZMAJ2w%253D%26srv%3Dbylepyau3ty4czmn77q4fglvperknl4bi2eb2fdy2bh4jxtf32kf73yd.onion).

## Privacy Policy

SimpleX Chat Ltd. ("SimpleX Chat") uses the best industry practices for security and encryption to provide secure [end-to-end encrypted](./docs/GLOSSARY.md#end-to-end-encryption) messaging via private connections. This encryption cannot be compromised by the servers via [man-in-the-middle attack](./docs/GLOSSARY.md#man-in-the-middle-attack).

SimpleX Chat is built on top of SimpleX messaging and application platform that uses a new message routing protocol allowing to establish private connections without having any kind of addresses that identify its users - we don't use emails, phone numbers, usernames, identity keys or any other user identifiers to pass messages between the users.

SimpleX Chat security assessment was done in October 2022 by [Trail of Bits](https://www.trailofbits.com/about), and most fixes were released in v4.2.0 – see [the announcement](./blog/20221108-simplex-chat-v4.2-security-audit-new-website.md).

### Information you provide

#### User profiles

We do not store user profiles. The profile you create in the app is local to your device.

When you create a user profile, no records are created on our servers, and we have no access to any part of your profile information, and even to the fact that you created a profile - it is a local record stored only on your device. That means that if you delete the app, and have no backup, you will permanently lose all the data and the private connections you create with other users.

#### Messages and Files

SimpleX Chat cannot decrypt or otherwise access the content or even the size of your messages and files you send or receive. Each message is padded to a fixed size of 16kb. Each file is sent in chunks of 256kb, 1mb or 8mb via all or some of the configured file servers. Both messages and files are sent end-to-end encrypted, and the servers do not have technical means to compromise this encryption, because part of the [key exchange](./docs/GLOSSARY.md#key-exchange) happens out-of-band.

Your message history is stored only on your own device and the devices of your contacts. While the recipients' devices are offline SimpleX Chat temporarily stores end-to-end encrypted messages on the messaging (SMP) servers that are preset in the app or chosen by the users.

The messages are permanently removed from the preset servers as soon as they are delivered. Undelivered messages are deleted after the time that is configured in the messaging servers you use (21 days for preset messaging servers).

The files are stored on file (XFTP) servers for the time configured in the file servers you use (48 hours for preset file servers).

If a messaging or file servers are restarted, the encrypted message or the record of the file can be stored in a backup file until it is overwritten by the next restart (usually within 1 week).

#### Connections with other users

When you create a connection with another user, two messaging queues (you can think about them as about mailboxes) are created on chosen messaging servers, that can be the preset servers or the servers that you configured in the app, in case it allows such configuration. SimpleX uses separate queues for direct and response messages, that the client applications prefer to create on two different servers, in case you have more than one server configured in the app, which is the default.

At the time of updating this document all our client applications allow configuring the servers. Our servers do not store information about which queues are linked to your profile on the device, and they do not collect any information that would allow us to establish that these queues are related to your device or your profile - the access to each queue is authorized by two anonymous unique cryptographic keys, different for each queue, and separate for sender and recipient of the messages.

#### iOS Push Notifications

When you choose to use instant push notifications in SimpleX iOS app, because the design of push notifications requires storing the device token on notification server, the notifications server can observe how many messaging queues your device has notifications enabled for, and approximately how many messages are sent to each queue.

Notification server cannot observe the actual addresses of these queues, as a separate address is used to subscribe to the notifications. It also cannot observe who, or even how many contacts, send messages to you, as notifications are delivered to your device end-to-end encrypted by the messaging servers.

It also does not allow to see message content or sizes, as the actual messages are not sent via the notification server, only the fact that the message is available and where it can be received from (the latter information is encrypted, so that the notification server cannot observe it). You can read more about the design of iOS push notifications [here](https://simplex.chat/blog/20220404-simplex-chat-instant-notifications.html#our-ios-approach-has-one-trade-off).

#### Another information stored on the servers

Additional technical information can be stored on our servers, including randomly generated authentication tokens, keys, push tokens, and other material that is necessary to transmit messages. SimpleX Chat limits this additional technical information to the minimum required to operate the Services.

#### SimpleX Directory Service

[SimpleX directory service](./docs/DIRECTORY.md) stores: your search requests, the messages and the members profiles in the group. You can connect to SimpleX Directory Service via [this address](https://simplex.chat/contact#/?v=1-4&smp=smp%3A%2F%2Fu2dS9sG8nMNURyZwqASV4yROM28Er0luVTx5X1CsMrU%3D%40smp4.simplex.im%2FeXSPwqTkKyDO3px4fLf1wx3MvPdjdLW3%23%2F%3Fv%3D1-2%26dh%3DMCowBQYDK2VuAyEAaiv6MkMH44L2TcYrt_CsX3ZvM11WgbMEUn0hkIKTOho%253D%26srv%3Do5vmywmrnaxalvz6wi3zicyftgio6psuvyniis6gco6bp6ekl4cqj4id.onion).

#### User Support.

If you contact SimpleX Chat any personal data you may share with us is kept only for the purposes of researching the issue and contacting you about your case. We recommend contacting support [via chat](https://simplex.chat/contact#/?v=1&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2FK1rslx-m5bpXVIdMZg9NLUZ_8JBm8xTt%23%2F%3Fv%3D1%26dh%3DMCowBQYDK2VuAyEALDeVe-sG8mRY22LsXlPgiwTNs9dbiLrNuA7f3ZMAJ2w%253D%26srv%3Dbylepyau3ty4czmn77q4fglvperknl4bi2eb2fdy2bh4jxtf32kf73yd.onion), when it is possible.

### Information we may share

We operate our Services using third parties. While we do not share any user data, these third party may access the encrypted user data as it is stored or transmitted via our servers.

We use a third party for email services - if you ask for support via email, your and SimpleX Chat email providers may access these emails according to their privacy policies and terms of service.

The cases when SimpleX Chat may need to share the data we temporarily store on the servers:

- To meet any applicable law, regulation, legal process or enforceable governmental request.
- To enforce applicable Terms, including investigation of potential violations.
- To detect, prevent, or otherwise address fraud, security, or technical issues.
- To protect against harm to the rights, property, or safety of SimpleX Chat, our users, or the public as required or permitted by law.

At the time of updating this document, we have never provided or have been requested the access to our servers or any information from our servers by any third parties. If we are ever requested to provide such access or information, we will follow the due legal process.

### Updates

We will update this Privacy Policy as needed so that it is current, accurate, and as clear as possible. Your continued use of our Services confirms your acceptance of our updated Privacy Policy.

Please also read our Terms of Service below.

If you have questions about our Privacy Policy please contact us via [email](chat@simplex.chat) or [chat](https://simplex.chat/contact#/?v=1&smp=smp%3A%2F%2FPQUV2eL0t7OStZOoAsPEV2QYWt4-xilbakvGUGOItUo%3D%40smp6.simplex.im%2FK1rslx-m5bpXVIdMZg9NLUZ_8JBm8xTt%23%2F%3Fv%3D1%26dh%3DMCowBQYDK2VuAyEALDeVe-sG8mRY22LsXlPgiwTNs9dbiLrNuA7f3ZMAJ2w%253D%26srv%3Dbylepyau3ty4czmn77q4fglvperknl4bi2eb2fdy2bh4jxtf32kf73yd.onion).

## Terms of Service

You accept our Terms of Service ("Terms") by installing or using any of our apps or services ("Services").

**Minimal age**. You must be at least 13 years old to use our Services. The minimum age to use our Services without parental approval may be higher in your country.

**Accessing the servers**. For the efficiency of the network access, the apps access all queues you create on any server via the same network (TCP/IP) connection. Our servers do not collect information about which queues were accessed via the same connection, so we cannot establish which queues belong to the same users. Whoever might observe your network traffic would know which servers you use, and how much data you send, but not to whom it is sent - the data that leaves the servers is always different from the data they receive - there are no identifiers or ciphertext in common. Please refer to our [technical design document](https://github.com/simplex-chat/simplexmq/blob/master/protocol/overview-tjr.md) for more information about our privacy model and known security and privacy risks.

**Privacy of user data**. We do not retain any data we transmit for any longer than necessary to provide the Services. We only collect aggregate statistics across all users, not per user - we do not have information about how many people use SimpleX Chat (we only know an approximate number of app installations and the aggregate traffic through our servers). In any case, we do not and will not sell or in any way monetize user data.

**Operating our services**. For the purpose of operating our Services, you agree that your end-to-end encrypted messages are transferred via our servers in the United Kingdom, the United States and other countries where we have or use facilities and service providers or partners.

**Software**. You agree to downloading and installing updates to our Services when they are available; they would only be automatic if you configure your devices in this way.

**Traffic and device costs**. You are solely responsible for the traffic and device costs on which you use our Services, and any associated taxes.

**Legal and acceptable usage**. You agree to use our Services only for legal and acceptable purposes. You will not use (or assist others in using) our Services in ways that: 1) violate or infringe the rights of SimpleX Chat, our users, or others, including privacy, publicity, intellectual property, or other proprietary rights; 2) involve sending illegal or impermissible communications, e.g. spam.

**Damage to SimpleX Chat**. You must not (or assist others to) access, use, modify, distribute, transfer, or exploit our Services in unauthorized manners, or in ways that harm SimpleX Chat, our Services, or systems. For example, you must not 1) access our Services or systems without authorization, other than by using the apps; 2) disrupt the integrity or performance of our Services; 3) collect information about our users in any manner; or 4) sell, rent, or charge for our Services.

**Keeping your data secure**. SimpleX Chat is the first messaging platform that is 100% private by design - we neither have ability to access your messages, nor we have information about who you communicate with. That means that you are solely responsible for keeping your device and your user profile safe and secure. If you lose your phone or remove the app, you will not be able to recover the lost data, unless you made a back up.

**Storing the messages on the device**. The messages are stored in the encrypted database on your device. Whether and how database passphrase is stored is determined by the configuration of the application you use. Legacy databases created prior to 2023 or in CLI (terminal) app may remain unencrypted, and it will be indicated in the app. In this case, if you make a backup of the app data and store it unencrypted, the backup provider may be able to access the messages. Please note, that the beta version of desktop app currently stores the database passphrase in the configuration file in plaintext, so you may need to remove passphrase from the device via the app configuration.

**Storing the files on the device**. The files are stored on your device unencrypted. If you make a backup of the app data and store it unencrypted, the backup provider will be able to access the files.

**No Access to Emergency Services**. Our Services do not provide access to emergency service providers like the police, fire department, hospitals, or other public safety organizations. Make sure you can contact emergency service providers through a mobile, fixed-line telephone, or other service.

**Third-party services**. Our Services may allow you to access, use, or interact with third-party websites, apps, content, and other products and services. When you use third-party services, their terms and privacy policies govern your use of those services.

**Your Rights**. You own the messages and the information you transmit through our Services. Your recipients are able to retain the messages you receive from you; there is no technical ability to delete data from their devices. While there are various app features that allow deleting messages from the recipients' devices, such as _disappearing messages_ and _full message deletion_, their functioning on your recipients' devices cannot be guaranteed or enforced, as the device may be offline or have a modified version of the app.

**License**. SimpleX Chat grants you a limited, revocable, non-exclusive, and non-transferable license to use our Services in accordance with these Terms. The source-code of services is available and can be used under [AGPL v3 license](https://github.com/simplex-chat/simplex-chat/blob/stable/LICENSE)

**SimpleX Chat Rights**. We own all copyrights, trademarks, domains, logos, trade secrets, and other intellectual property rights associated with our Services. You may not use our copyrights, trademarks, domains, logos, and other intellectual property rights unless you have our written permission, and unless under an open-source license distributed together with the source code. To report copyright, trademark, or other intellectual property infringement, please contact chat@simplex.chat.

**Disclaimers**. YOU USE OUR SERVICES AT YOUR OWN RISK AND SUBJECT TO THE FOLLOWING DISCLAIMERS. WE PROVIDE OUR SERVICES ON AN “AS IS” BASIS WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, TITLE, NON-INFRINGEMENT, AND FREEDOM FROM COMPUTER VIRUS OR OTHER HARMFUL CODE. SIMPLEX DOES NOT WARRANT THAT ANY INFORMATION PROVIDED BY US IS ACCURATE, COMPLETE, OR USEFUL, THAT OUR SERVICES WILL BE OPERATIONAL, ERROR-FREE, SECURE, OR SAFE, OR THAT OUR SERVICES WILL FUNCTION WITHOUT DISRUPTIONS, DELAYS, OR IMPERFECTIONS. WE DO NOT CONTROL, AND ARE NOT RESPONSIBLE FOR, CONTROLLING HOW OR WHEN OUR USERS USE OUR SERVICES. WE ARE NOT RESPONSIBLE FOR THE ACTIONS OR INFORMATION (INCLUDING CONTENT) OF OUR USERS OR OTHER THIRD PARTIES. YOU RELEASE US, AFFILIATES, DIRECTORS, OFFICERS, EMPLOYEES, PARTNERS, AND AGENTS ("SIMPLEX PARTIES") FROM ANY CLAIM, COMPLAINT, CAUSE OF ACTION, CONTROVERSY, OR DISPUTE (TOGETHER, "CLAIM") AND DAMAGES, KNOWN AND UNKNOWN, RELATING TO, ARISING OUT OF, OR IN ANY WAY CONNECTED WITH ANY SUCH CLAIM YOU HAVE AGAINST ANY THIRD PARTIES.

**Limitation of liability**. THE SIMPLEX PARTIES WILL NOT BE LIABLE TO YOU FOR ANY LOST PROFITS OR CONSEQUENTIAL, SPECIAL, PUNITIVE, INDIRECT, OR INCIDENTAL DAMAGES RELATING TO, ARISING OUT OF, OR IN ANY WAY IN CONNECTION WITH OUR TERMS, US, OR OUR SERVICES, EVEN IF THE SIMPLEX PARTIES HAVE BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES. OUR AGGREGATE LIABILITY RELATING TO, ARISING OUT OF, OR IN ANY WAY IN CONNECTION WITH OUR TERMS, US, OR OUR SERVICES WILL NOT EXCEED ONE DOLLAR ($1). THE FOREGOING DISCLAIMER OF CERTAIN DAMAGES AND LIMITATION OF LIABILITY WILL APPLY TO THE MAXIMUM EXTENT PERMITTED BY APPLICABLE LAW. THE LAWS OF SOME JURISDICTIONS MAY NOT ALLOW THE EXCLUSION OR LIMITATION OF CERTAIN DAMAGES, SO SOME OR ALL OF THE EXCLUSIONS AND LIMITATIONS SET FORTH ABOVE MAY NOT APPLY TO YOU. NOTWITHSTANDING ANYTHING TO THE CONTRARY IN OUR TERMS, IN SUCH CASES, THE LIABILITY OF THE SIMPLEX PARTIES WILL BE LIMITED TO THE EXTENT PERMITTED BY APPLICABLE LAW.

**Availability**. Our Services may be interrupted, including for maintenance, upgrades, or network or equipment failures. We may discontinue some or all of our Services, including certain features and the support for certain devices and platforms, at any time.

**Resolving disputes**. You agree to resolve any Claim you have with us relating to or arising from our Terms, us, or our Services in the courts of England and Wales. You also agree to submit to the personal jurisdiction of such courts for the purpose of resolving all such disputes. The laws of England govern our Terms, as well as any disputes, whether in court or arbitration, which might arise between SimpleX Chat and you, without regard to conflict of law provisions.

**Changes to the terms**. SimpleX Chat may update the Terms from time to time. Your continued use of our Services confirms your acceptance of our updated Terms and supersedes any prior Terms. You will comply with all applicable export control and trade sanctions laws. Our Terms cover the entire agreement between you and SimpleX Chat regarding our Services. If you do not agree with our Terms, you should stop using our Services.

**Enforcing the terms**. If we fail to enforce any of our Terms, that does not mean we waive the right to enforce them. If any provision of the Terms is deemed unlawful, void, or unenforceable, that provision shall be deemed severable from our Terms and shall not affect the enforceability of the remaining provisions. Our Services are not intended for distribution to or use in any country where such distribution or use would violate local law or would subject us to any regulations in another country. We reserve the right to limit our Services in any country. If you have specific questions about these Terms, please contact us at chat@simplex.chat.

**Ending these Terms**. You may end these Terms with SimpleX Chat at any time by deleting SimpleX Chat app(s) from your device and discontinuing use of our Services. The provisions related to Licenses, Disclaimers, Limitation of Liability, Resolving dispute, Availability, Changes to the terms, Enforcing the terms, and Ending these Terms will survive termination of your relationship with SimpleX Chat.

Updated August 17, 2023
