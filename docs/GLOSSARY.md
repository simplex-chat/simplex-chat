# Glossary

Choosing a secure messenger requires understanding many technical terms, that many users, even quite technical, often misunderstand. This list is aiming to fill this knowledge gap. Please suggest any additions.

## Blockchain

## Break-in recovery

[Post-compromise security](#post-compromise-security).

## Centralized network

## Content padding

## Decentralized network

## Double ratchet algorithm

It is used by two parties to exchange [end-to-end encrypted](#end-to-end-encryption) messages. The parties will use some [key agreement protocol](#key-agreement-protocol) to agree on the initial shared secret key.

Double Ratchet protocol provides [perfect forward secrecy](#perfect-forward-secrecy) and [post-compromise security](#post-compromise-security). It is [designed by Singal](https://signal.org/docs/specifications/doubleratchet), and used in SimpleX Chat and many other secure messengers. Most experts consider the state-of-the-art encryption protocol in message encryption.

## End-to-end encryption

Communication system where only the communicating parties can read the messages. It is designed to protect message content from any potential eavesdroppers – telecom and Internet providers, malicious actors, and also the provider of the communication service.

End-to-end encryption requires agreeing cryptographic keys between the sender and the recipient in a way that no eavesdroppers can access the agreed keys. See [key agreement protocol](#key-agreement-protocol).

## Federated network

## Forward secrecy

## Key agreement protocol

Key exchange is a process of agreeing cryptographic keys between the sender and the recipient(s) of the message. It is required for [end-to-end encryption](#end-to-end-encryption) to work.

## Key exchange

[Key agreement protocol](#key-agreement-protocol).

## Man-in-the-middle attack

## Merkle tree

## Peer-to-peer network

## Perfect forward secrecy

## Post-compromise security

The quality of the end-to-end encryption scheme allowing to recover security against a passive attacker who observes encrypted messages after compromising one (or both) of the parties. Also known as recovery from compromise or break-in recovery.

## Recovery from compromise

[Post-compromise security](#post-compromise-security).
