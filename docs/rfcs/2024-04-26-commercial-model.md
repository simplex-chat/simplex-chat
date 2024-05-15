# Commercial model for SimpleX communication network

## Problem

SimpleX two-tier network design provides a _potential_ for a much higher degree of decentralization and privacy than a p2p network can achieve even theoretically. This is a very strong statement, and its formal proof is out of scope of this document, please see the old comparison of SimpleX network and p2p network designs [here](../SIMPLEX.md#comparison-with-p2p-messaging-protocols).

The main downside of most, if not all, p2p networks is the lack of (or very limited) asynchronous message delivery that is critically important both for the network usability and for privacy and protection against traffic correlation attacks.

But while two-tier network design has a _potential_ for higher privacy, this potential is hard to realize as it does not have an in-built mechanism for network operator incentives. All SimpleX network relays preset in the app are operated by SimpleX Chat Ltd., and while there are probably over 1000 self-hosted and community-hosted messaging relays - not having a single register of such relays is important for true decentralization, so we cannot know the exact number - it is probably a valid argument that a substantial part of SimpleX network traffic is provided by preset relays. Again, while we don't know the exact share, it is probably not much less than 40-50% and probably not much more than 65-75% - which is a very large share in any case for a single entity.

For SimpleX network to achieve the level of decentralization that is designed, the applications have to be able to offer multiple network operators via the app who have commercial incentives to operate these relays.

Traditional commercial models for consumer Internet products rely on some of these ideas:
1. Offer service for free, sell users' data. We obviously had enough of such Internet and can't wait to see it implode, lose all users, and become illegal.
2. Create a cryptocurrency or issue cryptocurrency tokens, sell them for real money, and use this money to fund service operation. While many people believe that this is the future of the Web, calling it Web3, we see it as a technological dead end, that although it provides a great platform for speculation and a playground to test such ideas as smart contracts and consensus algorithms, all of which can be used outside of the context of cryptocurrency blockchains, is not a sustainable technological foundation for a general purpose communication or information management system due to its inherent regulatory risks and distorted commercial incentives. Moxie Marlinspike wrote [a good critique of Web3](https://moxie.org/2022/01/07/web3-first-impressions.html).
3. Freemium models where some users pay for the service and, effectively, sponsor the users of the free tier. This model is free from the downsides of the previous two options, and there are many commercial services operated on this model, but this model is suboptimal for privacy in the worst case, and in the best case it still does not achieve decentralization, as whoever charges the money (the app provider) should also provide the infrastructure.

In the mentioned critique of Web3 Moxie wrote: _We should accept the premise that people will not run their own servers by designing systems that can distribute trust without having to distribute infrastructure_.

This statement is interesting, as it contains the correct premise - that most people do not want to and won't run their own servers - but it reaches an incorrect and limited conclusion, that the only way to provide value is by figuring out how to decentralize the trust without decentralizing infrastructure. I completely disagree that this conclusion is the only one possible, and the offered solution actually offers a way for extreme infrastructure decentralization - when not just users are distributed across providers, as happens with federated designs, but each conversation between two users is supported by infrastructure of 4-6 different independent operators - and also provides commercial incentives for these operators.

## Solution design requirements

So, we want to find a solution that will:
1. Offer extreme decentralization, without any kind of central authority (unlike Tor that has central authority, and unlike p2p networks that have a single addressing space) or any kind of centralized state (unlike cryptocurrency blockchains that have a centralized single state that the whole network should reach consensus about).
2. Offer low barrier for entry for infrastructure operators.
3. Offer extreme data and infrastructure portability, when infrastructure operators offer standardized primitives used by client applications - such as messaging queues and file chunks in SimpleX network. Migrating from one operator to another should not be just possible or simple, it should happen continuously and automatically, all the time, without any user action, when files and conversations move from one set of operators to another, similarly to how data moves around on SSD drives - to balance the load on the whole network, to provide reliability and redundancy, and to ensure that infrastructure operators have zero control of users and their data, and have very limited knowledge of users activity. This limited knowledge both achieves users' privacy and reduces the legal responsibility of infrastructure operators to the level of network operators.
4. Offer commercially profitable model for infrastructure operators.

If these requirements are satisfied it would achieve a radical shift of control from infrastructure operators of today (that is achieved via SaaS model, when infrastructure operators are also software vendors, and software is provided as a service, which seems to be the root cause of corruption of Internet services - the process that Cory Doctorow refers to as [enshittification](https://www.youtube.com/watch?v=q118B_QdP2k)) to the software users, who use client software to directly consume low level infrastructure primitives of commoditized infrastructure operators that have zero control of how these primitives are used, other than pricing, that can be determined in a competitive process (e.g., via the real-time reverse auction).

## Solution concept

There are three types of participants in the network:
- software vendors (e.g., SimpleX Chat ltd.).
- infrastructure operators - commercial entities that have agreement with software vendors - they run server software provided by software vendors.
- users - they run client software.

These roles can obviously overlap, but strategically it is better if they don't, e.g. we would benefit from not operating infrastructure, and users would also achieve better metadata privacy by not running the servers.

To use SimpleX network client software needs to provision simple infrastructure resources - such as, create a messaging queue to receive the messages, create or use a session from a sending proxy to a particular destination relay, or to upload a file chunk that will be stored for a defined number of days. These resources are very cheap to provide, their price could be a very tiny fraction of a cent.

For the users' client software to be able to provision these resources, the software vendor will issue infrastructure certificates to the users. Some number of them can be provided for free, a larger number can be sold for a monetary payment (can be in-app purchase, or cryptocurrency payment, or any other process). Software vendor will keep a private record of issued certificates. Technically, certificates can be usual cryptographic certificates that sign a public key presented by the client (while the client holds the private key).

These certificates cannot be used as a cryptocurrency, as there is no public record of all issued certificates, and they can only be "spent" once with the infrastructure operator that has a commercial agreement with a software vendor.

When the client wants to provision infrastructure resource it signs the request using the private key (a public counterpart of which was signed by the vendor's root certificate) and present this signed request together with the client's certificate to infrastructure operator. Operator validates the signature and certificate using vendor's root certificate and immediately provisions the resource.

Operator then within a limited time presents its own signed request for payment to the software vendor - it would include the original request, without specifying which resource was provisioned, but only confirming that it was to this vendor. Software vendor can either confirm the acceptance and void the certificate or inform the vendor that this certificate was previously used (double spend), in which case the operator will stop provisioning the resource.

Pros:
- this design decouples payments from resource allocation, providing better privacy, and decentralizes the infrastructure.
- this design achieves extreme provider portability and lack of control of user and user data.

Cons:
- this design creates a strong dependence on software vendor's payment infrastructure availability - even though there can be many software vendors using this approach in a compatible way, it still creates a strong dependence of client software functioning on a single vendor.
- as described, this design allows software vendor to correlate payments of a given software user to specific infrastructure operators.

Problem 2 can be solved by using some sort of [zero-knowledge proofs](https://en.wikipedia.org/wiki/Non-interactive_zero-knowledge_proof), where infrastructure operator can prove to software vendor that 1) they received a valid request 2) software vendor can also prove that the same certificate was not presented before, but cannot determine which certificate it was.

Problem 1 can possibly be solved by delegating the right to issue a limited number of certificate to infrastructure operators, and making all records in the private blockchain accessible and writable by the operators using the agreements with the vendor that would also be visible on this chain, so that the operators cannot issue more certificates than agreed. In this case the payments will be made not by the software vendor to the operators but by the operators to the vendor, by compensating the buy/sell price difference.

This is a concept of design, rather than the actual design, and the details of cryptographic primitives and consensus algorithms for this chain are out of scope. It is important that this design limits the number of operations with each certificate to three:
1. certificate issued to the user, based on the rules agreed with the software vendor.
2. certificate is used as a micro-payment for infrastructure resource (it cannot be transferred to any other user without risks of double spend).
3. certificate can only be voided by software vendor (or delegate who issued it), so it cannot be used directly as a payment.

If we see cryptocurrency as similar to money, with similar regulations, this cryptographic primitive is close to gift cards, that have zero monetary value and can be only exchanged to a specific resource/service, thus avoiding the usual regulatory risks, and also avoiding speculative hype that could decouple the value of the certificates from the price of the infrastructure.
