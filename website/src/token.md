---
layout: layouts/token.html
title: "Community Vouchers: Your Freedom and Security"
permalink: "/token/index.html"
---

# SimpleX Community Vouchers

We're developing Community Vouchers as a way to enable secure payments to server operators, to make communities sustainable.

Group or channel owners can select network operators for better reliability and censorship resistance than with traditional online publishing methods.

These vouchers are blockchain utility tokens &mdash; to focus purely on server capacity usage, like prepaid telephone cards.

## SMPX: Community Voucher Token planned for 2026

<a href="javascript:void(0);" data-show-overlay="mint-simplex-nft" class="open-overlay-btn"><img src="/img/design_3/simplex_nft_smpx.jpg" width="200" class="float-to-right" style="border-radius: 10px;"></a>

SMPX token is v1 of Community Vouchers. We are aiming to launch it in 2026.

<a href="javascript:void(0);" data-show-overlay="mint-simplex-nft" class="open-overlay-btn">Mint a free SimpleX NFT</a> on Ethereum network (mainnet) for SMPX testnet access and feedback. The NFT is limited to 1 per wallet, non-transferable.

**Preliminary token overview**
- full name: **SimpleX Community Voucher**.
- symbol: SMPX.
- network: TBC. There are several viable L2 candidates: Arbitrum One, Optimism, Polygon, etc., and we are considering other networks as well.
- standard: ERC20, with contract-enforced supply and other limits on transactions and holdings (TBD based on modeling and testing, these will not be freely tradable ERC20 tokens).

**Potential utilities**
- Server messaging and file capacity for large channels and communities beyond free tier (see FAQ).
- Names in SimpleX Name System.

We're working with blockchain and legal experts for feasibility and compliance, aiming to launch testnet in 2026. Details may change based on input. More in our upcoming whitepaper draft.

To receive updates, sign up via email or connect to us [via SimpleX Chat](https://smp6.simplex.im/a#lrdvu2d8A1GumSmoKb2krQmtKhWXq-tyGpHuM7aMwsw).

## Community Vouchers FAQ

These are early insights into how Community Vouchers can work &mdash; some of these ideas are still vague; they will evolve based on your feedback and testing.

### Why Community Vouchers?

<img src="/img/design_3/community_vouchers_light.jpg" width="38%" class="float-to-right dark:hidden">

<img src="/img/design_3/community_vouchers_dark.jpg" width="38%" class="float-to-right hidden dark:block">

To cover server costs securely and privately.

With "free" centralized platforms:
- you lose security and privacy, because your data is used for advertising and sold.
- they de-platform inconvenient users, often based on frivolous complaints.
- you don't own all rights to your content.

Paying for server capacity is cheaper than "free" platforms. Our estimates based on the current costs are $5-10/month for 5,000 active message receivers (could be up to 50,000 listed community members) with 5-10 GB of files/media archive. These estimates are preliminary and may change.

### Free Tier?

It will be determined after testing. Preliminarily, we expect up to 1,000 active message receivers (can be up to 10,000 listed members) and 500 MB storage to be available for free groups.

"Active message recipient" in this model is a group member who periodically connects to the network, and receives group messages. Members who are listed but don't open the group for some time, for example two weeks, will stop receiving all group messages even when they are connected to the network. This is an evolving design that will balance security for group members and owners, to avoid inflated expenses, and to present realistic membership statistics to the group owners and to prospective members.

Private messaging with contacts and in private groups within "fair use" limits we apply today will remain free:
- there can be up to 128 undelivered messages per destination,
- the undelivered messages are stored up to 21 days,
- files up to 1 GB can be sent for free,
- files are available for download for up to 2 days.

Larger limits may be offered in paid tier, but it is not planned initially &mdash; the focus of Community Vouchers is to create a commercial model for communities.

### How Might Vouchers Work?

Buy via app (like phone top-ups), with unused capacity shown in the app.

Testnet is likely to use hashed IDs for privacy and on-chain payments, to validate the pricing and economic model. Zero-knowledge proofs and in-app payments will be added by the time production network is launched.


### Will self-hosted servers still be supported by SimpleX network?

Yes, absolutely. Not only we will continue to provide support to self-hosted servers, but we will improve it. We see network decentralization and server portability as exceptionally important, and while we need to develop a robust commercial model for the servers, we still need community supported servers to function, with all servers operating in single network.

It will be possible for all users:
- to join paid groups that use Community Vouchers via your own servers.
- to join groups that are run on free community-hosted servers.
- to create new server operators (collections of servers operated by one entity), so that the same features that are now available only for preset servers will be available to all servers very soon.

### What problems Community Vouchers solve that other payment methods can't?

Community Vouchers implemented via smart contracts on blockchain solve these problems:
- unlinkability of the voucher purchase and usage - to confirm ownership smart contracts will use zero-knowledge proofs, rather than visible transfers between blockchain addresses. While any blockchain observers may see that a given address purchased a voucher, they will not see how vouchers are used.
- server operators cannot fail to provide infrastructure &mdash; the funds will be locked in a smart contract until it is provided.
- codify the agreement about how revenue is shared between server operator and the network, so that it depends not on trust, but on cryptography.

### How is it possible to provide privacy on public blockchain?

In the same way it is possible to provide private communications on the public Internet, as SimpleX network does.

Our commitment to users' privacy and security remains as strong as ever, and we plan to bring the practical expertise of building private communication protocols over the last 5 years to how we develop the technology for the blockchain.

While specific designs are in early stages, here are some of the principles that we will follow to ensure privacy:
- each voucher purchase will be associated with a new blockchain address. There will be no per-user addresses, as wallets use. So the cornerstone of SimpleX network design - no user profile IDs - will be followed for blockchain development as well.
- all operations on blockchain will be supported by network servers that will run full blockchain nodes. For important requests, such as name resolution, the clients will use 2 or 3 independent servers, to ensure protection from MITM attacks.
- blockchain operations will be proxied, in the same way as it happens with private message routing.

We will be publishing the whitepaper with this design. It will provide an unprecedented level of security and privacy for blockchain applications, irrespective of which chain we choose to use.


### How many Community Vouchers will be pre-sold and available during public sale?

None. There will be no Community Vouchers pre-sold or in any other way made available to the team, or to investors or to the public.

Any blockchain token that is pre-sold to insiders and/or to the public with the objective to raise funds to develop technology is not a utility token, regardless of how it's named &mdash; it becomes an investment contract that passes [Howey test](https://www.investopedia.com/terms/h/howey-test.asp).

This is not what we are doing. Community Vouchers are restricted utility tokens, not an investment contract. They will be only issued on demand to people who want to pay for network servers, at a fixed price.

### Who will sell vouchers?

Community Vouchers will be sold via a smart contract in exchange for some other tradeable tokens, most likely stablecoins. We don't plan token emission, or any public or private pre-sales. And we won't have access to the funds from voucher sales &mdash; they will be locked in a smart contract, and only released once servers have provided capacity to the users, with the funds shared between server operators and SimpleX network, with operators receiving up to 60%, depending on trust evaluation. SimpleX network funds will be managed by smart contracts, and will be used for governance and development as defined by the contracts. Their price will be fixed based on server costs, with the exact economic model developed during testing phase.

### Who will control and upgrade smart contracts?

Community Vouchers will require several smart contracts for their functioning. During testing and development, SimpleX Chat will maintain and update all contracts. Once the network is ready for production, some critical contracts (e.g., those that control the funds) will be immutable, requiring a lot of testing and a security audit, and some less critical contracts will still be upgradeable based on a consensus model (e.g., multisig or voting).

It is always a journey from knowing that something is possible to knowing how exactly it will be done, and we are at the early stage of knowing it is possible. Specific designs would evolve, based on the input from legal and blockchain experts, and from the community &mdash; as everything else we develop for SimpleX network.

### Will I be able to sell or transfer Community Vouchers to other people?

Possibly, but with limits on the number of transactions and the time of holding.

Community Vouchers are designed with a single purpose &mdash; to facilitate payments for servers' capacity in a way that protects users' security. Smart contracts implementing them will restrict or completely prohibit trading. The specific parameters will be determined during design evolution and testing.

### Why Not Existing Crypto?

Existing cryptocurrencies do not allow the implementation of the required model for Community Vouchers. The price of cryptocurrencies is determined speculatively, and not based on costs. The fact that they can be freely traded and transferred exposes existing cryptocurrencies and tokens to financial regulations.

The existing cryptocurrencies such as XMR, BTC and some others are very likely to be accepted as payment for Community Vouchers, via bridges, but they cannot be used in the foundation of the system, because they are not as flexible as smart contracts, and cannot directly support the model we are developing.

### Why build on Ethereum blockchain?

Many people dislike Ethereum for its high energy usage and high transaction costs in the past. Also, blockchain transactions cannot provide privacy, can they? Why not use Monero (XMR) instead?

This was our assessment as well in the past. But the last three years changed it, addressing energy usage and transaction costs, and we've seen the growth of several L2 Ethereum blockchains. What made us decide that EVM-based blockchain is the best choice for the current stage is the planned rollout of zkEVM in 2025 with native support for zero-knowledge proofs.

[Our early ideas about Community Vouchers](https://github.com/simplex-chat/simplex-chat/blob/master/docs/rfcs/2024-04-26-commercial-model.md) and [the most recent design](https://github.com/simplex-chat/simplex-chat/blob/master/docs/rfcs/2025-10-23-vouchers.md) rely on zero-knowledge proofs, and as it will be natively supported, EVM blockchains provide a much better foundation to build Community Vouchers than building them from scratch &mdash; there is no need to re-invent solutions to problems that are already solved.

### Have you considered other blockchains?

We are actively considering which blockchain to build on. Ethereum ecosystem is the most widely adopted, and has very mature systems and tools, and it appears sufficient, but it has its downsides, as does everything. So we are not yet committed to Ethereum.

### Why use ERC20 specification &mdash; isn't it designed for freely tradable tokens?

[ERC20 token specification](https://eips.ethereum.org/EIPS/eip-20) has wider scope. It is very simple, one of the earliest, and the most adopted standard on EVM blockchain. It defines tokens, but they don't have to be freely tradeable &mdash; the specification allows any extensions and restrictions implemented on top of it.

Because of its wide adoption, this specification is the right choice to build on, at least initially, as it will be compatible with all wallets and existing tools out of the box, making testing, development, and early adoption much easier.

### If you build on another blockchain, how the NFT will be used to provide access?

We can take into account the list of addresses that hold NFTs and provide access to testnet on any blockchain via a cryptographic signature. That is the reason the NFT is deployed on Ethereum mainnet and not on some of L2 chains. We don't yet know at this stage which L2 testnet will be used.

## Disclaimer

This design is evolving &mdash; please share your feedback.

This is not an investment offer. All details are subject to legal review.
