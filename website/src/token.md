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
- network: TBC. There are several viable L2 candidates: Arbitrum One, Optimism, Polygon, etc.
- standard: ERC20, with contract-enforced supply and other limits on transactions and holdings (TBD based on modeling and testing, these will not be the usual freely tradable ERC20 tokens).

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

To cover server bills securely and privately.

With "free" centralized platforms:
- you lose security and privacy, because your data is used for advertising and sold.
- they de-platform inconvenient users, often based on frivolous complaints.
- you don't own all rights to your content.

Paying for server capacity is cheaper than "free" platforms. Our estimates based on the current costs are $5-10/month for 5,000 active message receivers (could be up to 50,000 listed community members) with 5-10 GB of files/media archive. These estimates are preliminary and may change.

### Why build on Ethereum blockchain?

Many people dislike Ethereum for its high energy usage and high transaction costs in the past. Also, blockchain transactions cannot provide privacy, can they? Why not use Monero (XMR) instead?

This was our assessment as well in the past. But the last three years changed it, addressing energy usage and transaction costs, and we've seen the growth of several L2 Ethereum blockchains. What made us decide that EVM-based blockchain is the best choice for the current stage is the planned rollout of zkEVM in 2025 with native support for zero-knowledge proofs.

[Our early ideas about Community Vouchers](https://github.com/simplex-chat/simplex-chat/blob/master/docs/rfcs/2024-04-26-commercial-model.md) and [the most recent design](https://github.com/simplex-chat/simplex-chat/blob/master/docs/rfcs/2025-10-23-vouchers.md) rely on zero-knowledge proofs, and as it will be natively supported, EVM blockchains provide a much better foundation to build Community Vouchers than building them from scratch &mdash; there is no need to re-invent solutions to problems that are already solved.

### Free Tier?

It will be determined after testing. Preliminarily, we expect up to 1,000 active message receivers (can be up to 10,000 listed members) and 500 MB storage to be available for free groups.

### Why Not Existing Crypto?

Existing cryptocurrencies do not allow the implementation of the required model for Community Vouchers. The price of cryptocurrencies is determined speculatively, and not based on costs. The fact that they can be freely traded and transferred exposes existing cryptocurrencies and tokens to financial regulations.

### How Might Vouchers Work?

Buy via app (like phone top-ups), with unused capacity shown in the app.

V1 would use hashed IDs for privacy and on-chain payments. Zero-knowledge proofs and in-app payments to be added later.

### Who will sell vouchers?

Community Vouchers will be sold via a smart contract in exchange for some other tradeable tokens, most likely stablecoins. We don't plan token emission, or any public or private pre-sales. And we won't have access to the funds from voucher sales &mdash; they will be locked in a smart contract, and only released once servers have provided capacity to the users, with the funds shared between server operators and SimpleX network, with operators receiving up to 60%, depending on trust evaluation. SimpleX network funds will be managed by smart contracts, and will be used for governance and development as defined by the contracts. Their price will be fixed based on server costs, with the exact economic model developed during testing phase.

### Who will control and upgrade smart contracts?

Community Vouchers will require several smart contracts for their functioning. During testing and development, SimpleX Chat will maintain and update all contracts. Once the network is ready for production, some critical contracts (e.g., those that control the funds) will be immutable, requiring a lot of testing and a security audit, and some less critical contracts will still be upgradeable based on a consensus model (e.g., multisig or voting).

It is always a journey from knowing that something is possible to knowing how exactly it will be done, and we are at the early stage of knowing it is possible. Specific designs would evolve, based on the input from legal and blockchain experts, and from the community &mdash; as everything else we develop for SimpleX network.

### Will I be able to sell or transfer Community Vouchers to other people?

Possibly, but with limits on the number of transactions and the time of holding.

Community Vouchers are designed with a single purpose &mdash; to facilitate payments for servers' capacity in a way that protects users' security. Smart contracts implementing them will restrict or completely prohibit trading. The specific parameters will be determined during design evolution and testing.

### Why use ERC20 specification &mdash; isn't it designed for freely tradable tokens?

[ERC20 specification](https://eips.ethereum.org/EIPS/eip-20) has wider scope. It is very simple, one of the earliest, and the most adopted standards on EVM blockchain. It defines tokens, but they don't have to be freely tradeable &mdash; the specification allows any extensions and restrictions implemented on top of it.

Because of its wide adoption, this specification is the right choice to build on, at least initially, as it will be compatible with all wallets and existing tools out of the box, making testing, development, and early adoption much easier.

## Disclaimer

This design is evolving &mdash; share your feedback!

This is not an investment offer. All details are subject to legal review.
