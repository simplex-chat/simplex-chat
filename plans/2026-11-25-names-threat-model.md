# Names: threat model of the page and the store service

The threat model of the names page and the store service after the changes of [2026-11-25-names-hardening.md](./2026-11-25-names-hardening.md), in the form of the [SimpleX Network threat model](https://github.com/simplex-chat/simplexmq/blob/stable/protocol/security.md#threat-model).

Buyers order names on the names page. The page signs each registration with the buyer's name key and shows the payment details; the store service takes the payment and submits the registration on chain with its registrar key. The page is built from simplex.domains and served by GitHub Pages, and it holds the deployment's values: contracts, chain, terms, prices, dispute days, the mode, the Stripe key, the Bitcoin keys and the Monero addresses.

## Parties

| Party | Holds |
|---|---|
| Buyer's browser | the name code; the name key, derived from it (BIP-39, m/44'/60'/0'/0/0, as in the app); the names page keeps name codes in its origin's storage |
| Names page | its code and the held deployment values; it signs the order, `RegisterName`, the terms acceptance, record edits and requests for an order with the name key |
| Card page | the Stripe publishable key; Stripe.js, on its own origin, apart from the name codes |
| Store service host | the database of orders and codes; the registrar key, which spends the sponsor's allowance and pays gas; the Stripe restricted key (PaymentIntents write, Charges read); the BTCPay API key; the Bitcoin extended public key and the Monero private view key |
| Contracts | the allowance, set by the guardian Safe; dispute days from 14 to 120; the guardian Safe can end a registration within its dispute period |
| Etherscan | an independent view of the chain: the registrar's tokens and their holders |

## Global assumptions

- The buyer's device and browser are not compromised, and the buyer keeps the name code.
- The deployed contracts are as verified, and the keys that own and guard them, those of the admin and guardian Safes included, are not compromised.
- The spending keys of SimpleX's Bitcoin and Monero wallets, and the Stripe account's dashboard, are not on the service host.
- The Stripe account has Connect disabled, and the service's key has only the permissions above.
- Etherscan, or another explorer the buyer chooses, shows the chain's state.
- The cryptographic primitives in use are not broken.

## An attacker who controls the service

The host: code, database, keys and BTCPay. The pages served by GitHub are intact.

*can:*

- refuse, delay or drop orders, and show wrong phases, balances, refunds and search answers.

- put the page into downtime.

- take the ETH in the registrar's wallet, the gas budget.

- register any free name to an address it controls, paid by the allowance, with dispute days of 14 or more, until the allowance is spent; a name a buyer paid for included.

- take a buyer's payment and leave the name unregistered; the money is in SimpleX's wallets or Stripe account, and support refunds it.

- quote a crypto amount at its own exchange rate; the payment still reaches SimpleX's address.

- issue name codes and register names with them, within the allowance.

- learn every lookup and order, with the buyer's IP address and links.

- see every Bitcoin receiving address and every incoming Monero payment, from the extended public key and the view key.

*cannot:*

- direct a crypto payment to an address outside SimpleX's wallets, or a card payment to another Stripe account: the page shows only addresses it derives from its held keys, and the card page mounts only PaymentIntents of its held key with the held amount.

- show an invoice for another amount than the held price.

- make the page sign a registration for other contracts, chain, terms, dispute days, resolver or owner.

- learn the name code or the name key, change a registered name's records, or transfer it: each needs the owner's signature.

- spend more than the allowance, or register with dispute days below 14.

- keep the guardian Safe from ending, within its dispute period, a registration it made for itself.

- hide from the buyer, on Etherscan, that the name is not held by the buyer's name address.

- change the pages.

## An attacker who controls the names page

Through the simplex.domains repository, GitHub Pages or DNS. The service is intact.

*can:*

- read the name codes of buyers who open it, and with them sign anything as those owners: change records and transfer names.

- show its own payment addresses or card form, and take the payment.

- sign a buyer's order for a name address it controls, so the service registers the paid name to that address.

- show any status, and a false Etherscan link and name address.

- send every lookup and the buyer's IP address elsewhere.

*cannot:*

- make the service register a name without a payment the service received or a valid code.

- spend the allowance or the registrar's gas beyond what the service does for paid orders and codes.

- reach owners who do not open it.

- change what Etherscan shows when the buyer opens it directly.

## An attacker who controls the card page

Through the card page's repository, GitHub Pages or the DNS of `pay.simplex.domains`. The names page and the service are intact.

*can:*

- show its own card form in the frame, and take the card payments made in it.

- tell the names page that a payment was made; the order's status still follows the service.

*cannot:*

- read the name codes, or sign anything as an owner: the card page runs on another origin.

- change the crypto payment details the names page shows.

- make the service register a name without a payment the service received or a valid code.

## An attacker who controls both

The service host and the names page.

*can:*

- do all of the above together: take payments, register names to addresses it controls, and take over the names of owners who open the page.

*cannot:*

- spend more than the allowance, or register with dispute days below 14.

- keep the guardian Safe from ending, within their dispute periods, registrations that were not paid for.

- move money already in SimpleX's wallets or Stripe account.

- reach owners who do not open the page.

- change the contracts or what Etherscan shows.

## Someone who has an order reference

Anyone who sees an order's reference or link. The reference is the service's own invoice or registration id; the payment providers receive only "name:" and the order's hash. The service and the pages are intact.

*cannot:*

- learn whether the reference names an order: an unsigned request for an order gets the same answer as a request for an unknown one.

- learn the order's name, name address, status, price or links: every request that names an order is signed with the order's name key.

- cancel the order's invoice, retry the order or choose a name for it.
