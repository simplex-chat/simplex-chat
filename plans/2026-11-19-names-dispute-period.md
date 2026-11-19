# Names Conditions: dispute period

Repository: simplex-chat/simplex-chat.
Branch: `ab/names-conditions-dispute`.
Base: master `44a193b54` (#8266).

Requirement: the Names Conditions describe the registry's dispute period, as the registry contracts implement it.

## Problem

A holder accepts docs/NAMES-CONDITIONS.md on chain. It states that changing the record of a name, creating subnames and transferring control require the holder's key, and that a name can be extended until 90 days after its period ends.

The registry contracts change on brenzi-agent/ens-contracts, branch `ab/dispute-period`, for simplex-network/ens-contracts:

- a registration through a registrar with an allowance on the sponsor has a dispute period of 14 to 120 days, its end stored in the registry;
- before that end, the registry's guardian or admin can end the registration, its record and its subnames;
- before that end, the name cannot be extended;
- a registration paid to the registry in ETH by the registrant's own transaction, or assigned by the registry's admin, is not subject to a dispute period.

The conditions would describe a registry that holders do not get.

## Registration states

| State | Entered by | What the registry allows |
|---|---|---|
| disputable | registration through a registrar with an allowance; release of a reserved name in its dispute period | transfer; end by the guardian or admin; reservation |
| final | registration paid in ETH by the registrant; assignment by the admin; end of the dispute period; release of a reserved name after its dispute period | transfer; extension; reservation |
| reserved | reservation | transfer until the registration period ends; end by the guardian or admin while still disputable; release by the admin |
| withheld | end of the registration of a reserved name; 90 days after the registration period of a reserved name; reservation of an available name | assignment or release by the admin |
| available | end of the registration of an unreserved name; 90 days after the registration period of an unreserved name; release of a withheld name | registration; reservation |

```mermaid
stateDiagram-v2
  [*] --> disputable: registered through a registrar with an allowance
  [*] --> final: paid in ETH by the registrant, or assigned by the admin
  disputable --> disputable: transferred
  disputable --> final: dispute period ends
  disputable --> available: ended on a ground the conditions state
  disputable --> reserved: reserved
  final --> final: extended or transferred
  final --> reserved: reserved
  final --> available: 90 days after the registration period
  reserved --> disputable: released by the admin, while disputable
  reserved --> final: released by the admin, after the dispute period
  reserved --> withheld: ended, while disputable
  reserved --> withheld: 90 days after the registration period
  withheld --> final: assigned by the admin
  withheld --> available: released by the admin
  available --> withheld: reserved
  available --> disputable: registered through a registrar with an allowance
  available --> final: paid in ETH by the registrant
```

## Changes

docs/NAMES-CONDITIONS.md:

| Place | Change |
|---|---|
| "SimpleX names" | "Reserved names" defined, also while registered; an assignment ends the reservation |
| your key | the next point as its exception |
| dispute period | new, second point |
| period and extension | after the dispute period, if any, unless reserved; "the period" named "the registration period" |
| reservation after a disputed payment | new, last point |

The dispute period point states:

- which registrations have one, its length, and its end in the registry;
- within that period, also after a transfer, the registry's admin or guardian can end the registration, its record and its subnames without the holder's key;
- in a sentence of its own, the only grounds on which they end it;
- registration again at once, unless the name is reserved.

The last point ties the reservation to a payment for the current registration or its extension.

The other sections are unchanged. A separate change, for the registry's upgradeable contracts, edits the section "Registry"; master is merged into the pull request merged second.

## After the merge

The website publishes docs/*.md on each push to master (web.yml), so the text appears there at the merge; the merge is timed close to the governance batch. Holders accept the version linked on chain: the name policy's general and community terms links are set to this file at the merge commit of the later of this change and the change for the upgradeable contracts:

1. tagged `general-terms-vN` and `community-terms-vN`;
2. mirrored on git.simplex.chat;
3. set in the same governance batch as `setSponsor` of the sponsor that takes `disputeDays`, so that every registration with a dispute period is made under this version.
