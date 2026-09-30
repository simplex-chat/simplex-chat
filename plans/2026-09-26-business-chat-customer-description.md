# Customer profile description must not become the business chat welcome message

## Problem

A support bot running a business address had a customer's personal introduction ("Hi, I'm …") show up in that customer's business chat as a message from the business ("Ask SimpleX Team", owner). The customer never wrote it into the chat, and team members added to the chat saw it as the business's own message. We confirmed the text was the customer's profile description.

## Cause

`Profile.description` was added in #7256. The same PR also copied it into the group profile of a business chat. This happens in `updateGroupProfileFromMember`, called from `updateBusinessChatProfile` in `processMemberProfileUpdate` when the main business member's profile changes.

That copy is correct on the customer side (`BCBusiness`), where the business's description becomes the chat's welcome message. On the business side (`BCCustomer`), the same code copied the customer's description into `GroupProfile.description`. That field serves as the group welcome message (`Types.hs`: "this has been repurposed as welcome message").

The business then passes it on as its own message:

- When the business adds a member who is not the customer (a team member, or a bot's assistant), `sendHistory` runs.
- `sendHistory` appends a welcome element: a new `XMsgNew (MCText description)` authored by the host with a fresh random shared message ID. Invited members have no join-request welcome message ID, so the element is always included.
- Every member added later therefore sees the customer's bio as a message from the business.
- The welcome element is re-sent to each member added later, even after the customer changes or removes the bio.

To trigger it, the customer changes their profile description while in the business chat and then sends a message there. The profile update goes out lazily with the next message.

## Fix

`updateGroupProfileFromMember` now takes the `BusinessChatType`:

- **`BCBusiness`** (customer side): unchanged. The business's description is copied as the welcome message.
- **`BCCustomer`** (business side): the customer's display name, full name, short description and image are still copied. The chat's existing description (welcome message) is kept.

The business side keeps its existing description rather than clearing it, so a welcome message the business set itself is preserved. `createBusinessRequestGroup` never copies the customer's description either, so after the fix the business side never takes its welcome message from the customer.

Existing chats that already have a customer description stored as their welcome message are left unchanged. No migration is included.

## Test

`testBusinessCustomerDescription` (`tests/ChatTests/Profiles.hs`):

1. The customer connects to a business address.
2. The customer sets a profile description and sends a message.
3. The business adds a team member.
4. The test checks that the business side reports no welcome message change, and that the team member receives only the real history.

Without the fix, the test fails with `welcome message changed to:` on the business side.
