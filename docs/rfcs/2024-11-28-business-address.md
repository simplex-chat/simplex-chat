# Business address

## Problem

When business uses a communication system for support and other business scenarios, it's important for the customer:
- to be able to talk to multiple people in the business, and know who they are.
- potentially, add friends or relatives to the conversation if this is about a group purchase.

It's important for the business:
- to have bot accept incoming requests.
- to be able to add other people to the coversation, as transfer and as escalation.

This is how all messaging support system works, and how WeChat business accounts work, but no messenger provides it.

## Solution

Make current contact addresses to support business mode. We already have all the elements for that.

- connection requests will be accepted automatically (non-optionally), and auto-reply will be sent (if provided).
- the request sender will be made member, can be made admin later manually.
- the new group with the customer will be created on each request instead of direct conversation.

Group will function differently from a normal group:
- Show business name and avatar to customer, customer name and avatar to business.
- Use different icon for customer and for the business if the avatar is not provided.
- Possibly, a sub-icon on business avatar for customers.
- Members added by business are marked as business, by customer as customer (not MVP).

This functionality allows to develop support bots that automatically reply, potentially answer some questions, and add support agents as required, who can escalate further.
