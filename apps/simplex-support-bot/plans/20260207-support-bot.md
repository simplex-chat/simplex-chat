# SimpleX Support Bot — Product Specification

## Principles

- **Opt-in**: Grok is never used unless the user explicitly chooses it.
- **User in control**: The user can switch between Grok and team at any time, and always knows who they are talking to. Once a team member engages, the conversation stays with the team.
- **Minimal friction**: No upfront choices or setup — the user just sends their question.
- **Ultimate transparency**: The user always knows whether they are talking to a bot, Grok, or a human, and what happens with their messages.

## Step 1 — Welcome (on connect, no choices, no friction)

Bot sends:
> Hello! Feel free to ask any question about SimpleX Chat.
> *Only SimpleX Chat team has access to your messages.* This is a SimpleX Chat team bot — it is not any LLM or AI.
> *Join public groups*: [existing link]
> Please send questions in English, you can use translator.

No mention of Grok, no choices. User simply types their question. Messages are forwarded to the team — never to any third party.

## Step 2 — After user sends first message

All messages are forwarded to the team group. Bot replies:
> Your message is forwarded to the team. A reply may take up to 24 hours.
>
> If your question is about SimpleX Chat, click /grok for an instant AI answer (non-sensitive questions only). Click /team to switch back any time.

On weekends, the bot says "48 hours" instead of "24 hours".

The bot also posts a clickable `/add groupId:name` shortcut to the team group so any team member can join with one tap.

## Step 3 — `/grok` (Grok mode)

Bot replies:
> *You are now chatting with Grok. You can send questions in any language.* Your message(s) have been forwarded.
> Send /team at any time to switch to a human team member.

Grok is added as a separate participant so the user can differentiate bot messages from Grok messages.

Grok is prompted as a privacy expert and support assistant who knows SimpleX Chat apps, network, design choices, and trade-offs. It gives concise, mobile-friendly answers — brief numbered steps for how-to questions, 1–2 sentence explanations for design questions. For criticism, it briefly acknowledges the concern and explains the design choice. It avoids filler and markdown formatting. Relevant documentation pages and links are injected into the context by the bot.

## Step 4 — `/team` (Team mode, one-way gate)

Bot adds the first configured team member to the support group as Owner and replies:
> A team member has been added and will reply within 24 hours. You can keep describing your issue — they will see the full conversation.

On weekends, the bot says "48 hours" instead of "24 hours".

If `/team` is clicked again after a team member was already added:
> A team member has already been invited to this conversation and will reply when available.

**One-way gate:** once a team member sends their first text message in the customer group, Grok is removed. From the moment a team member joins the group, `/grok` is permanently disabled and replies with:
> You are now in team mode. A team member will reply to your message.

## Team group view

All customer messages are forwarded to the team group with a formatted header showing: group ID, customer name, current state (QUEUE / GROK / TEAM), message number, and elapsed time since first contact. The clickable `/add groupId:name` shortcut (sent in Step 2) lets any team member join a conversation with one tap.

## Customer commands

| Command | Available | Effect |
|---------|-----------|--------|
| `/grok` | Before team escalation | Enter Grok mode |
| `/team` | Grok mode or before escalation | Add team member, permanently enter Team mode |

**Unrecognized commands** are treated as normal messages in the current mode.

## Team commands (in team group only)

| Command | Effect |
|---------|--------|
| `/add <groupId>:<name>` | Add yourself to the specified customer group as Owner |
| `/inviteall` | Add yourself to all customer groups active in the last 24 hours |
| `/invitenew` | Add yourself to groups active in the last 48 hours that have no team or Grok member yet |
| `/pending` | List all conversations awaiting a team response, sorted by longest wait first |
