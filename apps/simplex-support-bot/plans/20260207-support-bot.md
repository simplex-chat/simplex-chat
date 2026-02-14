# SimpleX Support Bot — MVP Product Specification

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

No mention of Grok, no choices. User simply types their question. Messages at this stage are only forwarded to the team — never to any third party.

## Step 2 — After user sends first message

All messages are forwarded to the team group. Bot replies:
> Your message is forwarded to the team. A reply may take up to 24 hours.
>
> If your question is about SimpleX Chat, click /grok for an instant AI answer (non-sensitive questions only). Click /team to switch back any time.

On weekends, the bot says "48 hours" instead of "24 hours".

## Step 3 — `/grok` (Grok mode)

Bot replies:
> *You are now chatting with Grok. You can send questions in any language.* Your message(s) have been forwarded.
> Send /team at any time to switch to a human team member.

Grok must be added as a separate participant to the chat, so that user can differentiate bot messages from Grok messages. When switching to team mode, Grok is removed.

Grok is prompted as a privacy expert and support assistant who knows SimpleX Chat apps, network, design choices, and trade-offs. It gives concise, mobile-friendly answers — brief numbered steps for how-to questions, 1-2 sentence explanations for design questions. For criticism, it briefly acknowledges the concern and explains the design choice. It avoids filler and markdown formatting. Relevant documentation pages and links must be injected into the context by the bot.

## Step 4 — `/team` (Team mode, one-way gate)

Bot adds a team member to the support group and replies:
> A team member has been added and will reply within 24 hours. You can keep describing your issue — they will see the full conversation.

**One-way gate:** once the user switches to team mode, `/grok` command is permanently disabled for this conversation and Grok participant is removed. Bot replies to any subsequent `/grok`:
> You are now in team mode. A team member will reply to your message.

This gate should trigger only after team joins and member sends message to team.

## Commands summary

| Command | Available in | Effect |
|---------|-------------|--------|
| `/grok` | Team Queue (before escalation only) | Enter Grok mode |
| `/team` | Grok mode or Team Queue | Add team member, permanently enter Team mode |
| `/add` | Team group only | Team member sends `/add groupId:name` → bot adds them to the customer group |

**Unrecognized commands:** treated as normal messages in the current mode.
