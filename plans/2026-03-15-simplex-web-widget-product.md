# SimpleX Web Widget: Product Plan

Revision 1, 2026-03-15

## Overview

A one-line JavaScript drop-in that lets site visitors chat with site owners via SimpleX Chat, directly from the browser. No app install. No account creation. End-to-end encrypted.

Site owners embed a script tag. Visitors see a chat bubble. Click to chat. Messages flow through SimpleX network to the site owner's SimpleX address.


## Dependencies

The widget is the interface layer. Without the underlying capabilities, it would be just a contact form with extra steps.

**Value stack:**

1. **Business chats** (v6.2, done) -- Hybrid of 1:1 and group conversation. Customer sees business name/avatar. Business sees customer name/avatar. But internally works as group -- multiple agents can participate, customer sees who sent each message, agents can be added for delegation and escalation. This is the WeChat business account model, but private.

2. **Support bot with escalation** (in development, almost ready) -- Auto-accepts contact requests, can answer common questions, escalates to human agents or AI based on configured logic. Without this, site owners must be online to accept connections and respond manually.

3. **Widget** (this plan) -- Delivery mechanism bringing business chat capability to web visitors who don't have SimpleX app installed.

**Why this stack is the moat:**

Competitors cannot just copy the widget. They would need:
- Hybrid 1:1/group protocol (business chat structure)
- Business addresses with auto-accept
- Agent invitation flow maintaining e2e encryption
- The underlying network infrastructure

Signal could build a widget, but their conversations are 1:1. Customer wouldn't see who they're talking to. Adding people means creating a group -- different UX, different mental model, breaks the support flow.


## Users

### Site Owners

**Who are they?**

1. **Privacy-differentiated businesses** -- VPNs, security tools, crypto services. Privacy is their brand. Using Intercom is hypocrisy.

2. **Compliance-sensitive services** -- Healthcare, legal, financial. Confidentiality is requirement, not preference. Third-party chat providers are liability.

3. **Niche communities** -- Anarchist, agorist, activist. They won't touch surveillance-based tools.

4. **Whistleblower/tip platforms** -- Journalists, NGOs, investigators. Source protection is existential.

5. **SimpleX believers** -- They use SimpleX personally. They want their business to match their values.

**Problems solved:**

- Can't force visitors to install an app for one support question
- Current solutions (Intercom, Zendesk, Drift) are surveillance infrastructure -- track visitors, correlate identities, sell data
- Self-hosted alternatives (Chatwoot, Rocket.Chat) are complex and not e2e encrypted
- Email works but provider reads everything
- They want SimpleX properties without the app install barrier

**Opportunities created:**

- Differentiation: "We don't track you, even in support"
- Reach privacy-conscious segment actively seeking them
- Convert visitors into SimpleX app users
- Anonymous feedback channel they couldn't have before
- Compliance story: no third-party data sharing, no accounts, nothing to subpoena
- Reply later without asking for contact details -- the connection IS the contact
- Convert casual inquiry into long-term customer relationship without friction
- Marketing and conversion channel without collecting personal data -- when Intercom/Drift ask for phone number, they lose 85-90% of visitors. Here they don't have to ask.

**Concerns:**

- Spam/abuse -- open contact invites bots
- Reliability -- will messages actually arrive?
- Integration complexity -- how much engineering time?
- Trust in our code -- they're embedding JS they didn't write
- Support workflow -- how do multiple agents handle conversations?
- Performance -- will it slow their site?

**Requirements:**

- One-line integration (script tag + parameters)
- Hash pinning to specific version
- Self-host option for maximum trust
- Customization -- brand colors, position, button style
- Mobile browser support
- Multiple agents/team routing (via bot)
- Canned responses for common questions
- Analytics without surveillance -- conversation counts, response times
- i18n support
- No impact on page load performance


### Site Visitors

**Who are they?**

1. **Privacy-conscious users** -- They notice when a site uses Intercom. They appreciate when it doesn't.

2. **Account-fatigued users** -- Don't want to create yet another account for one question.

3. **Anonymous inquirers** -- Legal questions, health questions, sensitive purchases. No trail.

4. **SimpleX users** -- They see the SimpleX logo, they trust it immediately.

5. **Casual askers** -- Just want an answer. Fastest path wins.

**Problems solved:**

- Don't want to install an app for one question
- Don't want to create an account
- Don't want to give email/phone
- Don't want Intercom tracking them across the web
- Want encrypted communication without setup

**Opportunities created:**

- Private conversation with businesses -- previously impossible without app install
- Entry point to SimpleX ecosystem -- taste before commitment
- Anonymous feedback they couldn't give before
- Confidential inquiries from untrusted devices

**Concerns:**

- Is this actually private? Site controls the JS
- What happens if I close the browser?
- How will I know when they respond?
- Can I continue this conversation later?
- Can I take this conversation with me to the app?

**Requirements (MVP):**

- No app install, no account creation
- Clear privacy indicator -- "via SimpleX Network"
- Standard chat UX -- sent/delivered checkmarks
- File sharing via upload page link
- Session persistence across tabs and sessions
- Graceful degradation -- retry on failure

**Requirements (Post-MVP):**

- Migration path to SimpleX app
- In-widget file upload
- Typing indicators
- Full accessibility support


## Product Flow

### Site Owner Setup

1. Create SimpleX business address or deploy support bot
2. Add script tag to site with address parameter and version hash
3. Optionally set accent color and dark mode sync
4. Done

Recommended: Deploy open-source support bot that auto-accepts contact requests and routes to agents. We provide the bot.

### Visitor Experience

**Widget closed:**
- Chat bubble with shield icon in bottom-right
- Text: "Talk to us privately via SimpleX Network"

**Widget opens (not connected yet):**
- Business name, logo, welcome message (from address data)
- Name field with placeholder "name or pseudonym"
- Incognito button (mask icon) to generate random name
- Message entry field
- Visitor sees name before sending -- conscious choice

**Send tapped:**
- First message included in connection request (single action)
- Single checkmark = sent, double checkmark = delivered
- "Keep this tab open to receive replies" instruction
- "Notify me when [Business] replies" button (browser notification)
- If SMP router down: retry button

**Conversation:**
- Standard chat UI
- Agent joins shown as pronounced system message
- Replies/reactions from site owner displayed (visitor can't create in MVP)
- Conversation persists across sessions and tabs
- Same conversation in all tabs on same site

**Mobile:**
- Full screen when expanded
- Back button to return to bubble

**Ending conversation:**
- Visitor can delete -- always notifies owner, reverts to fresh state
- Owner deletes -- visitor sees notification (if sent) or delivery fails on next send
- History preserved on visitor side until visitor deletes


## UX Specification

### Widget Closed (Bubble)

- Pointy bubble shape with shield icon, no SimpleX logo
- Text: "Talk to us privately via SimpleX Network"
- Position: bottom-right
- Unread indicator when response waiting

### Widget Opens

Connection happens on SEND, not on open. Widget shows:

- Business name, logo, welcome message (from address data)
- Name field with placeholder "name or pseudonym"
- Incognito button (mask icon) fills random name from reduced dictionary
- User sees random name before sending -- transparency, conscious choice
- Message entry field

### Send Tapped

- First message included in connection request (single action)
- "Notify me when [Business] replies" button above entry
- "Keep this tab open to receive replies" instruction
- If SMP router down: retry button

### Conversation

- Single checkmark = sent, double checkmark = delivered
- No typing indicators (bot is instant, mental model is async)
- No read receipts (SimpleX doesn't support them)
- Agent joins: pronounced system message ("Alex joined the conversation")
- Replies and reactions from site owner displayed; visitor cannot create them in MVP
- File sharing: link to simplex.chat/file upload page

### Persistence

- Conversation persists across sessions until visitor explicitly deletes
- Same conversation across all tabs on same site
- Return visitor sees history immediately

### Conversation End

**Owner ends:**
- Owner "deletes contact" in SimpleX Chat
- If owner notifies: visitor sees notification
- If owner deletes quietly: visitor discovers on next send (delivery fails)
- Visitor still sees history until they delete

**Visitor ends:**
- Always notifies owner (no quiet removal)
- Reverts to fresh state
- Can start new conversation immediately

### Mobile

- Full screen when expanded
- Chat bubble when closed
- Standard touch targets (48px minimum)
- Back button / X to return to bubble

### Customization

- Accent color matches site
- Dark mode with API to sync with site's dark mode
- Welcome message from business address data
- Position: bottom-right (configurable post-MVP)

### Notifications (MVP)

- "Keep this tab open to receive replies"
- Browser notification permission for in-tab alerts
- Web Push via notification router is post-MVP


## Trust Model

**What we claim:**

"Your conversation is encrypted between you and the site owner. No third party -- not us, not the hosting provider, not ad networks -- can read it."

**What we don't claim:**

"This is as secure as native SimpleX."

**Why the distinction:**

Site owner controls the JS. Visitor trusts site owner not to exfiltrate messages before encryption. This is honest -- visitor is talking TO the site owner. The protection is against third parties, not against the site owner.

The architectural guarantee: no party other than visitor and site owner can read the messages. SimpleX network, hosting providers, CDNs, ad networks -- none of them have access.


## MVP Scope

### In MVP

**Widget UI:**
- Chat bubble with shield icon, "Talk to us privately via SimpleX Network"
- Business name, logo, welcome message from address data
- Name field with incognito button (mask icon)
- First message in connection request
- Single/double checkmark for sent/delivered
- Agent join notifications (pronounced)
- Display replies and reactions from site owner
- "Keep this tab open" instruction
- Dark mode with API to sync with site
- Accent color customization
- Full screen on mobile

**Persistence:**
- Same conversation across sessions and tabs
- History preserved until visitor deletes
- Visitor deletion notifies owner

**File sharing:**
- Link to simplex.chat/file or site's upload page

**Documentation:**
- Setup guide for JS integration
- Bot setup guide

### Post-MVP

- Web Push notifications (via notification router + UnifiedPush protocol)
- Migration to app (QR code transfer)
- In-widget file upload
- Visitor replies and reactions
- Typing indicators
- Full accessibility support
- Full theme customization
- Position options

### First Adopters

1. simplex.chat
2. Evgeny's personal site
3. Friendly community sites


## Resolved Questions

### Spam/Abuse
Bot with captcha protection, same as directory groups. Not immediate concern at current network stage.

### Multi-tab
Same conversation across all tabs. Tech design will determine storage mechanism.

### Notifications (MVP)
"Keep this tab open" instruction. Web Push is post-MVP.

### Branding
"via SimpleX Network" visible. No white-label option -- branding IS the value.


## Business Model

The widget can be free or paid, at the discretion of each SMP router operator.

**How it works**: Browser WebSocket connections include an `Origin` header (browser-enforced, not spoofable). The SMP server sees which website is embedding the widget. The server controls access via CORS — it returns `Access-Control-Allow-Origin` only for allowed domains. Without the correct CORS header, the browser blocks the connection.

**What the server sees**: origin domain (which site) and usage volume (number of connections/commands). The server does NOT see who the users are, message content, or any per-user information.

**Billing model options for router operators**:
- Free for all domains (open access)
- Free with trial period, paid after (per-domain)
- Paid from start (per-domain, usage-based)
- Allowlist only (specific domains)

**Implementation**: server checks `Origin` header against configured domain list during WebSocket upgrade. Allowed domains get CORS headers and proceed. Unknown domains are rejected. Trial expiry, usage limits, and billing are operator concerns — the protocol layer just provides the access control mechanism.

This enables SimpleX router operators to monetize widget hosting without compromising user privacy — billing is per-site, not per-user.

## Privacy Model

**What SMP router operators can observe**:
- Which sites embed the widget (Origin header)
- Usage volume per site (connection/command counts)
- Visitor IP addresses (from TCP connections)
- Connection timing and patterns

**What they cannot observe**:
- Message content (end-to-end encrypted)
- User identities (no accounts, no cookies)
- Which queues belong to which visitors

**Visitor IP addresses**: SMP servers can observe visitor IP addresses from WebSocket connections. Visitors concerned about IP privacy should use Tor or a VPN.

## Open Questions

### Team Workflow
- Agents use SimpleX Chat app
- Bot handles routing and escalation
- Dashboard is out of scope (agents use app)

### Trust Verification
- "via SimpleX Network" text provides some verification
- Link to simplex.chat for more info
- Full verification is complex in browser -- acceptable tradeoff


## Success Metrics

**Measurable without tracking:**
- Number of sites embedding widget (observable)
- Integration time < 5 minutes
- Zero support requests for setup

**Not measurable (would require tracking that contradicts value proposition):**
- Message counts
- App conversion rates
- Visitor behavior

We accept limited visibility as a feature, not a bug.


## Competitive Landscape

| Solution | E2E Encrypted | No Account | Self-hostable | Privacy |
|----------|---------------|------------|---------------|---------|
| Intercom | No | No | No | Surveillance |
| Zendesk | No | No | No | Surveillance |
| Drift | No | No | No | Surveillance |
| Crisp | No | No | No | Tracks users |
| Tawk.to | No | No | No | Tracks users |
| Chatwoot | No | Optional | Yes | Better, not e2e |
| **SimpleX Widget** | Yes | Yes | Yes | Architectural |

No direct competitor offers e2e encryption with no app install.


## Why This Might Fail

### Site Owner Rejections

1. **Privacy is not their differentiator** -- Most businesses compete on price, features, service. Privacy is nice-to-have. The market of privacy-differentiated businesses is small.

2. **Their customers aren't asking for this** -- Site owners respond to demand. If customers aren't complaining about Intercom, why change?

3. **Anonymous makes their job harder** -- Can't look up customer history. Can't verify identity for account issues. Can't escalate with context.

4. **No metrics, no justification** -- Marketing needs conversion attribution. Sales needs lead scoring. Anonymous chat gives them nothing. Can't prove ROI.

5. **Can't prioritize** -- Intercom shows "paying customer" vs "free tier". Anonymous = everyone looks the same.

6. **No remarketing** -- Traditional chat captures leads for future marketing. Anonymous = conversation ends, relationship ends.

7. **Wrong buyer** -- Decision maker is CMO or VP Sales. They care about pipeline, not privacy. Security team might care but doesn't control budget.

8. **"Good enough" inertia** -- Intercom works. Switching cost is real. Marginal benefit doesn't justify migration.

### Visitor Rejections

1. **Privacy doesn't matter for support** -- "Help me reset my password." Transactional. They don't think about surveillance for routine questions.

2. **Unfamiliar = risky** -- Intercom is everywhere. Unknown widget might be broken, might be scam.

3. **"They won't take me seriously"** -- Anonymous = no accountability feeling. "I need them to know I'm a paying customer."

4. **Can't prove who I am** -- Support often requires identity verification. Anonymous chat can't connect to their account.

5. **No history** -- Repeat visitor expects "continue previous conversation". Ephemeral = start over.

6. **Email already works** -- Familiar, works across devices, persists, searchable. Why learn something new?

7. **Cognitive load** -- "What is SimpleX? Do I need an account?" vs "just type and send" with known tools.

### Market-Level Risks

1. **Niche ceiling** -- Privacy-focused sites are small percentage of market. Growth capped by market size.

2. **No viral loop** -- Visitor uses widget, likes it... nothing happens. No mechanism for organic spread.

3. **Chicken-egg brand** -- Site owners want established solution. Need adoption for credibility, need credibility for adoption.

4. **Wrong wedge** -- Support chat might not be the right entry point for privacy value proposition.

5. **Competes with our app** -- If widget is good enough, why install the app? If widget is too limited, why use it?


## Target Market Narrowing

These failure modes don't invalidate the product. They narrow the target.

**Primary target: Sites that already serve anonymous customers and accept cryptocurrency.**

These sites:
- Already operate in anonymous mode -- no identity required from customers
- Already accept crypto -- payment doesn't require identity
- Already attract privacy-conscious customers who actively seek this
- Have no expectation of CRM integration or lead capture
- Their customers ARE asking for private communication
- Privacy is their differentiator, not a nice-to-have

Examples:
- VPN providers
- Privacy tools and services
- Crypto exchanges and services
- Security software vendors
- Anonymous hosting providers
- Privacy-focused marketplaces
- Individual bloggers and personal sites -- want contact option without exposing email, auto-accepting address is enough

For these sites, the failure modes above don't apply. Their visitors expect anonymity. Their business model doesn't require identity capture. Intercom is actively harmful to their brand.

**Not our customers:**

1. **Compliance-driven enterprises** -- Healthcare, finance. They need SOC2, vendor assessments, compliance checkboxes. Open-source anonymity doesn't fit their procurement process.

2. **Mainstream e-commerce** -- They want lead capture, CRM integration, remarketing. They differentiate on price and service, not privacy. Intercom serves them well.

3. **Sites requiring identity verification** -- Banks, government, anything where "who you are" is prerequisite for service. Anonymous chat contradicts their core flow.

4. **High-volume support teams** -- Need dashboards, ticket assignment, SLAs, analytics, multi-agent routing with metrics. We can't compete on features, shouldn't try.

5. **Marketing-driven sites** -- Measure everything, optimize funnels, A/B test. Anonymous = no attribution = useless to them.

6. **"Does it integrate with Salesforce?"** -- If they ask this, wrong customer. They're buying CRM tooling, not privacy infrastructure.

The customers we refuse are as important as the customers we serve. Trying to serve everyone means serving no one well.


## Technical Risks

1. **Browser limitations** -- No TLS certificate pinning. Trust browser's CA system. Acceptable for threat model.

2. **Persistence complexity** -- Conversation must persist across sessions and sync across tabs. Storage mechanism TBD in tech design.

3. **JS supply chain** -- Site owner embeds our JS. Mitigated by including hash in script tag (Subresource Integrity). Browser refuses to execute if hash doesn't match. Self-host option also available.

4. **Adoption chicken-egg** -- Visitors don't know what SimpleX is. "via SimpleX Network" text helps, but brand recognition takes time.

5. **Background tab limitations** -- Browsers throttle/suspend background tabs. MVP uses "keep tab open" instruction. Post-MVP may add Web Push via notification router.
