# Plan: several directories as a generic service layer

Run: 012-service-layer · Base branch: master once the directory search pull request (#7588) has merged, since groups A and B build on its directory RPC and its body compression; f/directory-search if ep holds that pull request · Repos: simplex-chat-3 (simplexmq for the telemetry prerequisite) · Estimated diff: ~6,300 changed lines in five groups of slices; the first release (groups A, B and the two app slices) ~5,000

Page for ep: https://claude.ai/artifact/493JgCBa4ZCLZQXwAQpVQT

This is a decision document for ep and a plan for the whole design. It builds on ep's own draft "Agent protocols over SimpleX" (docs/protocol/agent-protocols.md on master, 2026-10-19), which already defines the Service RPC envelope and the MCP binding. What it adds: roles defined by required tools, a stored list of services, the apps' use of the directory role, telemetry as the second role, and consent.

## 0. Decision asked of ep

Recommended, in one line: **build directories as services over your MCP binding, with roles defined by namespaced tool names; ship the directory search pull request as it is first; store services per operator like chat relays; telemetry second, after the reply-queue fix in simplexmq.**

The decisions, each with the recommendation first (section 9 has the costs):

1. Protocol: your Service RPC envelope with MCP 2026-07-28 messages (recommended), or our own MCP-shaped JSON, or today's request types plus a "describe" request. With it, three amendments to your draft (section 4, "Amendments to the draft"): sizes stated after compression, the role-tool naming convention, and requests without `clientInfo`.
2. Order: release the one-directory search now and the service layer in a later release (recommended), or hold the search for the service layer.
3. Old directories that don't speak MCP: not supported by new apps, the bot is updated instead (recommended), or a permanent adapter in the core.
4. Where services are configured: in each operator's servers and under Your servers (recommended), or one Services list.
5. Telemetry's content: date, platform and app version only (recommended), or also network health per operator.
6. Who runs usage statistics, given that a default device's return queues are all on SimpleX Chat's servers and its proxy is usually SimpleX Chat's: SimpleX Chat runs it and simplexmq lets a service request name an operator to avoid for the return queue and the proxy (recommended; this also takes default users' searches off SimpleX Chat's proxy and return queues), or Flux runs it on one of its enabled servers (no change to the agent's choices), or almost no device reports.

What each decision unblocks: decisions 1–3 are needed before the first run (the MCP layer in the core and the directory) and the second (storage and the search call); decision 4 before the app screens; decisions 5 and 6 only before the statistics group. No outside date drives them; the first run can start once 1–3 are answered.

One part first, if ep prefers: group A alone (slices A1–A5) is useful without the rest. It is the part of ep's draft marked "Planned" (the core's `protocols` key, a bot helper) plus SimpleX Directory answering MCP, so software can search it through a raw `/_service_request`, and AI agents through `@simplex-chat/mcp` once its planned `mcp_call` exists; the apps don't change.

## 1. Goal and acceptance criteria

What the user gets (all groups): any contact address that declares the MCP protocol and offers the directory tool can be added as a directory; SimpleX Directory stays preset; a search goes to every directory in use at once and the results arrive as one list, with a retry per failed directory; the enable alert names every directory that will receive the text; a second kind of service, usage statistics, is opt-in, sends once a day the date, platform and app version, and is configured with the same screens.

Acceptance criteria, each checkable by a named test or command (tests named in section 6):

- AC1 (declaration): a profile's `preferences.protocols` round-trips through the core (JSON, the profile store) and reaches the profile the core sends (address link data, `XInfo`) and a business chat's `groupPreferences`; an older core keeps a received one as raw JSON. Test: `testProtocolsPreference`, `testProtocolsBusinessChat`, JSON tests.
- AC2 (MCP client): the core sends `tools/list` and `tools/call` in the envelope of agent-protocols.md, accepts only well-formed JSON-RPC results with `id` 1, and maps envelope errors, JSON-RPC errors and `isError` to typed errors. Test: `testServiceTools`, `testServiceToolErrors`, unit tests of the parsers with hostile inputs.
- AC3 (test a service): `/_service test <userId> <address>` returns the address's name, its declared MCP versions, its tool names (first page, at most 64, each valid) and the roles it fills; failures are typed (not a contact address, no MCP declaration, no Service RPC, unsupported version, no answer, bad answer). Test: `testServiceTest`.
- AC4 (directory bot over MCP): SimpleX Directory answers `server/discover`, `tools/list` and `tools/call simplex.directory.search` in the envelope, declares `protocols.mcp` in its profile, and still answers today's `{"type":"search"}` unchanged. Tests: `testDirectoryMCP*`, today's `testDirectorySearchRpc*` unchanged.
- AC5 (directory role call): `/_directory search <userId> <serviceId> <json>` sends the search only to a stored service of this user that is in use for search (it has the directory tool, the directory role is turned on, and its operator group is enabled), and returns typed results with at most 50 entries, each a group or channel link, or a typed timeout error within the timeout. Test: `testDirectoryRoleSearch`, `testDirectoryRoleRefused`, `testDirectoryRoleTimeout`.
- AC6 (storage): services are a list in each operator group of `/_servers`, saved, validated (duplicate address), preset (SimpleX Directory for the SimpleX Chat operator), merged at start-up, copied to a new profile, with the compatibility rules of plan 015 (an app or core without the key saves and reads nothing for services). Tests: plan 015's AC1–AC6, retargeted (section 6, group B).
- AC7 (apps, first release): Settings lists services per operator and under Your servers; "Add server › Service" tests and adds one; search fans out to the services in use for search; per-service retry; the alert names them; `DIRECTORY_SERVICE_LINK` is gone from both apps. Checked on the CI runners (ios-sim, desktop-linux, android-emu) and by review; not compiled in the container.
- AC8 (telemetry, later group): with the switch on, at most one report per UTC day per device to each service in use for statistics in the active, non-hidden profile, holding exactly `day`, `platform`, `appVersion`; none unless every server the agent could pick for the return queue is a preset operator's other than the statistics operator's, and the request would be proxied (section 4, Telemetry); none from a desktop while it controls a phone. Tests: unit tests of D0(i)'s functions over built agent server configs, since chat test servers are all on localhost and carry no operator (tests/ChatClient.hs:214-215@M, Operators.hs:467-470@M): the return-queue check with the statistics service on smp4-like SimpleX Chat host and roles named (SimpleX Chat and Flux with default roles: sent under (b), refused under (c); custom only, statistics operator only, statistics operator and custom, Flux and custom: refused); the route check ("always" with Flux proxies available: sent under (b); "unknown servers" with the host in the list, "never", fallback "allow": refused; proxy candidates including the statistics operator's: refused); one integration test of the send path (`testTelemetrySubmit`) with a configuration built to pass; telemetry bot tests; app behaviour on runners.

## 2. Non-goals

- The chat transport (`/_call mcp` in a conversation) and `@simplex-chat/mcp`'s planned `mcp_tools`/`mcp_call`: ep's spec covers them; this design uses Service RPC only and doesn't block them.
- Moving the badge service onto the envelope (it has its own versioned RPC, docs/protocol/badges-rpc.md).
- Interpreting tool JSON Schemas on phones, fetching tool icons, showing tool descriptions.
- Sorting results across directories by member count (a directory can inflate counts).
- The simplexmq changes themselves (group D lists them as a prerequisite; they are proposed, not yet planned, and have no owner yet: question 12).
- A CLI search command.

## 3. Current state

From research.md (file:line there):

- ep's spec defines the envelope `{"protocol": "mcp", "request": …}` → `{"response": …} | {"error": {code, message?, retryAfter?}}`, the MCP binding (revision 2026-07-28 or later, one JSON-RPC request with its `_meta`; `server/discover`, `tools/list`, `tools/call`), declarations `preferences.protocols.<name> = {versions, transports {chat, rpc}}`, sizes (≤ 10,000 bytes of JSON), and lists the core's `protocols` key and a bot helper as "Planned" (docs/protocol/agent-protocols.md@M).
- MCP 2026-07-28 is stateless: no `initialize`, each request carries `io.modelcontextprotocol/protocolVersion` and `clientCapabilities` in `_meta`; earlier revisions need a handshake first, which one-shot Service RPC can't do. Tool names are 1–128 chars of `A-Za-z0-9_.-`; MCP itself warns that aggregators meet name collisions ("two servers each exposing a `search` tool").
- Service RPC today: `/_service_request` (Commands.hs:1508-1509, :6401@B), a new connection per request, no profile, optional signature, 10 s timeout from the apps (agent default 30 s), bodies ≤ 10,737 bytes compressed and ≤ 64 KiB decompressed with JSON depth ≤ 32 (Protocol.hs:1075-1110@B, compression only @B), errors `A_SERVICE …` (research §3).
- The reply queue is on one of the user's own servers, picked at random preferring another operator when the agent knows the target's operator, created on the session that carries the user's other queues on that server (`mkTSession`, simplexmq Client.hs:1441-1442@Q); the request itself goes through a proxy under the apps' default private routing ("always", AppGroup.swift:78, SimpleXAPI.kt:5434@M), so the service's server doesn't see the IP address unless the user chose another mode; but the proxy is picked at random from the user's enabled proxy servers (Client.hs:695-705@Q), so for a default device it is SimpleX Chat's own about 11 times in 17. iOS's default was "unknown servers" until 2026-10-21 (3126d9b54), and iOS users who saved network settings before keep it; under it a request to a server in the user's list goes directly. By default Flux doesn't store messages (Presets.hs:42@M), so a default user's return queues are all on SimpleX Chat's servers; and because SimpleX's own bots sit on preset servers that are disabled by default (smp4–6, Presets.hs:86-97@M), the agent doesn't know their operator and can't avoid it (section 4, Telemetry).
- The directory answers only `{"type":"search"}` with `entries`, `searchCursor`, `moreCount`, `pageSize`, and "unsupported request" otherwise (Directory/Rpc.hs:26-51, Service.hs:379-380@B); global in-flight cap 8; page size 10; image ≤ 12,500 chars; the cursor is a JSON object.
- The apps hard-code one address (`DIRECTORY_SERVICE_LINK`, DirectorySearch.swift:14, .kt:14@B), build and parse the search JSON themselves, keep consent in an app preference ("Directory search", default off), and on desktop-with-phone the phone's core sends the request.
- Chat relays give the storage pattern: a table per list with `domains` linking rows to operators, presets in `PresetOperator`, a start-up merge, closed warning types in the apps, `ListDef` for keys an older peer omits (research §7, §8). Plan 015 applied it to directories and converged; its last review left four P2s (research §7).
- Remote desktop pairs run nearly the same release (minimums 7.1.0.11, raised in most version bumps); an unknown command gives `CECommandError`, an undecodable reply `RPEInvalidJSON`.
- Profiles keep unknown preference keys as raw JSON (`_json`, Types/Preferences.hs:166-182@M); a contact address's link data carries the profile, so the declaration is readable without connecting.
- The badge service already speaks its own versioned RPC (Badges/Service.hs@M); nothing submits telemetry today; the core keeps per-server statistics (Stats.hs@M).

Invariants to keep: every warning and error the core returns is one the asking app decodes; every core in a supported remote pair decodes the other's replies; a stored preset keeps the user's choices; bodies are bounded before any app sees them; a search or report carries no profile; consent is checked on every path that sends text.

## 4. Design

### The model in one paragraph

A **service** is a contact address whose profile declares `protocols.mcp` with `transports.rpc` (ep's spec) and whose `tools/list` names its tools. A **role** is a contract SimpleX defines: a set of required tool names with fixed inputs and outputs. A service **fills** a role when the role's tools are a subset of the service's tools. The apps keep a list of services per profile, as they keep chat relays, and each role asks "which services in use fill me?" instead of using a constant. Directory search is the first role; usage statistics (telemetry) the second. Adding a third role is a contract, a call, and a screen line, not a new storage or protocol.

```
contact address ──link data──▶ profile.preferences.protocols.mcp {versions, transports.rpc}
        │
        └─ Service RPC {"protocol":"mcp","request": tools/list} ──▶ tools: [simplex.directory.search, …]

role directory  requires {simplex.directory.search}   ⊆ tools → fills
role telemetry  requires {simplex.telemetry.submit}   ⊄ tools → doesn't
```

What gets removed: `DIRECTORY_SERVICE_LINK` and the search JSON building and parsing in both apps (the core does it once, typed); CI's editing of app sources to point at the test directory (it adds the test service in Settings instead).

### Protocol (question 1)

Recommended: **ep's Service RPC envelope with MCP 2026-07-28 messages**, as specified, plus three SimpleX rules:

1. Requests carry `id: 1` (one request per connection), `_meta` with `io.modelcontextprotocol/protocolVersion: "2026-07-28"` and `io.modelcontextprotocol/clientCapabilities: {}`, and **no** `clientInfo` (ep's privacy note: every SimpleX app then sends the same bytes apart from the arguments).
2. Role tools have namespaced names, `simplex.<role>.<action>`, so a third-party MCP server's own `search` tool never makes it a directory by accident. The name is the contract: changes are additive (new optional fields); a breaking change is a new name.
3. Services serving roles answer `server/discover` (MCP requires it) and list role tools on the first page of `tools/list`; the core reads only the first page.

Role tools return their data in `structuredContent`; the core reads only that. Whether `content` also carries the JSON as text (MCP's "SHOULD", for older MCP clients) is the service's choice: the page fit runs on the final compressed body, so either fits.

Sizes: the bound is the compressed body, 10,737 bytes, at most 64 KiB once decompressed, JSON depth ≤ 32 (Protocol.hs:946-947, :1075-1110@B). ep's draft says "SHOULD NOT exceed 10,000 bytes of JSON", which describes master, where bodies aren't compressed; the directory search branch adds compression (Protocol.hs:1075-1096@B, not on master); one allowed directory image (12,500 characters) is already over 10,000.

#### Amendments to the draft

Proposed for "Agent protocols over SimpleX", in the docs slice of group A, if ep agrees:
- "Message size" for Service RPC: the limit is 10,737 bytes after the core's compression and 64 KiB after decompression; `too_large` refers to the compressed size.
- A "Roles" section: SimpleX-defined tools are named `simplex.<role>.<action>`; a client uses a service for a role when the service lists all of the role's tools; the contracts of `simplex.directory.search` and `simplex.telemetry.submit`.
- Privacy: SimpleX clients omit `clientInfo` and send `id: 1`.

An example exchange, a search (the envelope fields are ep's; `_meta` is shortened after the first request):

```json
{"protocol": "mcp",
 "request": {"jsonrpc": "2.0", "id": 1, "method": "tools/call",
   "params": {"name": "simplex.directory.search",
              "arguments": {"searchText": "cinews"},
              "_meta": {"io.modelcontextprotocol/protocolVersion": "2026-07-28",
                        "io.modelcontextprotocol/clientCapabilities": {}}}}}
```

```json
{"response": {"jsonrpc": "2.0", "id": 1,
  "result": {"resultType": "complete",
    "content": [{"type": "text", "text": "3 groups, 0 more"}],
    "structuredContent": {"entries": [ … ], "moreCount": 0, "pageSize": 10}}}}
```

The search contract keeps today's field names (`searchText`, `searchCursor` in; `entries`, `searchCursor`, `moreCount`, `pageSize` out; the entry type of Directory/Rpc.hs), so the directory's paging code and the apps' entry decoding carry over; only the wrapping changes. The entry types move from the directory app into the core library so the bot and the core share one definition of an entry (a move commit). The request and response records of the contract are new, untagged records in the core (MCP `arguments` and `structuredContent` have no `"type"` field), with `searchCursor :: Maybe J.Value`, opaque and at most 1,024 bytes encoded: the core never parses a directory's cursor, so each directory keeps its own; SimpleX Directory converts its `SearchCursor` with its existing JSON instances.

What literal MCP buys: one vocabulary with ep's spec and with `@simplex-chat/mcp` (an AI agent's planned `mcp_call` reaches the same services with the same messages); MCP tooling and SDK types for TypeScript services; any MCP server can become a SimpleX service through a thin bridge. What it costs: about 300 core lines for the envelope and JSON-RPC types (versus about 150 for our own JSON); about 250 bytes of overhead per request and 120 per response before compression; tracking MCP revisions (the core speaks one, a service answers -32022 with its versions, and the core says "needs an app update"). Phones never handle JSON Schema: the core reads only tool names, and each role's inputs and outputs are Haskell types, so a schema can't make a phone do work.

Alternatives:
- **Our own MCP-shaped JSON** (`{"type":"tools"}`, `{"type":"call","tool":…}`): about 150 fewer core lines and ~150 fewer bytes per request, the same style as today's directory; but a second dialect beside ep's spec, a mapper for every MCP client, and the badge service already shows how dialects accumulate. Not recommended.
- **Today's request types plus a "describe" request** (run 010's form): the smallest change (about 100 lines in the bot, nothing in the core's protocol), but each new role is a new ad-hoc request type and nothing is reusable by AI agents. It is what plan 015's option B would have done.

How today's directory fits: the bot gains the MCP binding beside its current request (it keeps `{"type":"search"}` for released apps), declares `{"mcp": {"versions": ["2026-07-28"], "transports": {"rpc": true}}}` in its profile (not `chat`: declaring the chat transport would oblige it to answer `/_call` in conversations), and is upgraded before any app that uses the service layer ships (it already has to be upgraded for its address keys before release). New apps don't map old directories (question 3): every directory runs the same open-source bot, so an old one is updated, not adapted. An adapter would cost about 60 core lines and a test, kept for as long as any old directory might exist.

### Roles (question 2)

- **Definition.** In the core, a closed type `ChatServiceRole = CSRDirectory` (with `CSRTelemetry` added by D1, when statistics are built; not `SR…`: simplexmq's `SMPServiceRole` already has `SRMessaging | SRNotifier | SRProxy`, Transport.hs:592@Q), JSON `"directory"`, `"telemetry"`, with `roleTools :: ChatServiceRole -> NonEmpty ToolName`. `serviceRoles tools = filter (all (`elem` tools) . roleTools) [minBound ..]` is the only matching rule. The apps decode roles as strings and ignore unknown ones, so a newer core's role doesn't break an older app; the core likewise drops role names it doesn't know when reading the `roles` column and the `/_servers` JSON, so a database written by a newer version (a desktop downgrade, or an Android export imported into an older app) doesn't fail the start-up merge, which reads every user's servers in one transaction (Chat.hs:171@M).
- **Contracts.** `simplex.directory.search`: in `{searchText (≤ 100 chars), searchCursor?}`, out `{entries, searchCursor?, moreCount, pageSize}`. `simplex.telemetry.submit`: in `{day, platform, appVersion}`, out `{accepted}`. The services publish matching `inputSchema`/`outputSchema` for MCP clients; the core validates with its own parsers, not with the schemas.
- **In use.** A service is in use for a role when it fills the role, the user turned the role on for it ("Use for search"), it isn't deleted, and its operator is enabled (the custom group counts as enabled) — plan 015's rule with "enabled" made per role. The core checks all four before sending a role call (the operator check as `opEnabled` does for relays, Operators.hs:584, :592-593@M).
- **Two or more services fill a role.** Directory: each search goes to all services in use for it at once, at most five, in settings order (question 7); results merge as boards 14–15 draw; each failed service gets its own retry row. Telemetry: each service in use for statistics gets the day's report (normally only the preset one).
- **Role screens.** Directory: today's results, the load row, per-service retry, the Directories submenu (boards 14–17, unchanged). Telemetry: a switch and a one-line status in Privacy settings, nothing else.
- **Concurrency.** Each role runs on its own trigger (a search, a daily timer); within a role all services are asked at once with a 10 s timeout each; nothing in the core serialises them (research §3). On iOS each request runs off the cooperative thread pool (plan 015's finding: the FFI call blocks its thread for up to the timeout).

### Telemetry (question 3)

- **What:** `{"day": "2026-10-22", "platform": "ios", "appVersion": "7.1"}`: the UTC date, one of ios, android, desktop, and the major.minor version. Nothing else in the first version. Optional later (ep's call, each with its cost): per preset operator, bucketed counts of failed and successful message sends that day (useful for operators' reliability; buckets rather than counts, because exact counts fingerprint a device).
- **When and from where:** at most once per UTC day per device to each service in use for statistics, at a random moment while the app runs that day, from the active profile if it isn't hidden (its services and servers); a missed day is never sent later, and reports are never batched. A desktop that controls a phone sends nothing while connected (its commands would run on the phone's core and servers). The once-a-day mark is kept by the app per device, keyed by UTC day and service address (not by the per-profile service id), so three profiles holding the same preset still send one report, and switching profiles mid-day doesn't send again.
- **Consent:** its own switch, "Share usage statistics" in Privacy settings, off by default. Turning it on shows an alert with the exact report and the services' names; each service has "Use for usage statistics" (preset: on for the statistics service; user-added services: off until the user turns it on). Status under the switch: "Last sent today to [service]", "Not sent: no service in use", "Not sent: your servers would identify you", or "Not sent: private routing is off for this server" (below).
- **What the service can learn:** per report, the three fields, the time it arrived, and the server of the return queue (the service needs it to answer, simplexmq Agent.hs:2287-2290@Q); no profile, no device key, and no key that links two reports. Whether it also learns the IP address and the user's session depends on the route (below).
- **How the agent picks servers for a service request, today.** Two servers besides the service's own: the return queue's, from the enabled servers with the storage role (`withNextSrv … storageSrvs`, Agent.hs:2282-2284@Q), and the proxy's, from the enabled servers with the proxy role (`getSMPProxyClient` → `getNextServer c userId proxySrvs [destSrv]`, Client.hs:695-705@Q, fixed per destination session). Both prefer operators the target's host doesn't belong to, at random, falling back to the whole list (`getNextServer_`, Client.hs:2516-2534@Q). But the agent learns the target's operator only through a server in that same enabled list sharing the target's host (`usedOperatorsHosts` over the selected list; `mkUserServers`, Env/SQLite.hs:136-140@Q, keeps other servers only as hosts). SimpleX's own bots sit on preset servers that are disabled by default (smp4–6, Presets.hs:86-97@M: SimpleX Directory on smp4, the badge service on smp5, Chat.hs:74@M), so for them no operator is avoided: the pick is random over all enabled servers. And Flux's preset SMP roles have storage off (Presets.hs:42@M), with onboarding turning it on only when no other enabled operator stores (ChooseServerOperators.swift:205-215, .kt:338-347). For a default device this means: every return queue is on a SimpleX Chat server, on the session that carries the user's other queues there (`mkTSession`, Client.hs:1441-1442@Q), and the proxy is SimpleX Chat's own in about 11 of 17 picks (11 enabled SimpleX Chat servers, 6 Flux, all with the proxy role, Presets.hs:28, :42, :68-78, :109-115@M). Custom servers have no operator (`Nothing`, Operators.hs:464-465@M), so they always count as "another operator". Older installs that stored smp4–6 as enabled behave differently (stored presets keep their flags, Operators.hs:411-433@M).
- **Two ways a report can be linked to its sender, and the checks against each.** The core names the service's operator, found from the address's host and the operators' server domains as the core already matches servers to operators (`matchingHost`, Operators.hs:467-470@M; a host no operator claims has none), and computes both checks with pure functions exported from simplexmq (D0(i)) over the same inputs and the same named operator the agent uses, so the check and the choice can't drift apart.
  1. **The return queue.** If it can land on the user's own server, every report names that server and reports link across days; if it can land with the statistics operator, that operator can join the report to the user's session. The core sends only when every return-queue candidate is a preset operator's server other than the service's operator; otherwise "Not sent: your servers would identify you".
  2. **The route.** The request must go through a proxy (`smpProxyMode`, Client.hs:1189-1208@Q): mode "always"; "unknown servers" with the service's host not in the user's list (`knownHosts`, Env/SQLite.hs:140@Q, which includes disabled preset hosts); "unprotected" only when the IP address isn't protected (:1198-1200). And every proxy candidate must belong to an operator other than the service's, or the statistics operator sees the IP address with the report. Otherwise "Not sent: private routing is off for this server". iOS defaulted to "unknown servers" until 2026-10-21 (3126d9b54, AppGroup.swift:78@M), and users who saved network settings before keep it. With fallback "allow protected" (the default), a request falls back to direct only when the IP address is protected (e.g. Tor) after a proxy error; that rare case is accepted and stated. Fallback "allow" refuses.
- **The options (decision 6).** With today's agent, a SimpleX Chat statistics service fails both checks on every default device.
  - **(b) Recommended: SimpleX Chat runs the statistics service, and simplexmq lets a service request name an operator to avoid** (D0(ii)): the agent then prefers other operators' enabled servers for both the return queue and the proxy, may use an enabled operator's servers without the storage role for the return queue (Flux, for default users), and falls back to today's choice when no other operator is enabled. The core passes the target's operator, found by host as above, for every service request (statistics, search, and the badge service's requests, whose address on smp5 is SimpleX Chat's). Default devices then pass both checks (return queue and proxy on Flux); in the fallback the checks refuse statistics, while search and badge requests go ahead as today. The same change moves default users' searches off SimpleX Chat's proxy and return queues. Cost: D0(ii), a few hundred lines in simplexmq with tests; `JRServiceReq` is a stored async command (Agent.hs:2207-2209@Q), so the new field needs an encoding that still reads old rows; Flux briefly holds return queues (seconds, deleted after the answer, Agent.hs:1842-1858@Q) for users who left Flux's storage off, which is ep's call.
  - (a) Flux runs the statistics bot, on one of Flux's enabled servers. Then today's host matching already marks Flux as used for the proxy (its servers are enabled with the proxy role), the return queue is on SimpleX Chat's storage servers, and default devices pass both checks without D0(ii). Still needs D0(i) and D1–D2. Users who turn Flux off send nothing. Cost: Flux operates the service and holds the counts, an arrangement between the operators; searches stay as today.
  - (c) Keep SimpleX Chat's service with today's agent: only devices with a second storing preset operator, no custom server and a proxy list without SimpleX Chat report, almost none; not worth building group D.
- **What still links after (a) or (b):** the report is with one operator, the return queue and proxy with the other, so linking needs the two operators to share data. The return queue is still created on the user's existing session with that server; giving service-request return queues their own session (D0(iii)) is optional hardening against that, not a prerequisite. The alert promises only what holds: no profile and no identifier are sent.
- **What it counts:** reports per day per platform and version among devices that turned the switch on (it is off by default), so a lower bound of opted-in daily active devices, not of all users. Weekly or monthly actives can't be computed without linking reports; that is the price of having no identifier. The counts can't be authenticated: reports carry no key by design, so anyone can send any number of them, and nothing can rate-limit them without linking. The numbers are indicative, not a measurement anyone can rely on against a motivated sender.
- **Who runs it:** a small bot, `simplex-telemetry-service`, on the shared MCP helper, counting reports per day, platform and version and keeping nothing else (no return-queue addresses, no times finer than the day); run by SimpleX Chat under option (b), by Flux under (a).

### Configuration and the screens (question 4)

Recommended: services sit where boards 10–12 put directories, renamed: a **Services** section on each operator's servers screen and under Your servers; **Add server › Service**; a **New service** screen whose "Test service" fetches the name, checks the MCP declaration and lists what the service can be used for. Mock-ups of the changed boards are on ep's page.

| Board | In this design |
|---|---|
| 10 Directories on an operator's screen | changes: section "Services"; each row shows the name and what it's used for ("SimpleX Directory · Search"); footer "Services answer your app's requests, like searches." |
| 11 Add server › Directory | changes: the item is "Service" |
| 12 New directory | changes: "New service"; "Test service" shows "Can be used for: Search" (and "Usage statistics" when offered); one switch per role ("Use for search", "Use for usage statistics"); "Other tools: N" when it has more |
| 13 Enable alert with two directories | changes: the text drops "so they can't build your search history": "Search text is sent to SimpleX Directory and [Your directory], each search separately and without your profile." |
| 14 Results from two directories | stays |
| 15 One directory failed | stays |
| 16 Long press: Open, Directories | stays |
| 17 Directories submenu | stays; a tap opens that service's screen |
| new: Privacy settings | "Share usage statistics" switch under "Directory search", with its status line |
| new: statistics alert | "Share usage statistics?" / "Once a day your app sends [SimpleX Chat statistics] this report: 2026-10-22 · iOS · 7.1. No profile or identifier is sent." / Cancel, Share |

Alternative: one **Services** list in Network & servers, outside operators. Simpler to explain, but a preset service loses its operator (the operator's switch and conditions no longer cover it), and ep asked for no new settings category. Not recommended; it would cost the same storage with a new screen instead of a section.

A user sees: in Network & servers, SimpleX Directory under SimpleX Chat's servers marked "Search"; Add server › Service to add another; in Privacy, "Directory search" (exists) and "Share usage statistics" (new, off).

### Storage, presets and compatibility (question 5)

Plan 015's option A carries over with "directory" replaced by "service" and two new columns:

- Table `services (service_id, user_id … REFERENCES users ON DELETE CASCADE, address BLOB, display_name TEXT, domains TEXT, preset INTEGER, tools TEXT, roles TEXT, created_at, updated_at)`, `STRICT`, unique `(user_id, address)`, index on `user_id`. `tools` is the comma-joined tool names (the MCP name charset has no comma); `roles` the roles the user turned on. No `deleted` (nothing references a service) and no `tested` (stored tools are the test result).
- Type `UserService' s {serviceId, address :: ShortLinkContact, displayName, domains, preset, tools :: [ToolName], roles :: [ChatServiceRole], deleted}` with `UserService`, `NewUserService`, `AUserService`; the API adds the derived `availableRoles` (from `serviceRoles tools`) so the apps don't repeat the matching rule.
- In `UserOperatorServers` as `services :: ListDef UserService` (an older phone's reply parses as none); in `UpdatedUserOperatorServers` as `services :: Maybe [AUserService]` (`Nothing` from an older app saves and validates nothing for services). Validation: duplicate address error; a "no service for search" warning only when the request carries services (plan 015's R1-1 and R2-1 fixes).
- Presets: SimpleX Directory in the SimpleX Chat operator with tools `[simplex.directory.search]` and role directory on; later the statistics service with `[simplex.telemetry.submit]` and role telemetry on (nothing is sent until the Privacy switch is on). A preset's stored row takes the preset's tools on each start and keeps the user's roles; a role the preset newly offers starts as the preset says.
- Start-up merge: plan 015's adoption before grouping (a custom row at a preset's address becomes the preset once and for good) and its handling of presets an operator stops listing, with the four P2s of its last review applied: the claim narrowed to "its operator still preset"; tests for the `-s` shape, for an operator missing from the presets, and for adoption before the drop check; a restart test with `testCfg` variants; and question 8 answered "keep it, turned off, as the user's own", which needs no new store function.
- Names: normalised with `mkValidName` where app input enters the core; a service's name is its own profile's name.
- Older apps and cores: unaffected; they ignore the new key, the directory keeps `{"type":"search"}`. A newer app on an older core (desktop with an older phone): `/_servers` has no services, so the app shows none and hides "Search in Directory"; the new commands return `CECommandError`. In practice desktop and phone run the same release (the minimum versions rise with each release).
- Bot API: the new commands stay undocumented like the servers commands beside them (bots/src/API/Docs/Commands.hs `undocumentedCommands`); `Preferences.protocols` is documented (it is part of the profile type bots use). For services written as bots, a Haskell helper module (`Simplex.Chat.Bot.MCP`) answers the envelope, `server/discover`, `tools/list` and `tools/call` from a list of tools with handlers; the directory and statistics bots use it; a TypeScript helper (ep's "planned" bot-library helper) is a follow-up.
- Migration: one, `M2026MMDD_services`, dated after the newest on master and on the branch when group B starts; SQLite and Postgres.

### Core API (new commands)

- `APITestService {userId, address, requestTimeout}` (`/_service test <userId> <address> [timeout=<s>]`, 10 s by default) → `CRServiceTestResult {serviceProfile, mcpVersions, tools, availableRoles, moreTools :: Bool, testFailure :: Maybe ServiceTestFailure}`, like `APITestChatRelay`. Steps: fetch the link data (no connection); require a contact address whose profile declares `protocols.mcp` with `transports.rpc` and a version the core speaks; send `tools/list`; parse.
- `APIDirectorySearch {userId, serviceId, requestTimeout, request :: {searchText, searchCursor?}}` (`/_directory search <userId> <serviceId> [timeout=<s>] <json>`, 10 s by default) → `CRDirectorySearchResults {user, serviceId, results}`; refuses a service that isn't in use for search: not this user's, doesn't fill the role, the role turned off, or its operator turned off.
- `APISubmitTelemetry {userId, serviceId, report}` (group D) → `CRTelemetrySubmitted`; the same refusals for the telemetry role, and refusals when the return-queue check or the route check fails (section 4, Telemetry), each a distinct error so the app can show its status line; the report is a closed record the core encodes, so an app can't add fields.
- Generic use (bots, AI agents) needs nothing new: `/_service_request` already carries any envelope.

### What the core validates and what the apps do

The core: the envelope and JSON-RPC shape (`id` 1, `resultType` absent or "complete"); tool lists (first page, at most 64 names, each 1–128 chars of the MCP charset, others dropped; schemas, descriptions, icons and annotations ignored, never stored, never fetched); role results through typed parsers (entries decoded one at a time, bad ones dropped, capped at 50, each must be a group or channel link, images ≤ 12,500 chars as today, `searchCursor` ≤ 1,024 bytes encoded, counts bounded); names via `mkValidName`; that the role is in use before sending. The apps: consent, the fan-out, per-service state, rendering.

### Alternatives considered (the whole design)

- **Directories like chat relays (plan 015, option A; the operators design):** a directory is its own list, no roles, no MCP; about 2,000 fewer lines for directories alone (about 3,350 against about 5,400 here, submenu included: the MCP layer in the core and the directory, the core's role call, and the per-role switches are the difference); every future kind of service is new storage, presets, screens and protocol again. Statistics in that design would be its own list (a table, presets and merge reusing the relay helpers, a request format, a settings section and the switch): about 1,400–1,600 lines by the same measures (core ~650, each app ~300–350, the bot ~200), against about 900 here. So with statistics the totals are about 4,850 there and 6,300 here: statistics alone recovers only 500–700 of the 2,000, and the layer pays for itself in lines only after three or four kinds of service beyond directories. Choosing that design now and moving to services later would cost about the same extra 2,000 later, plus about 600 to rework what was built in between (the directory table migrated to services with tools and roles, its types and API renamed, the settings section and per-role switches in both apps). What it buys sooner is one protocol shared with ep's spec and AI agents, and no new storage or screens per service. Part of the extra, the `protocols` key (A1) and the bot helper (A4's generic part), is work ep's draft already lists as planned for AI agents. This design reuses its storage pattern almost entirely.
- **Services matched by schema compatibility instead of tool names:** would let a service name its tools freely, but needs a JSON Schema comparator in the core, which is undecidable in general and a denial-of-service surface on phones. Rejected.
- **App-side MCP (apps build and parse the envelope):** no new core commands, but the same parsing twice (Swift, Kotlin), no bound in one place, and no check in the core that a role is in use. Rejected.
- **The core fans out a search itself and streams events:** one command per search, but per-service progress and retry would need events and remote-desktop forwarding of them. Rejected: the apps fan out, as plan 015 planned.

## 5. Compatibility and security

- Wire protocol: no change to the chat protocol or agent; Service RPC payloads change only for new apps calling new services. The directory serves both shapes. Profiles gain an optional `preferences.protocols` key (ep's spec; older clients keep it as raw JSON).
- The amended size limit: a body over 10,737 bytes is sent compressed (`compressBodyTo` passes smaller bodies through unchanged, Protocol.hs:1066-1076@B). Every app version that searches has the decompression (it came with the search branch); software on an older core (before #7588) reads only bodies small enough to travel uncompressed, which covers today's badge service exchanges and the directory's legacy pages that fit. A service talking to such software keeps its answers under 10,737 bytes.
- Database: one migration (`services`), up and down, SQLite and Postgres; no backfill (presets arrive through the start-up merge).
- Older and newer clients: section 4, "Storage, presets and compatibility".
- Adversary, a hostile or careless service the user added:
  - Oversized or deep bodies: bounded before parsing (10,737 bytes compressed, 64 KiB decompressed, depth 32; unchanged). Test: existing `serviceBodyTests`.
  - A huge or odd tool list: first page only, at most 64 names, invalid names dropped, everything but names ignored. Test: `testServiceToolsBounds` (a list of 1,000 tools, names with spaces, 200-char names, schemas with `$ref` to a URL: none fetched).
  - Claims the directory tool and returns junk: the typed parser rejects the response (that service's retry row), drops bad entries, caps at 50. Test: `testDirectoryRoleJunk`.
  - Claims a role it later drops, or adds one: stored tools change only on "Test service"; a role a user-added service gains starts off. Test: `testServiceRoleDefaults`.
  - Many services: at most five asked per search (question 7). Test (app): runner check with six services.
  - One slow service: 10 s timeout each, results shown as each arrives. Tests: `testDirectoryRoleTimeout` (a service that never answers gives a typed timeout error within the timeout); that a slow service doesn't hold up another is the apps' fan-out, checked on the runners with one test directory that never answers (a core chat test runs one command at a time, Terminal/Input.hs:58-65@M).
  - Name spoofing ("SimpleX Directory" added by anyone): the screens show the address's host under the name; the alert marks the user's own services; `mkValidName` strips format characters.
  - Prompt-like text in results: shown as data, as today's directory entries are.
- Adversary, the network or a service's operator: unchanged by this design for search unless option (b) is chosen (research §3, section 10: the return queue's server and the proxy, usually SimpleX Chat's for a default user, can link a user's searches); telemetry waits for the simplexmq change and the core's two checks (question 6).
- Consent per role: the Directory search switch (exists) and the Share usage statistics switch (new, off); per service, a switch per role; the core refuses a role call to a service not in use for that role (role off, or its operator off), so a bug in an app can't send to a service the user didn't choose for that role. The global switches stay app preferences, as "Directory search" is today: the core can't check them.

## 6. Slices

Five groups, in this order: A (MCP in the core and the directory), then B (storing services and the directory role call), then C (the apps); D (statistics) after B and the simplexmq change; E optional. A comes before B because B's types use A3's tool names and roles. Each slice builds and passes its tests alone. Sizes are changed lines, rough: A ~1,350, B ~1,150, C1 ~1,300, C2 ~1,200, C3 ~400, D ~900. Basis and confidence: plan 015's estimates for the parts it shares, and the directory search branch's measured size for the app work (iOS ~1,060 and Kotlin ~1,020 changed lines, research §6); within about a third either way.

Order within the groups, with the reason for each dependency: A1 (A3's declaration check reads A1's typed field; A4's bot declares it) → A2 (the entry types A4 encodes and B4 decodes; structural, so it goes early) → A3 (defines `ToolName`, `ChatServiceRole`, the envelope and JSON-RPC types, and the roles' contract records, which A4 encodes and B4 decodes) → A4 → A5; then B1 → B2 → B3 → B4 (B4 looks up stored services).

### Group A: MCP over Service RPC (core and directory bot)

#### Slice A1: `protocols` in profile preferences (behavioural, additive)
- Size and risk: medium · high (profile wire format, though optional and specified by ep)
- Estimate: ~120 min
- Files: src/Simplex/Chat/Types/Preferences.hs:
  - `protocols :: Maybe (Map Text ProtocolDeclaration)` in `Preferences` (:185-195@M) and `GroupPreferences` (:397-411), with `ProtocolDeclaration {versions :: [Text], transports :: ProtocolTransports {chat, rpc :: Maybe Bool}}`, unknown keys of each kept;
  - `protocols :: MapDef Text ProtocolDeclaration` in `FullPreferences` (:476-485), empty in `defaultChatPrefs` (:553-564). `MapDef` is a new newtype like `ListDef` (:488-494) whose `omittedField` is the empty map, because aeson's `Map` instance has none (aeson fork FromJSON.hs:2073-2074) and these records derive JSON with TH (:1312, :1314): with a plain `Map`, a newer desktop couldn't decode an older phone's `User` or `GroupInfo` (`fullPreferences`, `fullGroupPreferences`, Types.hs:140, :524@M), which nearly every response carries;
  - the same `MapDef` field in `FullGroupPreferences` (:498-514), empty in `defaultGroupPrefs` (:569-586), carried by `mergeGroupPreferences` and `toGroupPreferences` (:1099-1139) as `commands` is (:1115), because the `/set … #group` feature commands rebuild a group's preferences through them (`setGroupPreference*`, :432-472; Commands.hs:3668-3673, :3690-3693@M) and would otherwise drop a group's or business chat's declaration;
  - `mergePreferences` (:1054-1064) and `fullPreferences'` (:1081-1091) take the user's own value only (no per-contact override, unlike `commands` at :1064, since a declaration is what this profile serves); the incognito branch of `userProfileDirect` (Types.hs:808-818@M) gets none;
  - `toChatPrefs` (:539-550) writes it back; `businessGroupPrefs` (:591-600) copies it into a business chat's `groupPreferences`, as ep's spec requires ("MUST be copied … as `commands` are");
  - the bot API docs entries for `Preferences`, `GroupPreferences`, `FullPreferences` and `FullGroupPreferences` (bots/src/API/Docs/Types.hs:291-310); generated TS and Python types (the docs test regenerates them).
- Change: decode, encode and send the key. Today a profile the core sends (address link data, `setMyAddressData`, Commands.hs:4152-4159@M, and `XInfo`, :4140-4146) is rebuilt by `userProfileDirect` (Types.hs:808-811@M) from the typed `FullPreferences`, and `toChatPrefs` sets `_json = NoJSON Nothing`, so an untyped key never reaches link data or contacts; `StrictData` would force a value there, and `Nothing` would drop the declaration silently, so the merge must carry it explicitly. Nothing reads it yet.
- Tests to add or update: `testProtocolsPreference` (a bot sets it with `/_profile`; its address's link data, read with `/_connect plan`, and the profile a new contact receives both carry it; an incognito connection carries none; a profile without it parses); `testProtocolsBusinessChat` (a business address declaring it: the business chat's `groupPreferences` carry it, and still do after `/set voice #<chat> off`); JSON tests, including decoding a `User` and a `GroupInfo` whose full preferences lack the key, and `CRActiveUser` through `RRResult ChatResponse` (the remote reply); `-m "Bot API docs"`.
- Verify: `cabal build lib:simplex-chat`, `cabal build simplex-chat-test`, then `-m "protocols"`, `-m "JSON Tests"`, `-m "Bot API docs"` with non-zero example counts.
- Done when: AC1.

#### Slice A2: move the directory's entry types into the core library (structural)
- Size and risk: small · low
- Estimate: ~45 min
- Files: from apps/simplex-directory-service/src/Directory/Rpc.hs `DirectorySearchEntry` (Rpc.hs:32-42@B, with its JSON instance), and from Directory/Listing.hs `DirectoryEntryType` and `PublicLink` (Listing.hs:75-88@B), which depend only on core types (`GroupType`, `GroupMemberAdmission`, `GroupSummary`, `ConnReqContact`, `ShortLinkContact`), to a new library module src/Simplex/Chat/Directory/Entry.hs; Listing.hs and Rpc.hs import it; simplex-chat.cabal. The request and response records (`DirectoryRequest`, `DirectoryResponse`) stay in the bot: they are tagged (`"type":"search"`) and use the bot's private `SearchCursor` (Directory/Search.hs:20-26@B), so they are the legacy request's types, not the role's.
- Change: a move with no edits, so the bot and the core share one definition of an entry; the directory's tests prove it.
- Tests: none new; `-m "SimpleX Directory service bot"` unchanged.
- Done when: the move compiles and the directory suite passes.

#### Slice A3: the MCP client in the core and `APITestService` (behavioural)
- Size and risk: medium · high (parses untrusted input)
- Estimate: ~120 min
- Files: src/Simplex/Chat/Services/MCP.hs (new: envelope, JSON-RPC request and response types, `toolsListRequest`, `toolCallRequest`, result parsers with the bounds of section 4), src/Simplex/Chat/Services/Roles.hs (new: `ChatServiceRole`, `roleTools`, `serviceRoles`, `ToolName` with its charset check, and the directory role's untagged contract records: the search request `{searchText, searchCursor :: Maybe J.Value}` and response `{entries :: [J.Value], searchCursor :: Maybe J.Value, moreCount, pageSize}`, entries kept as raw values so the reader can drop bad ones one by one), src/Simplex/Chat/Controller.hs (command, response, `ServiceTestFailure`), src/Simplex/Chat/Library/Commands.hs (handler: link data via the connect-plan path, declaration check, `sendServiceRequestTo`; one timeout, 10 s unless the command passes `timeout=`, bounds the whole call, the link fetch included, since `resolveServiceTarget` fetches the stored short link before sending and the agent's timeout covers only the request, Commands.hs:5993-6009@B), src/Simplex/Chat/View.hs, simplex-chat.cabal, bots docs (`undocumentedCommands`, `undocumentedResponses`).
- Tests: unit tests of the parsers (good, envelope error, JSON-RPC error, -32022, `isError`, wrong `id`, unknown `resultType`, oversized list, bad names, `$ref` schemas); `testServiceTest` against a test bot in tests/ChatTests/Services.hs (failures: plain contact address, no declaration, chat-only declaration, timeout).
- Verify: build, `-m "services"`, `-m "Bot API docs"`.
- Done when: AC2, AC3.

#### Slice A4: the MCP helper for bots and the directory over MCP (behavioural)
- Size and risk: medium · medium (the live directory; legacy must stay byte-identical)
- Estimate: ~120 min
- Files: src/Simplex/Chat/Bot/MCP.hs (new, using A3's envelope and JSON-RPC types: answer the envelope; `server/discover`, `tools/list`, `tools/call` dispatch to handlers; envelope errors `unsupported_protocol`, `bad_request`, `too_large`; JSON-RPC -32601, -32602, -32022), apps/simplex-directory-service/src/Directory/Service.hs (route requests with `protocol` to the helper, others to today's path, both under the one in-flight cap; at start-up compare the whole wanted preferences, `commands` and `protocols`, instead of `commands` alone (Service.hs:238@B), so a running directory publishes its declaration on its first start with this version; declare `{"mcp": {"versions": ["2026-07-28"], "transports": {"rpc": true}}}` only), Directory/Rpc.hs (the tool definition with input and output schemas; the search handler converts its `SearchCursor` to and from the opaque `searchCursor` with its existing JSON instances; the page fit runs on the final compressed envelope).
- Tests: `testDirectoryMCPDiscover`, `testDirectoryMCPTools`, `testDirectoryMCPSearch` (paging, literal, too long, busy, two image entries paged, via MCP), `testDirectoryDeclaresProtocols` (a directory started with an old profile publishes the declaration in its link data), and all of today's `testDirectorySearchRpc*` unchanged.
- Verify: build, `-m "SimpleX Directory service bot"`.
- Done when: AC4.

#### Slice A5: amendments to the draft spec (docs)
- Size and risk: small · low · ~30 min
- Files: docs/protocol/agent-protocols.md (the three amendments of section 4, if ep agrees; otherwise this slice is dropped and the rules live in the role contracts' module comments).
- Done when: ep has approved the text.

### Group B: storing services and the directory role call (plan 015's run A, retargeted)

#### Slice B1: services in the servers types, all empty; shared relay checks extracted (structural)
- Size and risk: small · low · ~60 min
- As plan 015 slice 1 with `UserService` for `UserDirectory`: `services :: ListDef UserService` in `UserOperatorServers`, `Maybe [AUserService]` in `UpdatedUserOperatorServers`, `getUserServers` returns a 5-tuple, the duplicate-address fold and the in-use check extracted with relays as their only users; JSON tests decoding without the key, including `CRUserServers` through `RRResult ChatResponse`.
- Verify: `-m "Operators"`, `-m "chat relay tests"`, `-m "get and set SMP servers"`, `-m "create user with same servers"`, `-m "JSON Tests"`, `-m "Bot API docs"`.

#### Slice B2: the services table, store, grouping and save (behavioural)
- Size and risk: medium · high (migration) · ~90 min
- As plan 015 slice 2: the migration (SQLite, Postgres, both Migrations.hs, simplex-chat.cabal), `getServices`, `insertService`, `updateService`, delete-on-save for non-presets, grouping by `domains`, names via `mkValidName` in `APISetUserServers` and `APIValidateServers`, tool names validated on save, roles a service doesn't fill dropped on save, unknown role names dropped on read (column and JSON); View; `testGetSetServices`, a store test reading a row whose `roles` holds an unknown name, and a JSON test decoding a service with one.
- Verify: `-m "Schema dump"`, `-m "services"`, plus B1's.

#### Slice B3: presets, adoption, dropped presets, validation, new profiles (behavioural)
- Size and risk: medium · high (start-up merge for every user) · ~90 min
- As plan 015 slice 3, with its last review's four P2s: the pre-grouping pass (adopt rows at a preset's address; a preset its still-preset operator no longer lists becomes the user's own, turned off); OperatorTests cases for `-s`, for an operator missing from the presets, for adoption before the drop check, each over every group; a restart test in tests/ChatTests/Services.hs with `testCfg` variants (`withNewTestChatCfgOpts`); `copyServers` copies services; validation (duplicate address error, "no service for search" warning only when the request carries services).
- Verify: `-m "Operators"`, `-m "services"`, B1's list.

#### Slice B4: the directory role call (behavioural)
- Size and risk: small · medium · ~60 min
- Files: Controller.hs, Commands.hs (`APIDirectorySearch` with an optional `timeout=` like `/_service_request`, 10 s by default as the apps use today, bounding the whole call including the link fetch, as in A3: look up the stored service, check it is in use for search including its operator group, `tools/call`, read A3's contract records, decode the entries one at a time as `DirectorySearchEntry` (A2), keep the first 50 that decode and are group or channel links, refuse a `searchCursor` over 1,024 bytes encoded), View.hs, docs lists.
- Tests: `testDirectoryRoleSearch`, `testDirectoryRoleRefused` (unknown service id, another user's service, role off, tool missing, operator off), `testDirectoryRoleJunk` (one malformed entry and one contact link among good entries: the good ones come back; 60 entries: 50 come back), `testDirectoryRoleTimeout` (`timeout=2` against a service that never answers: a typed timeout error within the test harness's 20 s per line).
- Verify: build, `-m "services"`, `-m "SimpleX Directory service bot"`.
- Done when: AC5.

### Group C: the apps (first release)

#### Slice C1: iOS (behavioural)
- Size and risk: large · medium · ~2 days with runner checks
- Settings: `UserService` types (roles as strings, `services` optional), the Services section on the operator screen and Your servers, Add server › Service, `ServiceView` / `NewServiceView` with Test service and per-role switches, built from `ChatRelayView`'s shared pieces; the new warning and error cases. Search: `OnlineSearchModel` with per-service state (cursor, count, loading, failed), the fan-out (each request off the cooperative pool, at most five), merge and dedupe by link, per-service retry rows, the load row across services, the alert naming services and marking the user's own (board 13's corrected text), the remembered-consent list from plan 015's question 2; `DIRECTORY_SERVICE_LINK` removed; product docs and specs; CI: the iOS smoke test adds the test service in Settings and turns the preset off.
- Verify: ios-sim on a runner (push session); not compiled in the container.

#### Slice C2: Kotlin (behavioural)
- Size and risk: large · medium · ~2 days
- C1 mirrored file for file, every difference listed; desktop-linux and android-emu runs.

#### Slice C3: the Directories submenu (behavioural, both platforms)
- Size and risk: medium · low · ~1 day
- Boards 16–17: the long-press menu lists the services whose pages contained the result; a tap opens that service's screen.

### Group D: usage statistics (second role)

- D0 (simplexmq, prerequisite), not yet planned, needs an owner (question 12): (i) required under every option: export pure functions that give, for a user's agent config, a target address and an optional operator to avoid, the return queue's candidate servers, the proxy's candidate servers, and whether the request would be proxied, the same functions the agent's own choices use (`getNextServer_`/`usedOperatorsHosts`, `shouldUseProxy`, Client.hs:1189-1208, :2516-2534, :695-705@Q), so the core's checks can't drift (~150 lines); (ii) under option (b): a service request may name an operator to avoid; the agent prefers other operators for the return queue and the proxy, allows enabled servers without the storage role for the return queue, and falls back as today; service requests only, not other joins; the stored `JRServiceReq` command gains the field with an encoding that reads old rows (a few hundred lines with tests; changes search and badge requests too); (iii) optional hardening: return queues for service requests on their own transport session.
- D1 (core and bot, ~450 lines, high: privacy): `CSRTelemetry` and the `simplex.telemetry.submit` contract records (in Roles.hs, added here rather than in A3, so no screen offers a statistics switch before statistics exist), `APISubmitTelemetry` (the refusals of section 4: not in use, the return-queue check, the route check, using D0's functions), the preset statistics service, `simplex-telemetry-service` on the MCP helper (counts per day, platform, version; stores nothing else). Tests: the unit tests of the two checks and `testTelemetrySubmit` as in AC8, refusals, the bot's counters.
- D2 (both apps, ~500 lines): the Privacy switch, its alert with the exact report, the daily timer with a random moment and the once-a-day mark per device, the active non-hidden profile, nothing while a desktop controls a phone, the status lines (sent, no service, servers would identify you, private routing off), "Use for usage statistics" on service screens.
- Before release: deploy the statistics bot and its address (an operator step), and add the address to the presets.

### Group E (optional follow-ups)

- A TypeScript helper in packages/simplex-chat-nodejs that serves MCP tools over Service RPC (ep's planned bot-library helper), with `startChat` allowing service requests.
- The directory's legacy `{"type":"search"}` removed once no supported app sends it.

Smallest useful first release: **groups A, B and C1–C2** (several directories, added in Settings, searched together). C3 (submenu) and group D follow. Before that, the directory search pull request ships as it is (question 2).

Before the first release with services: whoever operates SimpleX Directory deploys the version from A4 (with MCP and its address's keys); this blocks that release, and breaks nothing for released apps, which keep sending today's request.

What reuses plan 015 and what replaces it: group B is plan 015's run A with the type renamed, two columns added and its last review's fixes applied; C1–C3 are its runs B–D with "directory" read as "service in use for search". Replaced: its "Test directory" (connect plan and an empty search) by "Test service" (declaration and `tools/list`); its app-built search JSON by the core's typed role call; its "Use for search" flag by per-role switches.

## 7. Test plan

- New tests and what each proves: section 6 per slice; the hostile-input unit tests prove the core bounds every field a service controls; `testDirectoryRoleRefused` proves the core sends nothing to a service whose role is off; the restart test proves the start-up merge on a real database.
- Existing tests that must stay green: today's directory RPC tests (`-m "SimpleX Directory service bot"`), service request tests (`-m "service request"`), "Operators", "chat relay tests", server tests, "create user with same servers", "JSON Tests", "Schema dump", "Bot API docs", "group link connection plan".
- Full-suite command for the final check: on the host, `cabal test simplex-chat-test` (regenerates `chat_query_plans.txt`), and the Postgres build and tests for the migration.
- Pace: A1, A3, B2, B3 and D1 are high risk: each built and tested alone with a lean review after it. A2 and B1 are structural: built and tested with their neighbour, no separate review. A4 with one review; B4 with one review; A5 is docs. Groups A and B as two ship runs one after the other (~7 h and ~6 h with reviews); C1 and C2 as their own runs (~2 days each with runner checks); C3 and D as later runs.

## 8. Risks

- Riskiest assumption: that a service request carrying ep's envelope with MCP 2026-07-28 messages fits every role within 10,737 bytes, including a directory page with images. A4 tests it first: the directory's page fit runs on the enveloped response, and `testDirectoryMCPSearch` pages two image entries.
- MCP revisions change: the core speaks one revision; a service answering -32022 gives "needs an app update". The tools part of MCP has changed only additively across revisions (research §2), and roles read only `structuredContent` and names.
- Remote desktop pairs: new keys are optional and new commands fail cleanly; pairs run the same release in practice.
- The live directory must be upgraded (MCP and its address keys) before the first app with the service layer ships; a release-checklist item, not code. If it isn't: older apps are unaffected (they send today's request, which the directory keeps answering), but in new apps every search to SimpleX Directory fails with its retry row ("Test service" says it doesn't declare MCP), until the directory is updated.
- Telemetry depends on decision 6 (by default only SimpleX Chat stores messages, so a SimpleX Chat statistics service can't be reached from default devices without option (b)), on a simplexmq change outside this repo that isn't planned yet, and on deploying the statistics bot; its counts are indicative only (opt-in, unauthenticated).

## 9. Open questions

Each with the recommended answer and what changes otherwise. Questions 1–6 are ep's (section 0); 7–12 are smaller.

1. Protocol. Recommended: ep's envelope with MCP 2026-07-28, the three rules of section 4, and the three amendments to the draft. If our own MCP-shaped JSON: ~150 fewer core lines, a second dialect beside ep's spec. If today's requests plus "describe": ~1,000 fewer lines now (group A shrinks to a "describe" request in the bot and its parsing in the core), no reuse by AI agents, each role its own request type. Under either alternative only group A changes: services are still stored with their tools and roles (group B), and the apps (group C) don't see the wire format, since the core's role call hides it.
2. Order. Recommended: release the one-directory search pull request first; the service layer replaces its constant and request in a later release, and the bot keeps the old request until those versions are unsupported. If held: the search waits for groups A–C (about six working days of runs, plus the host's full test suite and the app checks on runners) and the old request never ships.
3. Old directories. Recommended: no adapter; directories are updated. If an adapter: ~60 core lines and a test, kept as long as any old directory exists.
4. Where services are configured. Recommended: per operator and under Your servers (boards 10–12 renamed). If one Services list: a new screen, operators no longer cover preset services.
5. Telemetry content. Recommended: date, platform, major.minor version. Either way the counts cover only devices that opted in and can be inflated by anyone (no key, by design). If network health is added: bucketed send success and failure per preset operator, more useful and more identifying.
6. Who runs statistics, and where the return queue and proxy go (section 4, Telemetry). Recommended: (b) SimpleX Chat runs it, and simplexmq lets a service request name an operator to avoid, for the return queue and the proxy, with today's fallback (D0(ii)). If (a), Flux runs the bot on an enabled Flux server: no change to the agent's choices, Flux holds the counts, searches stay as today. If (c), as is: almost no device passes the checks; not worth building. Under every option the core checks the return queue's and the proxy's candidates before sending (D0(i)).
7. Services asked per search. Recommended: at most five, in settings order; Settings says which. If no cap: every added directory receives every search and each costs a reply queue.
8. A preset an operator stops listing. Recommended: kept, turned off, as the user's own (no new store function; the user's choice survives a later re-preset). If deleted: plan 015's delete path and its tests.
9. Consent. Recommended: one switch per role in Privacy plus a switch per role on each service, and the core refusing role calls to services not in use for that role (role or operator off). If one switch per role only: a user-added service that also offers statistics would receive reports when the global switch is on.
10. Tool lists refreshed. Recommended: only on "Test service" (and presets by app updates). If by MCP's `ttlMs`: background requests on a timer, each a reply queue, for little gain.
11. The statistics service as a preset. Recommended: a preset of the operator that runs it (question 6), sending nothing until the user turns the switch on.
12. Who plans and owns the simplexmq change (D0). Recommended: ep assigns it in simplexmq after the first release; its required part is small (exported pure functions for the two checks, D0(i)) and, under option (b), a service request naming an operator to avoid for the return queue and the proxy (D0(ii)). If nobody owns it: statistics can't ship.

## 10. Out of scope (noticed, not doing)

- A user with private routing "unknown servers" (iOS's default until 2026-10-21) or "never" sends searches directly to the directory's server when it is in their server list (research §3); unchanged here.
- For default users every search's return queue is on a SimpleX Chat server, the directory's own operator, on the session that carries the user's other queues there (Flux doesn't store by default); the search review called this "can be", it is "always". And about two searches in three reach SimpleX Directory through SimpleX Chat's own proxy, which sees the searcher's IP address, while the directory (same operator) sees the search. Option (b) of question 6 would change both for search too; otherwise they stay, and the alert promises nothing about either.
- The directory's cursor carries the group's database id (`lastGroupId`, research §4); keeping it opaque is the bot's business.
- The badge service could move to ep's envelope as its own protocol name.
- `@simplex-chat/mcp`'s planned `mcp_tools` and `mcp_call` would reach the same services; no dependency either way.

## 11. Comparison with the operators design

(Added about an hour before the wrap-up, if that design's plan is available.)

## 12. Follow-up runs

- Run A (MCP in the core and the directory, A1–A5), then Run B (storage and the role call, B1–B4), Run C1 (iOS), Run C2 (Kotlin), Run C3 (submenu), Run D (telemetry, after simplexmq D0), Run E (TypeScript helper), each planned in full when it starts, from this design.
