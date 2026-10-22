import {T} from "@simplex-chat/types"
import {chatErrorText} from "../../src/log"

const server = "smp://u2dS9sG8nMNURyZwqASV4yROM28Er0luVTx5X1CsMrU=@smp4.simplex.im,o5vmywmrnaxalvz6wi3zicyftgio6psuvyniis6gco6bp6ekl4cqj4id.onion"
const relay = "smp://hpq7_4gGJiilmz5Rf-CswuU5kZGkm_zOIooSw6yALRg=@smp5.simplex.im"
const agentError = (error: object): T.ChatError => ({type: "errorAgent", agentError: error, agentConnId: ""}) as unknown as T.ChatError
const broker = (brokerErr: object): object => ({type: "BROKER", brokerAddress: server, brokerErr})
const forwarding = (brokerErr: object): object => ({type: "SMP", serverAddress: server, smpErr: {type: "PROXY", proxyErr: {type: "BROKER", brokerErr}}})
const destination = (proxyErr: object): object => ({type: "PROXY", proxyServer: server, relayServer: relay, proxyErr: {type: "protocolError", protocolErr: {type: "PROXY", proxyErr}}})
const domain = {nameTLD: T.SimplexTLD.Simplex, domain: "ridgeline", subDomain: []}

describe("SimpleX errors", () => {
  const cases: [string, T.ChatError, string][] = [
    ["invalid link", {type: "error", errorType: {type: "invalidConnReq"}}, "Invalid connection link: Please check that you used the correct link or ask your contact to send you another one."],
    ["unsupported link", {type: "error", errorType: {type: "unsupportedConnReq"}}, "Unsupported connection link: This link requires a newer app version. Please upgrade the app or ask your contact to send a compatible link."],
    [
      "name without a valid link",
      {type: "error", errorType: {type: "simplexDomainNotReady", simplexDomain: domain, simplexDomainError: {type: "noValidLink"}}},
      "No valid link: The SimpleX name ridgeline.simplex is registered, but it has no valid link.",
    ],
    [
      "name not in a profile",
      {type: "error", errorType: {type: "simplexDomainNotReady", simplexDomain: domain, simplexDomainError: {type: "unknownDomain"}}},
      "Unconfirmed name: The SimpleX name ridgeline.simplex is registered, but not added to profile. Please add it to your address or channel profile, if you are the owner.",
    ],
    ["no name servers", agentError({type: "NO_NAME_SERVERS"}), "SimpleX name error: None of your servers are set to resolve SimpleX names. Use a connection link."],
    ["name not found", agentError({type: "SMP", serverAddress: server, smpErr: {type: "NAME", nameErr: {type: "NOT_FOUND"}}}), "Name not found: This SimpleX name is not registered. Please check the name."],
    [
      "no name resolver",
      agentError({type: "SMP", serverAddress: server, smpErr: {type: "NAME", nameErr: {type: "NO_RESOLVER"}}}),
      `SimpleX name error: Server ${server} does not support name resolution. Use a connection link.`,
    ],
    ["resolver error", agentError({type: "SMP", serverAddress: server, smpErr: {type: "NAME", nameErr: {type: "RESOLVER", resolverErr: "bad\nreply"}}}), "SimpleX name error: Resolver error: bad reply"],
    [
      "link removed",
      agentError({type: "SMP", serverAddress: server, smpErr: {type: "AUTH"}}),
      "Connection link removed: Your contact removed this link, or it was a one-time link that was already used. To connect, ask your contact to create a new link.",
    ],
    ["blocked", agentError({type: "SMP", serverAddress: server, smpErr: {type: "BLOCKED", blockInfo: {reason: T.BlockingReason.Spam}}}), "Connection blocked: Connection is blocked by server operator: Spam."],
    [
      "blocked content",
      agentError({type: "SMP", serverAddress: server, smpErr: {type: "BLOCKED", blockInfo: {reason: T.BlockingReason.Content}}}),
      "Connection blocked: Connection is blocked by server operator: Content violates conditions of use.",
    ],
    ["quota", agentError({type: "SMP", serverAddress: server, smpErr: {type: "QUOTA"}}), "Undelivered messages: The connection reached the limit of undelivered messages, your contact may be offline."],
    ["already connected", agentError({type: "INTERNAL", internalErr: "SEUniqueID"}), "Already connected? It seems like you are already connected via this link. If it is not the case, there was an error (SEUniqueID)."],
    ["timeout", agentError(broker({type: "TIMEOUT"})), "Connection timeout: Please check your network connection with smp4.simplex.im and try again."],
    ["network", agentError(broker({type: "NETWORK", networkError: {type: "connectError", connectError: "refused"}})), "Connection error: Please check your network connection with smp4.simplex.im and try again."],
    ["unknown CA", agentError(broker({type: "NETWORK", networkError: {type: "unknownCAError"}})), "Connection error: Fingerprint in server address does not match certificate: smp4.simplex.im."],
    ["host", agentError(broker({type: "HOST"})), "Connection error: Server address is incompatible with network settings: smp4.simplex.im."],
    ["server version", agentError(broker({type: "TRANSPORT", transportErr: {type: "version"}})), "Connection error: Server version is incompatible with your app: smp4.simplex.im."],
    ["forwarding timeout", agentError(forwarding({type: "TIMEOUT"})), "Private routing error: Error connecting to forwarding server smp4.simplex.im. Please try later."],
    ["forwarding network", agentError(forwarding({type: "NETWORK", networkError: {type: "failedError"}})), "Private routing error: Error connecting to forwarding server smp4.simplex.im. Please try later."],
    [
      "forwarding unknown CA",
      agentError(forwarding({type: "NETWORK", networkError: {type: "unknownCAError"}})),
      "Private routing error: Fingerprint in forwarding server address does not match certificate: smp4.simplex.im.",
    ],
    ["forwarding host", agentError(forwarding({type: "HOST"})), "Private routing error: Forwarding server address is incompatible with network settings: smp4.simplex.im."],
    [
      "forwarding version",
      agentError(forwarding({type: "TRANSPORT", transportErr: {type: "version"}})),
      "Private routing error: Forwarding server version is incompatible with network settings: smp4.simplex.im.",
    ],
    [
      "destination timeout",
      agentError(destination({type: "BROKER", brokerErr: {type: "TIMEOUT"}})),
      "Private routing error: Forwarding server smp4.simplex.im failed to connect to destination server smp5.simplex.im. Please try later.",
    ],
    [
      "destination network",
      agentError(destination({type: "BROKER", brokerErr: {type: "NETWORK", networkError: {type: "timeoutError"}}})),
      "Private routing error: Forwarding server smp4.simplex.im failed to connect to destination server smp5.simplex.im. Please try later.",
    ],
    [
      "no private routing session",
      agentError(destination({type: "NO_SESSION"})),
      "Private routing error: Forwarding server smp4.simplex.im failed to connect to destination server smp5.simplex.im. Please try later.",
    ],
    [
      "destination unknown CA",
      agentError(destination({type: "BROKER", brokerErr: {type: "NETWORK", networkError: {type: "unknownCAError"}}})),
      "Private routing error: Fingerprint in destination server address does not match certificate: smp5.simplex.im.",
    ],
    [
      "destination host",
      agentError(destination({type: "BROKER", brokerErr: {type: "HOST"}})),
      "Private routing error: Destination server address of smp5.simplex.im is incompatible with forwarding server smp4.simplex.im settings.",
    ],
    [
      "destination version",
      agentError(destination({type: "BROKER", brokerErr: {type: "TRANSPORT", transportErr: {type: "version"}}})),
      "Private routing error: Destination server version of smp5.simplex.im is incompatible with forwarding server smp4.simplex.im.",
    ],
    ["service timeout", agentError({type: "AGENT", agentErr: {type: "A_SERVICE", serviceError: {type: "timeout"}}}), "Connection timeout: The service did not respond. Please try again."],
    ["large message", {type: "errorStore", storeError: {type: "largeMsg"}}, "Message is too large! Please reduce the message size and send again."],
    ["group not found", {type: "errorStore", storeError: {type: "groupNotFound", groupId: 15}}, "Group not found! This group no longer exists."],
  ]

  it.each(cases)("reads as the apps word it: %s", (_, chatError, text) => {
    expect(chatErrorText(chatError)).toBe(text)
  })

  it("keeps the chain of error types and their scalar fields for other errors", () => {
    expect(chatErrorText({type: "error", errorType: {type: "groupUserRole", groupInfo: {groupId: 15}, requiredRole: T.GroupMemberRole.Author}} as unknown as T.ChatError)).toBe(
      'SimpleX error: error: groupUserRole: {"requiredRole":"author"}',
    )
    const noService = {...agentError(broker({type: "NO_SERVICE"})), connectionEntity_: {type: "rcvDirectMsgConnection", entityConnection: {connId: 1}}} as T.ChatError
    expect(chatErrorText(noService)).toBe(`SimpleX error: errorAgent: BROKER: NO_SERVICE: {"agentConnId":"","brokerAddress":"${server}"}`)
    expect(chatErrorText(agentError({type: "INTERNAL", internalErr: "other\nline"}))).toBe('SimpleX error: errorAgent: INTERNAL: {"agentConnId":"","internalErr":"other\\nline"}')
  })
})
