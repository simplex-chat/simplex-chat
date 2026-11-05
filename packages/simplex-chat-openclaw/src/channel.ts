import { describeAccountSnapshot } from "openclaw/plugin-sdk/account-helpers";
import {
  createScopedDmSecurityResolver,
  createTopLevelChannelConfigAdapter,
} from "openclaw/plugin-sdk/channel-config-helpers";
import type { ChannelOutboundAdapter } from "openclaw/plugin-sdk/channel-contract";
import { createChatChannelPlugin } from "openclaw/plugin-sdk/channel-core";
import { missingTargetError } from "openclaw/plugin-sdk/channel-feedback";
import { createChannelMessageAdapterFromOutbound } from "openclaw/plugin-sdk/channel-outbound";
import {
  DEFAULT_ACCOUNT_ID,
  formatPairingApproveHint,
  type ChannelPlugin,
} from "openclaw/plugin-sdk/channel-plugin-common";
import {
  buildChannelOutboundSessionRoute,
  type ChannelOutboundSessionRouteParams,
} from "openclaw/plugin-sdk/core";
import { buildPassiveChannelStatusSummary } from "openclaw/plugin-sdk/extension-shared";
import {
  collectStatusIssuesFromLastError,
  createComputedAccountStatusAdapter,
  createDefaultChannelRuntimeState,
} from "openclaw/plugin-sdk/status-helpers";
import {
  normalizeSimplexTarget,
  parseSimplexContactId,
  readSimplexSection,
  resolveSimplexAccount,
  SIMPLEX_CHANNEL_ID,
  type ResolvedSimplexAccount,
} from "./account.js";
import {
  simplexOutboundAdapter,
  simplexPairingTextAdapter,
  startSimplexGatewayAccount,
} from "./gateway.js";

const SIMPLEX_TARGET_HINT = "<contact id|simplex:contact id>";

const resolveSimplexDmPolicy = createScopedDmSecurityResolver<ResolvedSimplexAccount>({
  channelKey: SIMPLEX_CHANNEL_ID,
  resolvePolicy: (account) => account.config.dmPolicy,
  resolveAllowFrom: (account) => account.config.allowFrom,
  policyPathSuffix: "dmPolicy",
  defaultPolicy: "pairing",
  approveHint: formatPairingApproveHint(SIMPLEX_CHANNEL_ID),
  normalizeEntry: normalizeSimplexTarget,
});

const simplexConfigAdapter = createTopLevelChannelConfigAdapter<ResolvedSimplexAccount>({
  sectionKey: SIMPLEX_CHANNEL_ID,
  resolveAccount: resolveSimplexAccount,
  listAccountIds: (cfg) => (readSimplexSection(cfg) ? [DEFAULT_ACCOUNT_ID] : []),
  deleteMode: "remove-section",
  resolveAllowFrom: (account) => account.config.allowFrom,
  formatAllowFrom: (allowFrom) => allowFrom.map((entry) => normalizeSimplexTarget(String(entry))),
});

function resolveSimplexOutboundSessionRoute(params: ChannelOutboundSessionRouteParams) {
  const contactId = parseSimplexContactId(params.target);
  if (contactId === undefined) {
    return null;
  }
  return buildChannelOutboundSessionRoute({
    cfg: params.cfg,
    agentId: params.agentId,
    channel: SIMPLEX_CHANNEL_ID,
    accountId: params.accountId,
    recipientSessionExact: true,
    peer: { kind: "direct", id: String(contactId) },
    chatType: "direct",
    from: `simplex:${contactId}`,
    to: `simplex:${contactId}`,
  });
}

const simplexPluginOutboundAdapter: ChannelOutboundAdapter = {
  ...simplexOutboundAdapter,
  resolveTarget: ({ to }) => {
    const contactId = parseSimplexContactId(to ?? "");
    return contactId === undefined
      ? { ok: false, error: missingTargetError("SimpleX", SIMPLEX_TARGET_HINT) }
      : { ok: true, to: String(contactId) };
  },
};

export const simplexPlugin: ChannelPlugin<ResolvedSimplexAccount> = createChatChannelPlugin({
  base: {
    id: SIMPLEX_CHANNEL_ID,
    meta: {
      id: SIMPLEX_CHANNEL_ID,
      label: "SimpleX",
      selectionLabel: "SimpleX Chat",
      docsPath: "/channels/simplex",
      docsLabel: "simplex",
      blurb: "End-to-end encrypted direct messages without phone numbers or user IDs.",
      order: 60,
    },
    capabilities: {
      chatTypes: ["direct"],
      media: false,
    },
    reload: { configPrefixes: ["channels.simplex"] },
    config: {
      ...simplexConfigAdapter,
      isConfigured: (account) => account.configured,
      describeAccount: (account) =>
        describeAccountSnapshot({ account, configured: account.configured }),
    },
    setup: {
      applyAccountConfig: ({ cfg }) => ({
        ...cfg,
        channels: {
          ...cfg.channels,
          simplex: { ...readSimplexSection(cfg), enabled: true },
        },
      }),
    },
    messaging: {
      targetPrefixes: [SIMPLEX_CHANNEL_ID],
      normalizeTarget: normalizeSimplexTarget,
      inferTargetChatType: ({ to }) => (parseSimplexContactId(to) === undefined ? undefined : "direct"),
      targetResolver: {
        looksLikeId: (input) => parseSimplexContactId(input) !== undefined,
        hint: SIMPLEX_TARGET_HINT,
      },
      resolveOutboundSessionRoute: resolveSimplexOutboundSessionRoute,
    },
    message: createChannelMessageAdapterFromOutbound({
      id: SIMPLEX_CHANNEL_ID,
      outbound: simplexOutboundAdapter,
    }),
    status: createComputedAccountStatusAdapter<ResolvedSimplexAccount>({
      defaultRuntime: createDefaultChannelRuntimeState(DEFAULT_ACCOUNT_ID),
      collectStatusIssues: (accounts) => collectStatusIssuesFromLastError(SIMPLEX_CHANNEL_ID, accounts),
      buildChannelSummary: ({ snapshot }) => buildPassiveChannelStatusSummary(snapshot),
      resolveAccountSnapshot: ({ account }) => ({
        accountId: account.accountId,
        name: account.name,
        enabled: account.enabled,
        configured: account.configured,
      }),
    }),
    gateway: {
      startAccount: startSimplexGatewayAccount,
    },
  },
  pairing: {
    text: simplexPairingTextAdapter,
  },
  security: {
    resolveDmPolicy: resolveSimplexDmPolicy,
  },
  outbound: simplexPluginOutboundAdapter,
});
