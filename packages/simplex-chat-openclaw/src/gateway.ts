import { mkdir } from "node:fs/promises";
import path from "node:path";
import { T } from "@simplex-chat/types";
import type { api } from "simplex-chat";
import { DEFAULT_ACCOUNT_ID } from "openclaw/plugin-sdk/account-id";
import type { ChannelOutboundAdapter } from "openclaw/plugin-sdk/channel-contract";
import type {
  ChannelIngressContextBinding,
  StableChannelIngressIdentityParams,
} from "openclaw/plugin-sdk/channel-ingress-runtime";
import { runPassiveAccountLifecycle } from "openclaw/plugin-sdk/channel-outbound";
import { createChannelPairingController } from "openclaw/plugin-sdk/channel-pairing";
import type { ChannelPlugin } from "openclaw/plugin-sdk/channel-plugin-common";
import { attachChannelToResult } from "openclaw/plugin-sdk/channel-send-result";
import type { OpenClawConfig } from "openclaw/plugin-sdk/config-contracts";
import { channelReadyPatch } from "openclaw/plugin-sdk/gateway-runtime";
import type { PluginRuntime } from "openclaw/plugin-sdk/plugin-runtime";
import { resolveStateDir } from "openclaw/plugin-sdk/state-paths";
import {
  chunkTextForOutbound,
  sanitizeAssistantVisibleText,
  stripMarkdown,
} from "openclaw/plugin-sdk/text-chunking";
import {
  normalizeSimplexTarget,
  parseSimplexContactId,
  SIMPLEX_CHANNEL_ID,
  type ResolvedSimplexAccount,
} from "./account.js";
import { getSimplexRuntime } from "./runtime.js";

type SimplexGatewayStart = NonNullable<
  NonNullable<ChannelPlugin<ResolvedSimplexAccount>["gateway"]>["startAccount"]
>;
type SimplexOutboundAdapter = Pick<
  ChannelOutboundAdapter,
  "chunker" | "deliveryCapabilities" | "deliveryMode" | "textChunkLimit" | "sendText"
> & {
  sendText: NonNullable<ChannelOutboundAdapter["sendText"]>;
  sanitizeText: NonNullable<ChannelOutboundAdapter["sanitizeText"]>;
};

const SIMPLEX_TEXT_LIMIT = 3000;
const activeClients = new Map<string, api.ChatApi>();

const simplexIngressIdentity = {
  key: "simplex-contact",
  normalizeEntry: (entry: string) => {
    const normalized = normalizeSimplexTarget(entry);
    return normalized === "*" || /^\d+$/.test(normalized) ? normalized : null;
  },
  normalizeSubject: (value: string) => (/^\d+$/.test(value) ? value : null),
  sensitivity: "pii",
  entryIdPrefix: "simplex-entry",
} satisfies StableChannelIngressIdentityParams;

function renderPlainText(cfg: OpenClawConfig, accountId: string, text: string): string {
  const runtime = getSimplexRuntime();
  const tableMode = runtime.channel.text.resolveMarkdownTableMode({
    cfg,
    channel: SIMPLEX_CHANNEL_ID,
    accountId,
  });
  return stripMarkdown(runtime.channel.text.convertMarkdownTables(text, tableMode));
}

async function sendTextChunks(client: api.ChatApi, contactId: number, text: string) {
  for (const chunk of chunkTextForOutbound(text, SIMPLEX_TEXT_LIMIT)) {
    await client.apiSendTextMessage([T.ChatType.Direct, contactId], chunk);
  }
}

export const startSimplexGatewayAccount: SimplexGatewayStart = async (ctx) => {
  const account = ctx.account;
  ctx.setStatus({ accountId: account.accountId, lifecycle: "starting" });
  const channelRuntime = ctx.channelRuntime as PluginRuntime["channel"] | undefined;
  if (!channelRuntime?.inbound?.buildContext) {
    throw new Error("SimpleX requires its registered channel runtime context builder");
  }
  const runtime = getSimplexRuntime();
  const pairing = createChannelPairingController({
    core: runtime,
    channel: SIMPLEX_CHANNEL_ID,
    accountId: account.accountId,
  });

  const resolveInboundAccess = async (
    contactId: string,
    rawBody: string,
    contextBinding?: ChannelIngressContextBinding,
  ) =>
    await channelRuntime.inbound.ingress.resolveStable({
      channelId: SIMPLEX_CHANNEL_ID,
      accountId: account.accountId,
      identity: simplexIngressIdentity,
      cfg: ctx.cfg,
      useDefaultPairingStore: true,
      subject: { stableId: contactId },
      conversation: { kind: "direct", id: contactId },
      contextBinding,
      dmPolicy: account.config.dmPolicy ?? "pairing",
      allowFrom: account.config.allowFrom,
      command: runtime.channel.commands.shouldComputeCommandAuthorized(rawBody, ctx.cfg)
        ? { modeWhenAccessGroupsOff: "configured" }
        : undefined,
    });

  const receiveMessage = async (client: api.ChatApi, chatItem: T.AChatItem, text: string) => {
    if (chatItem.chatInfo.type !== "direct" || !text) {
      return;
    }
    const contact = chatItem.chatInfo.contact;
    const contactId = String(contact.contactId);
    const reply = async (message: string) => {
      await sendTextChunks(client, contact.contactId, message);
    };
    const access = await resolveInboundAccess(contactId, text);
    if (access.senderAccess.decision === "pairing") {
      await pairing.issueChallenge({
        senderId: contactId,
        senderIdLine: `Your SimpleX contact id: ${contactId}`,
        sendPairingReply: reply,
        onCreated: () => {
          ctx.log?.info?.(`[${account.accountId}] SimpleX pairing request from contact ${contactId}`);
        },
        onReplyError: (err) => {
          ctx.log?.warn?.(
            `[${account.accountId}] SimpleX pairing reply to contact ${contactId} failed: ${String(err)}`,
          );
        },
      });
      return;
    }
    if (access.senderAccess.decision !== "allow") {
      ctx.log?.debug?.(
        `[${account.accountId}] blocked SimpleX contact ${contactId} (${access.senderAccess.reasonCode})`,
      );
      return;
    }
    const { dispatchInboundDirectDm } = await import("openclaw/plugin-sdk/channel-inbound");
    await dispatchInboundDirectDm({
      channelRuntime,
      resolveChannelIngress: async (contextBinding) => {
        const exactAccess = await resolveInboundAccess(contactId, text, contextBinding);
        if (!exactAccess.senderAccess.allowed) {
          throw new Error(`SimpleX contact authorization changed before dispatch (${contactId})`);
        }
        return exactAccess;
      },
      cfg: ctx.cfg,
      channel: SIMPLEX_CHANNEL_ID,
      channelLabel: "SimpleX",
      accountId: account.accountId,
      peer: { kind: "direct", id: contactId },
      senderId: contactId,
      senderAddress: `simplex:${contactId}`,
      recipientAddress: `simplex:${account.accountId}`,
      conversationLabel: contact.profile.displayName,
      rawBody: text,
      messageId: String(chatItem.chatItem.meta.itemId),
      timestamp: Date.parse(chatItem.chatItem.meta.itemTs),
      commandAuthorized: access.commandAccess.requested
        ? access.commandAccess.authorized
        : undefined,
      deliver: async (payload) => {
        const message = renderPlainText(
          ctx.cfg,
          account.accountId,
          sanitizeAssistantVisibleText(payload.text ?? ""),
        );
        if (message) {
          await reply(message);
        }
      },
      onRecordError: (err) => {
        ctx.log?.error?.(
          `[${account.accountId}] failed recording SimpleX inbound session: ${String(err)}`,
        );
      },
      onDispatchError: (err, info) => {
        ctx.log?.error?.(`[${account.accountId}] SimpleX ${info.kind} reply failed: ${String(err)}`);
      },
    });
  };

  await runPassiveAccountLifecycle({
    abortSignal: ctx.abortSignal,
    start: async () => {
      const { bot, util } = await import("simplex-chat");
      const dbDir = path.join(resolveStateDir(), "simplex");
      await mkdir(dbDir, { recursive: true });
      const started = bot.run({
        profile: { displayName: account.name, fullName: "" },
        dbOpts: { type: "sqlite", filePrefix: path.join(dbDir, account.accountId) },
        options: { logContacts: false },
        onMessage: (chatItem, content) => {
          started
            .then(([client]) => receiveMessage(client, chatItem, content.text))
            .catch((err) => {
              ctx.log?.error?.(
                `[${account.accountId}] SimpleX message handling failed: ${String(err)}`,
              );
            });
        },
      });
      const [client, , address] = await started;
      activeClients.set(account.accountId, client);
      ctx.log?.info?.(
        `[${account.accountId}] SimpleX address: ${
          address ? util.contactAddressStr(address.connLinkContact) : "none"
        }`,
      );
      ctx.setStatus(channelReadyPatch({ accountId: account.accountId }));
      return {
        stop: async () => {
          activeClients.delete(account.accountId);
          await client.close();
          ctx.log?.info?.(`[${account.accountId}] SimpleX client stopped`);
        },
      };
    },
    stop: async (handle) => {
      await handle.stop();
    },
  });
};

export const simplexPairingTextAdapter = {
  idLabel: "simplexContactId",
  message: "Your pairing request has been approved!",
  normalizeAllowEntry: normalizeSimplexTarget,
  notify: async ({ id, message, accountId }: { id: string; message: string; accountId?: string }) => {
    const client = activeClients.get(accountId ?? DEFAULT_ACCOUNT_ID);
    const contactId = parseSimplexContactId(id);
    if (client && contactId !== undefined) {
      await sendTextChunks(client, contactId, message);
    }
  },
};

export const simplexOutboundAdapter: SimplexOutboundAdapter = {
  deliveryMode: "direct",
  textChunkLimit: SIMPLEX_TEXT_LIMIT,
  chunker: chunkTextForOutbound,
  sanitizeText: ({ text }) => sanitizeAssistantVisibleText(text),
  deliveryCapabilities: {
    durableFinal: {
      text: true,
      messageSendingHooks: true,
    },
  },
  sendText: async ({ cfg, to, text, accountId, assertDirectAdapterHandoff, onPlatformSendDispatch }) => {
    const aid = accountId ?? DEFAULT_ACCOUNT_ID;
    const client = activeClients.get(aid);
    if (!client) {
      throw new Error(`SimpleX client not running for account ${aid}`);
    }
    const contactId = parseSimplexContactId(to);
    if (contactId === undefined) {
      throw new Error(`SimpleX target must be a contact id, got ${to}`);
    }
    const message = renderPlainText(cfg, aid, text ?? "");
    if (!message) {
      throw new Error("SimpleX send requires non-empty text after markdown stripping.");
    }
    assertDirectAdapterHandoff?.();
    if (onPlatformSendDispatch) {
      await onPlatformSendDispatch();
      assertDirectAdapterHandoff?.();
    }
    const [sent] = await client.apiSendTextMessage([T.ChatType.Direct, contactId], message);
    return attachChannelToResult(SIMPLEX_CHANNEL_ID, {
      to: String(contactId),
      messageId: String(sent.chatItem.meta.itemId),
    });
  },
};
