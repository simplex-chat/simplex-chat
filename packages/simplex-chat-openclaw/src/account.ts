import { DEFAULT_ACCOUNT_ID } from "openclaw/plugin-sdk/account-id";
import type { OpenClawConfig } from "openclaw/plugin-sdk/config-contracts";

export const SIMPLEX_CHANNEL_ID = "simplex";

export type SimplexAccountConfig = {
  enabled?: boolean;
  name?: string;
  dmPolicy?: string;
  allowFrom?: Array<string | number>;
};

export type ResolvedSimplexAccount = {
  accountId: string;
  name: string;
  enabled: boolean;
  configured: boolean;
  config: SimplexAccountConfig;
};

export function readSimplexSection(cfg: OpenClawConfig): SimplexAccountConfig | undefined {
  return (cfg.channels as Record<string, SimplexAccountConfig | undefined> | undefined)?.simplex;
}

export function resolveSimplexAccount(cfg: OpenClawConfig): ResolvedSimplexAccount {
  const config = readSimplexSection(cfg) ?? {};
  return {
    accountId: DEFAULT_ACCOUNT_ID,
    name: config.name ?? "OpenClaw",
    enabled: config.enabled !== false,
    configured: readSimplexSection(cfg) !== undefined,
    config,
  };
}

export function normalizeSimplexTarget(target: string): string {
  return target.trim().replace(/^simplex:/i, "");
}

export function parseSimplexContactId(target: string): number | undefined {
  const id = normalizeSimplexTarget(target);
  return /^\d+$/.test(id) ? Number(id) : undefined;
}
