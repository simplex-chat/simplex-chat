import type { BadgeType } from "./catalog.js";

// The equity crowdfunding on Wefunder, while the round is open. Set ACTIVE to false when it closes
// and every invest panel disappears without touching a screen.
export const CROWDFUNDING_ACTIVE = true;

export const WEFUNDER_URL = "https://wefunder.com/simplex.chat?utm_source=badges";

// The badge each investment amount buys as a perk, in dollars. The months are the catalog's terms,
// so a chosen tier and term always names one amount.
export const INVESTOR_PERKS: Readonly<Record<BadgeType, Readonly<Record<number, number>>>> = {
  supporter: { 1: 100, 3: 250, 12: 1000 },
  legend: { 1: 1000, 3: 2500, 12: 10000 },
};

export function perkAmount(badgeType: BadgeType, months: number): number | undefined {
  return INVESTOR_PERKS[badgeType]?.[months];
}

export function minimumFor(badgeType: BadgeType): number {
  return Math.min(...Object.values(INVESTOR_PERKS[badgeType]));
}

export function dollars(amount: number): string {
  return `$${amount.toLocaleString("en-US")}`;
}
