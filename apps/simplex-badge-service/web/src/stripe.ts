import type { Theme } from "./domain.js";

// Stripe.js must load from js.stripe.com and may not be bundled or self-hosted, so this is a script tag.
export const STRIPE_JS_URL = "https://js.stripe.com/dahlia/stripe.js";

const KEY_ELEMENT_ID = "stripe-publishable-key";

declare global {
  interface Window {
    Stripe?: StripeGlobal;
  }
}

export function publishableKey(): string {
  return document.getElementById(KEY_ELEMENT_ID)?.getAttribute("content")?.trim() ?? "";
}

let loading: Promise<StripeGlobal> | null = null;

// The tag is built here rather than in `screens`, which imports this module.
export function loadStripeJs(src: string): Promise<StripeGlobal> {
  if (loading !== null) return loading;
  loading = new Promise<StripeGlobal>((resolve, reject) => {
    // The src is set as an attribute because the page's script-src policy is written against the markup.
    const tag = document.createElement("script");
    tag.setAttribute("src", src);
    tag.setAttribute("async", "");
    tag.addEventListener("load", () => {
      const factory = window.Stripe;
      if (factory === undefined) {
        loading = null;
        reject(new Error("stripe: the script loaded and defined no global"));
        return;
      }
      resolve(factory);
    });
    tag.addEventListener("error", () => {
      loading = null;
      // A failed script tag will not load again, so it is removed before a retry.
      tag.remove();
      reject(new Error("stripe: the script did not load"));
    });
    document.head.append(tag);
  });
  return loading;
}

export const CONFIRM_FAILED = "The card was not accepted. Check the details and try again.";

export interface PaymentElement {
  mount(target: unknown): void;
  destroy(): void;
}

export interface StripeError {
  message?: string;
}

export interface StripeElements {
  create(kind: "payment"): PaymentElement;
}

export interface PaymentIntentResult {
  paymentIntent?: { status?: string };
  error?: StripeError;
}

export interface Appearance {
  theme: "stripe" | "night" | "flat";
  variables?: Record<string, string>;
  rules?: Record<string, Record<string, string>>;
}

export interface StripeInstance {
  elements(options: { clientSecret: string; appearance?: Appearance }): StripeElements;
  confirmPayment(options: {
    elements: StripeElements;
    confirmParams: { return_url: string };
    redirect: "if_required";
  }): Promise<PaymentIntentResult>;
}

// Stripe.js runs in its own iframe and cannot read the page's CSS variables, so the palette is
// duplicated here and kept in sync with public/styles.css by hand.
const CARD_FONT =
  'Satoshi, -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Oxygen, Ubuntu, Cantarell, "Fira Sans", "Droid Sans", "Helvetica Neue", sans-serif';

interface Palette {
  accent: string;
  onAccent: string;
  ink: string;
  bg: string;
  surface: string;
  line: string;
  muted: string;
  danger: string;
}

const LIGHT_PALETTE: Palette = {
  accent: "#3889FF", onAccent: "#FFFFFF", ink: "#1E2122", bg: "#F7F7F7", surface: "#FFFFFF", line: "#E8E8E8", muted: "rgba(30, 33, 34, .56)", danger: "#b3261e",
};
const DARK_PALETTE: Palette = {
  accent: "#3889FF", onAccent: "#FFFFFF", ink: "#FFFFFF", bg: "#141416", surface: "#1E2122", line: "#424347", muted: "rgba(255, 255, 255, .56)", danger: "#FF9F98",
};

// The flat theme is light-oriented, so dark mode must supply every colour down to the icons and
// spinner, or an unset value keeps the light default and shows as a light patch on a dark form.
function flatAppearance(p: Palette): Appearance {
  return {
    theme: "flat",
    variables: {
      colorPrimary: p.accent,
      colorBackground: p.surface,
      colorText: p.ink,
      colorTextSecondary: p.muted,
      colorTextPlaceholder: p.muted,
      colorDanger: p.danger,
      accessibleColorOnColorPrimary: p.onAccent,
      iconColor: p.ink,
      iconHoverColor: p.ink,
      iconCardCvcColor: p.muted,
      iconCardCvcErrorColor: p.danger,
      iconCardErrorColor: p.danger,
      iconChevronDownColor: p.muted,
      iconChevronDownHoverColor: p.ink,
      iconLoadingIndicatorColor: p.accent,
      tabIconColor: p.muted,
      tabIconSelectedColor: p.accent,
      logoColor: p.ink,
      accordionItemLabelColorText: p.ink,
      accordionItemLabelSelectedColorText: p.ink,
      fontFamily: CARD_FONT,
      fontSizeBase: "16px",
      borderRadius: "12px",
      spacingUnit: "4px",
    },
    rules: {
      ".Input": { backgroundColor: p.bg, color: p.ink, border: `1px solid ${p.line}`, boxShadow: "none" },
      ".Input:focus": { border: `1px solid ${p.accent}`, boxShadow: `0 0 0 1px ${p.accent}` },
      ".Input--invalid": { border: `1px solid ${p.danger}`, boxShadow: "none" },
      ".Input::placeholder": { color: p.muted },
      ".Label": { color: p.ink },
      ".Tab": { backgroundColor: p.surface, color: p.ink, border: `1px solid ${p.line}`, boxShadow: "none" },
      ".Tab:hover": { border: `1px solid ${p.line}` },
      ".Tab--selected": { color: p.ink, border: `1px solid ${p.line}`, boxShadow: "none" },
      ".AccordionItem": { backgroundColor: p.surface, color: p.ink, border: `1px solid ${p.line}`, boxShadow: "none" },
      ".AccordionItem--selected": { border: `1px solid ${p.line}`, boxShadow: "none" },
    },
  };
}

export function appearanceFor(theme: Theme, systemDark: boolean): Appearance {
  const dark = theme === "dark" || (theme === "system" && systemDark);
  return flatAppearance(dark ? DARK_PALETTE : LIGHT_PALETTE);
}

export type StripeGlobal = (publishableKey: string) => StripeInstance;

export type LoadStripeJs = (src: string) => Promise<StripeGlobal>;

export type CardFailure = "offline" | "script" | "sdk" | "unconfigured";

export type CardPlan =
  | { kind: "unavailable"; reason: CardFailure }
  | { kind: "load"; publishableKey: string };

export function cardPlan(publishableKey: string | undefined, offline: boolean): CardPlan {
  const key = publishableKey?.trim() ?? "";
  if (key === "") return { kind: "unavailable", reason: "unconfigured" };
  if (offline) return { kind: "unavailable", reason: "offline" };
  return { kind: "load", publishableKey: key };
}

export type LoadPlan = Extract<CardPlan, { kind: "load" }>;

export interface MountRequest {
  plan: LoadPlan;
  clientSecret: string;
  target: unknown;
  appearance: Appearance;
  // The 3DS redirect returns here, so this URL must carry no order id.
  returnUrl: string;
  loadStripe: LoadStripeJs;
}

export type ConfirmOutcome =
  | { kind: "submitted" }
  | { kind: "error"; message: string };

export type MountResult =
  | {
      kind: "mounted";
      confirm: () => Promise<ConfirmOutcome>;
      destroy: () => void;
    }
  | { kind: "failed"; reason: CardFailure };

export async function mountCard(req: MountRequest): Promise<MountResult> {
  let stripe: StripeGlobal;
  try {
    stripe = await req.loadStripe(STRIPE_JS_URL);
  } catch {
    return { kind: "failed", reason: "script" };
  }
  try {
    const sdk = stripe(req.plan.publishableKey);
    const elements = sdk.elements({ clientSecret: req.clientSecret, appearance: req.appearance });
    const element = elements.create("payment");
    element.mount(req.target);
    let destroyed = false;
    return {
      kind: "mounted",
      confirm: () => confirmWith(sdk, elements, req.returnUrl),
      destroy: () => {
        if (destroyed) return;
        destroyed = true;
        try { element.destroy(); } catch { /* already gone */ }
      },
    };
  } catch {
    return { kind: "failed", reason: "sdk" };
  }
}

// A card is not a redirect-based method, so redirect:"if_required" keeps it in-frame and resolves
// with the PaymentIntent. A confirm that threw did not succeed and must not be treated as success.
async function confirmWith(sdk: StripeInstance, elements: StripeElements, returnUrl: string): Promise<ConfirmOutcome> {
  try {
    const r = await sdk.confirmPayment({ elements, confirmParams: { return_url: returnUrl }, redirect: "if_required" });
    if (r.error) return { kind: "error", message: r.error.message ?? CONFIRM_FAILED };
    const status = r.paymentIntent?.status;
    if (status === "succeeded" || status === "processing") return { kind: "submitted" };
    return { kind: "error", message: CONFIRM_FAILED };
  } catch {
    return { kind: "error", message: CONFIRM_FAILED };
  }
}
