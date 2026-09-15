import type { Theme } from "./domain.js";

// Stripe.js has to come from js.stripe.com and may not be bundled or self-hosted, which is
// why this is a script tag. public/sw.js routes the origin to bypass, so it is never cached.
export const STRIPE_JS_URL = "https://js.stripe.com/dahlia/stripe.js";

// The shell carries the key in a meta element rather than the bundle, so a build is the same
// file whichever account serves it.
const KEY_ELEMENT_ID = "stripe-publishable-key";

// Stripe.js defines this when it loads, and only then, which is why it is optional.
declare global {
  interface Window {
    Stripe?: StripeGlobal;
  }
}

export function publishableKey(): string {
  return document.getElementById(KEY_ELEMENT_ID)?.getAttribute("content")?.trim() ?? "";
}

let loading: Promise<StripeGlobal> | null = null;

// Loaded at most once, and forgotten again on failure so a later attempt can retry. The tag
// is built here rather than through `screens`, which imports this module.
export function loadStripeJs(src: string): Promise<StripeGlobal> {
  if (loading !== null) return loading;
  loading = new Promise<StripeGlobal>((resolve, reject) => {
    // Attributes rather than properties: this is the markup the page's script-src policy is
    // written against, and it is what a reader of the DOM sees.
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
      // the retry appends its own, and a tag that failed will not load later
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

// What confirmPayment resolves with when it does not redirect. A card never redirects (see confirm),
// so this is the normal result: the PaymentIntent with its status, or an error.
export interface PaymentIntentResult {
  paymentIntent?: { status?: string };
  error?: StripeError;
}

// Stripe.js ships a few built-in themes (`stripe`, `night`, `flat`); we build on `flat` and supply
// the palette in `variables`/`rules` so the card form matches the site.
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

// Kept in sync with the tokens in public/styles.css by hand: Stripe.js runs in its own iframe and
// cannot read the page's CSS variables, so the card form's palette is supplied here explicitly.
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

// The flat theme with the site's palette. flat is light-oriented, so dark needs every colour supplied,
// down to the icons and the loading spinner (`iconLoadingIndicatorColor`); anything left unset keeps
// the theme's light default and reads as a light patch on a dark form.
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
      // the fields take the page ground, a step down from the card's surface, so they read as recessed
      ".Input": { backgroundColor: p.bg, color: p.ink, border: `1px solid ${p.line}`, boxShadow: "none" },
      ".Input:focus": { border: `1px solid ${p.accent}`, boxShadow: `0 0 0 1px ${p.accent}` },
      ".Input--invalid": { border: `1px solid ${p.danger}`, boxShadow: "none" },
      ".Input::placeholder": { color: p.muted },
      ".Label": { color: p.ink },
      // the card and its method tab/accordion stay in the neutral line colour; only a focused field
      // takes the accent, so the blue marks where the buyer is typing and nothing else
      ".Tab": { backgroundColor: p.surface, color: p.ink, border: `1px solid ${p.line}`, boxShadow: "none" },
      ".Tab:hover": { border: `1px solid ${p.line}` },
      ".Tab--selected": { color: p.ink, border: `1px solid ${p.line}`, boxShadow: "none" },
      ".AccordionItem": { backgroundColor: p.surface, color: p.ink, border: `1px solid ${p.line}`, boxShadow: "none" },
      ".AccordionItem--selected": { border: `1px solid ${p.line}`, boxShadow: "none" },
    },
  };
}

// The Payment Element does not read the page's theme, so map the site's setting to the matching
// palette. `system` follows the OS; the caller resolves that, since this stays free of the DOM.
export function appearanceFor(theme: Theme, systemDark: boolean): Appearance {
  const dark = theme === "dark" || (theme === "system" && systemDark);
  return flatAppearance(dark ? DARK_PALETTE : LIGHT_PALETTE);
}

export type StripeGlobal = (publishableKey: string) => StripeInstance;

export type LoadStripeJs = (src: string) => Promise<StripeGlobal>;

// "unconfigured": the page carries no publishable key, so it has no card form. In a real deployment
// the key is present whenever the server offers card, so this is a misconfiguration guard.
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
  // Where a 3DS redirect returns; must carry no order id (see cardReturnUrl in main.ts).
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

// A confirm that threw did not succeed. Treating it as success would move the page to the confirming
// screen for a payment nobody attempted. redirect:"if_required" keeps a card in-frame (a card is not
// a redirect-based method), resolving with the PaymentIntent; a declined card carries its reason.
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
