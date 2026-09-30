import { test } from "node:test";
// Installs the globals main.ts needs; it runs once per process on import, so a second boot scenario needs its own file.

import {
  MemStorage, StubHistory, installDocument, locationOf,
  type Clipboard, type Connectivity, type ServiceWorkers, type StubDocument, type StubElement,
} from "./stub-dom.js";

export interface Reply {
  status: number;
  body: unknown;
  headers?: Record<string, string>;
}

export interface Page {
  app: StubElement;
  chrome: StubElement;
  documentElement: StubElement;
  document: StubDocument;
  history: StubHistory;
  location: { pathname: string; search: string; hash: string };
  storage: MemStorage;
  clipboard: Clipboard;
  fetches: Array<{ url: string; init?: RequestInit }>;
  confirms: string[];
  workers: ServiceWorkers;
  connectivity: Connectivity;
  respondWith(reply: Reply): void;
  // Answers a request already holding, which respondWith cannot; returns false when nothing matches.
  answerHeld(reply: Reply, match?: string): boolean;
  confirmAnswer(answer: boolean): void;
  setOffline(on: boolean): void;
  reducedMotion(on: boolean): void;
  fire(type: string): void;
  press(key: string, init?: { shiftKey?: boolean }): void;
}

export interface BootOptions {
  storage?: MemStorage;
  url?: string;
}

export function installPage(opts: BootOptions = {}): Page {
  const { app, chrome, documentElement, clipboard, document, workers, connectivity } = installDocument();
  const storage = opts.storage ?? new MemStorage();
  const fetches: Array<{ url: string; init?: RequestInit }> = [];
  const confirms: string[] = [];
  const windowListeners = new Map<string, Array<() => void>>();
  const location = { pathname: "/", search: "", hash: "" };

  let offline = false;
  let nextResponse: Reply | null = null;
  const held: Array<{ url: string; resolve: (r: Response) => void }> = [];
  let answer = true;
  let reduced = false;

  const syncLocation = (): void => { Object.assign(location, locationOf(history.url)); };
  const fire = (type: string): void => {
    syncLocation();
    for (const fn of [...(windowListeners.get(type) ?? [])]) fn();
  };
  const press = (key: string, init: { shiftKey?: boolean } = {}): void => {
    const event = { key, shiftKey: false, ...init, preventDefault: () => {} };
    for (const fn of [...(windowListeners.get("keydown") ?? [])]) (fn as (e: unknown) => void)(event);
  };
  const history = new StubHistory(() => { fire("popstate"); }, () => { syncLocation(); });
  if (opts.url !== undefined) {
    history.replaceState(null, "", opts.url);
  }
  syncLocation();

  Object.defineProperty(globalThis, "history", { configurable: true, value: history });
  Object.defineProperty(globalThis, "location", { configurable: true, value: location });
  Object.defineProperty(globalThis, "window", {
    configurable: true,
    value: {
      localStorage: storage,
      confirm: (message: string) => { confirms.push(message); return answer; },
      matchMedia: (query: string) => ({
        media: query,
        matches: query === "(prefers-reduced-motion: reduce)" && reduced,
      }),
      addEventListener(type: string, fn: () => void) {
        const list = windowListeners.get(type) ?? [];
        list.push(fn);
        windowListeners.set(type, list);
      },
      fetch: async (input: unknown, init?: RequestInit): Promise<Response> => {
        fetches.push(init ? { url: String(input), init } : { url: String(input) });
        // A browser with no network throws this TypeError.
        if (offline) throw new TypeError("Failed to fetch");
        const reply = nextResponse;
        nextResponse = null;
        if (reply === null) {
          // A pending promise with no timer lets the process still exit while the request holds.
          return new Promise<Response>((resolve, reject) => {
            const signal = init?.signal;
            // abort never fires for an already-aborted signal, so reject explicitly or the stub holds where fetch would reject.
            if (signal?.aborted) { queueMicrotask(() => { reject(new Error("aborted")); }); return; }
            const entry = { url: String(input), resolve };
            held.push(entry);
            signal?.addEventListener("abort", () => {
              const at = held.indexOf(entry);
              if (at >= 0) held.splice(at, 1);
              // A real fetch rejects an aborted request on the microtask drain of the same turn.
              queueMicrotask(() => { reject(new Error("aborted")); });
            }, { once: true });
          });
        }
        return responseOf(reply);
      },
    },
  });

  function responseOf(reply: Reply): Response {
    return {
      ok: reply.status < 400,
      status: reply.status,
      headers: {
        get: (name: string) => {
          const map = reply.headers ?? {};
          const key = Object.keys(map).find((k) => k.toLowerCase() === name.toLowerCase());
          return key === undefined ? null : map[key]!;
        },
      },
      json: async () => reply.body,
      text: async () => JSON.stringify(reply.body),
    } as unknown as Response;
  }

  return {
    app, chrome, documentElement, document, history, location, storage, clipboard, fetches, confirms, workers, connectivity,
    respondWith: (reply) => { nextResponse = reply; },
    answerHeld: (reply, match) => {
      const at = match === undefined ? 0 : held.findIndex((h) => h.url.includes(match));
      if (at < 0 || held.length === 0) return false;
      const [entry] = held.splice(at, 1);
      entry!.resolve(responseOf(reply));
      return true;
    },
    confirmAnswer: (value) => { answer = value; },
    setOffline: (on) => {
      offline = on;
      connectivity.online = !on;
      fire(on ? "offline" : "online");
    },
    reducedMotion: (on) => { reduced = on; },
    fire,
    press,
  };
}

export const flush = (): Promise<void> => new Promise((r) => setImmediate(r));

export async function settle(times = 6): Promise<void> {
  for (let i = 0; i < times; i++) await flush();
}


// Waits for an outcome rather than a fixed number of turns, since crypto.subtle.digest resolves off the main thread and a tick count would race.
export async function until(condition: () => boolean, what: string, turns = 500): Promise<void> {
  for (let i = 0; i < turns; i++) {
    if (condition()) return;
    await flush();
  }
  throw new Error(`timed out waiting for ${what}`);
}

export function timedTest(ms: number) {
  return (name: string, fn: () => void | Promise<void>): void => {
    test(name, { timeout: ms }, fn);
  };
}

export function screenOf(app: StubElement): StubElement { return app.all("section.panel")[0]!; }

export function inViewOf(app: StubElement): StubElement {
  const found = app.all("section.panel").find((p) => !p.hasAttribute("inert"));
  if (found === undefined) throw new Error("exactly one panel must be in view");
  return found;
}

export function headingOf(p: StubElement): string { return p.all("h1")[0]?.textContent ?? ""; }

export function primaryOf(p: StubElement): StubElement | undefined {
  return p.all("button.primary").find((b) => !b.hasAttribute("disabled"));
}

const FORGET_LABEL = "Forget everything on this device";

export function forgetControl(page: Page): StubElement | undefined {
  page.chrome.all("button.menu-button")[0]!.click();
  page.chrome.all("button.menu-item").find((b) => b.textContent === "Your codes")!.click();
  return page.app.all("button.danger").find((b) => b.textContent === FORGET_LABEL);
}
