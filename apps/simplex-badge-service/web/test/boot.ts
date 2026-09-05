import { test } from "node:test";
// Installs the globals `src/main.ts` reaches for; it runs on import, so this is called first and once per
// process, and a second boot scenario needs a second test file. `stub-dom.ts`'s caveats apply in full: no
// layout, CSS, accessibility tree, focus, painting or navigation. No assertions: `check-tests.js` skips it.

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
  /** The header the shell gives `main.ts`: the wordmark, and the menu. */
  chrome: StubElement;
  /** `<html>`, which carries `data-theme` when the buyer overrides the system. */
  documentElement: StubElement;
  document: StubDocument;
  history: StubHistory;
  /** Kept in step with `history` the way a browser keeps `location`. */
  location: { pathname: string; search: string; hash: string };
  storage: MemStorage;
  clipboard: Clipboard;
  fetches: Array<{ url: string; init?: RequestInit }>;
  /** Every message `window.confirm` was asked, in order. */
  confirms: string[];
  /** What `navigator.serviceWorker.register` was asked, and when. */
  workers: ServiceWorkers;
  connectivity: Connectivity;
  /** Answers the NEXT request; without one the request holds, as the `?wait=` does. */
  respondWith(reply: Reply): void;
  /** Answers a request that is already holding, which `respondWith` cannot: it is read when the
   * request is made. This is how an answer lands after the buyer has navigated away from the page
   * that asked for it. Returns false when nothing is holding for `match`. */
  answerHeld(reply: Reply, match?: string): boolean;
  confirmAnswer(answer: boolean): void;
  /** `navigator.onLine` flips, `fetch` rejects the way a browser's does with no network, and the
   * window event fires, so the offline state is shown from this rather than simulated. */
  setOffline(on: boolean): void;
  reducedMotion(on: boolean): void;
  /** Dispatches a window event: `popstate`, `resize`. */
  fire(type: string): void;
  /** A key press on the window, which is where the menu's Escape and Tab live. */
  press(key: string, init?: { shiftKey?: boolean }): void;
}

export interface BootOptions {
  /** Seeded before the module runs, so a boot can resume a stored order. */
  storage?: MemStorage;
  /** The URL the page is opened at. Defaults to `/`. */
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
    const event = { key, shiftKey: false, ...init, preventDefault: () => { /* recorded by the handler */ } };
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
        // What a browser throws with no network, and what the backoff and
        // the "the order was not created, and nothing was charged" both see.
        if (offline) throw new TypeError("Failed to fetch");
        const reply = nextResponse;
        nextResponse = null;
        if (reply === null) {
          // Holds, and aborts the way a real request does, the behaviour
          // suspending on `visibilitychange` depends on. A pending promise
          // keeps no timer, so the process still exits.
          return new Promise<Response>((resolve, reject) => {
            const signal = init?.signal;
            // `abort` never fires for a signal that is already aborted, so without this the stub
            // would hold where a real `fetch` rejects, and the test would fail on its timeout
            // rather than say what went wrong.
            if (signal?.aborted) { queueMicrotask(() => { reject(new Error("aborted")); }); return; }
            const entry = { url: String(input), resolve };
            held.push(entry);
            signal?.addEventListener("abort", () => {
              const at = held.indexOf(entry);
              if (at >= 0) held.splice(at, 1);
              // A real `fetch` rejects an aborted request from the microtask drain of the same
              // turn, measured: microtask, then the rejection, then anything queued as a task.
              // Rejecting straight from the listener would land a turn earlier than that.
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

/** One turn of the microtask and immediate queues, which is the unit `settle` counts in. */
export const flush = (): Promise<void> => new Promise((r) => setImmediate(r));

export async function settle(times = 6): Promise<void> {
  for (let i = 0; i < times; i++) await flush();
}


/** Waits for an outcome, not a fixed number of turns: `checkout` awaits `crypto.subtle.digest`, which
 * resolves off the main thread, so a tick count is a race. Bounded, so a regression fails rather than hangs. */
export async function until(condition: () => boolean, what: string, turns = 500): Promise<void> {
  for (let i = 0; i < turns; i++) {
    if (condition()) return;
    await flush();
  }
  throw new Error(`timed out waiting for ${what}`);
}

/** A test with a timeout, so a regression that turns something bounded into something
 * unbounded fails loudly rather than hanging the file: a hung file is dropped whole. */
export function timedTest(ms: number) {
  return (name: string, fn: () => void | Promise<void>): void => {
    test(name, { timeout: ms }, fn);
  };
}

/** The screen that has taken the root, the wizard panel in view, its heading, and the button a
 * buyer would press to move on. Every boot scenario needs these, and each held its own copy. */
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
