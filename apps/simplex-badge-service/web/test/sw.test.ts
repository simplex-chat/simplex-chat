import { timedTest } from "./boot.js";
import assert from "node:assert/strict";
import { readFileSync, readdirSync } from "node:fs";
import vm from "node:vm";

const swTest = timedTest(2000);
// the shell's own deadline is 3s, so the one test that waits it out needs more than that
const stalledTest = timedTest(8000);

const ORIGIN = "https://badges.simplex.chat";
const WORKER_SOURCE = readFileSync(new URL("../../public/sw.js", import.meta.url), "utf8");
const SHELL_HTML = readFileSync(new URL("../../public/index.html", import.meta.url), "utf8");

/** Anubis serves its challenge as HTML at the page's own path, which is the whole problem. */
const ANUBIS_CHALLENGE = `<!doctype html><html><head><title>Making sure you are not a bot</title></head>
<body><script id="anubis_challenge" type="application/json">{"rules":{"difficulty":4}}</script>
<script async type="module" src="/.within.website/x/cmd/anubis/static/js/main.mjs"></script></body></html>`;

// ------------------------------------------------------------- the fake world

interface FakeResponse {
  url: string;
  ok: boolean;
  status: number;
  /** The bytes, readable by a test without disturbing the body. */
  body: string;
  bodyUsed: boolean;
  clone(): FakeResponse;
  text(): Promise<string>;
}

/** A single-use body, the property the worker's `clone()` exists for: `text()` disturbs it, a disturbed body
 * cannot be cloned, and `Cache.put` rejects one (`TypeError: Response body is already used`). A re-readable
 * stand-in would let `shell.text()` then `cache.put(shell)` pass here and fail on every browser. */
function response(url: string, status: number, body: string): FakeResponse {
  let used = false;
  return {
    url, status, body,
    ok: status >= 200 && status < 300,
    get bodyUsed() { return used; },
    clone() {
      if (used) throw new TypeError("Response body is already used");
      return response(url, status, body);
    },
    async text() {
      if (used) throw new TypeError("Response body is already used");
      used = true;
      return body;
    },
  };
}

/** The Cache API's key: an absolute URL, query string included. */
function keyOf(key: unknown): string {
  const raw = typeof key === "string" ? key : String((key as { url: string }).url);
  return new URL(raw, `${ORIGIN}/`).href;
}

/** A bare URL is a GET, as `new Request(url)` is. */
function methodOf(key: unknown): string {
  return typeof key === "string" ? "GET" : String((key as { method?: string }).method ?? "GET");
}

class FakeCache {
  readonly entries = new Map<string, FakeResponse>();
  /** Every key this cache was asked for, and every key written into it. */
  readonly matched: string[] = [];
  readonly written: string[] = [];

  constructor(private readonly net: FakeNet) {}

  async match(key: unknown): Promise<FakeResponse | undefined> {
    this.matched.push(keyOf(key));
    // The Cache API matches GET and nothing else: a request with any other
    // method never has a cached answer, whatever is stored under its URL.
    if (methodOf(key) !== "GET") return undefined;
    return this.entries.get(keyOf(key));
  }

  async put(key: unknown, res: FakeResponse): Promise<void> {
    if (res.bodyUsed) throw new TypeError("Failed to execute 'put' on 'Cache': Response body is already used");
    this.written.push(keyOf(key));
    this.entries.set(keyOf(key), res);
  }

  /** Atomic: one non-2xx and nothing at all is added. */
  async addAll(keys: readonly string[]): Promise<void> {
    const got = await Promise.all(keys.map(async (k) => [k, await this.net.fetch(k)] as const));
    const bad = got.find(([, res]) => !res.ok);
    if (bad) throw new Error(`addAll: ${bad[0]} answered ${bad[1].status}`);
    for (const [k, res] of got) await this.put(k, res);
  }

  async keys(): Promise<string[]> { return [...this.entries.keys()]; }
}

class FakeCacheStorage {
  readonly stores = new Map<string, FakeCache>();
  readonly opened: string[] = [];
  readonly deleted: string[] = [];

  constructor(private readonly net: FakeNet) {}

  async open(name: string): Promise<FakeCache> {
    this.opened.push(name);
    const existing = this.stores.get(name);
    if (existing !== undefined) return existing;
    const made = new FakeCache(this.net);
    this.stores.set(name, made);
    return made;
  }

  async keys(): Promise<string[]> { return [...this.stores.keys()]; }

  async delete(name: string): Promise<boolean> {
    this.deleted.push(name);
    return this.stores.delete(name);
  }
}

class FakeNet {
  readonly requests: Array<{ url: string; init?: Record<string, unknown> }> = [];
  /** Answers, by pathname-with-query. Anything unlisted is a 404. */
  readonly routes = new Map<string, { status: number; body: string }>();
  offline = false;

  /** Paths whose connection is accepted and never answered, as a captive portal does. */
  readonly stalled = new Set<string>();

  serve(path: string, status: number, body: string): void {
    this.routes.set(path, { status, body });
  }

  stall(path: string): void {
    this.stalled.add(path);
  }

  readonly fetch = async (input: unknown, init?: Record<string, unknown>): Promise<FakeResponse> => {
    const url = keyOf(input);
    this.requests.push(init ? { url, init } : { url });
    if (this.offline) throw new TypeError("Failed to fetch");
    const path = url.slice(ORIGIN.length);
    if (this.stalled.has(path)) return new Promise<FakeResponse>(() => { /* never answers */ });
    const hit = this.routes.get(path);
    return hit === undefined
      ? response(url, 404, "not found")
      : response(url, hit.status, hit.body);
  };

  get paths(): string[] { return this.requests.map((r) => r.url.slice(ORIGIN.length)); }
}

interface WorkerExports {
  BUILD: string;
  CACHE: string;
  ASSETS: string;
  SHELL: string;
  ENTRY: string;
  PRECACHE: string[];
  strategyFor(url: URL): string;
}

interface Rig {
  sw: WorkerExports;
  caches: FakeCacheStorage;
  net: FakeNet;
  /** Neither may be called: a new build activates on the next full load. */
  calls: { skipWaiting: number; claim: number };
  install(): Promise<void>;
  activate(): Promise<void>;
  request(url: string, method?: string): Promise<Served>;
}

interface Served {
  /** Whether the worker took the request over at all. */
  responded: boolean;
  body?: string;
  status?: number;
}

/** Loads `public/sw.js` into a fresh context, so the worker's own top-level state is re-evaluated and one
 * test's caches cannot leak into another's. */
interface TimerSpy {
  set: (id: number) => void;
  clear: (id: number) => void;
}

function rig(options: { shell?: string; shellStatus?: number; missing?: readonly string[]; timers?: TimerSpy } = {}): Rig {
  const net = new FakeNet();
  const caches = new FakeCacheStorage(net);
  const calls = { skipWaiting: 0, claim: 0 };
  const listeners = new Map<string, Array<(event: unknown) => void>>();

  const self = {
    location: new URL(`${ORIGIN}/`),
    addEventListener(type: string, fn: (event: unknown) => void) {
      const list = listeners.get(type) ?? [];
      list.push(fn);
      listeners.set(type, list);
    },
    skipWaiting: () => { calls.skipWaiting++; },
    clients: { claim: async () => { calls.claim++; } },
  } as Record<string, unknown>;

  // a worker has timers; the shell fetch races one against a network that stalls
  const spy = options.timers;
  const watchedSetTimeout = (fn: () => void, ms: number): NodeJS.Timeout => {
    const handle = setTimeout(fn, ms);
    spy?.set(Number(handle));
    return handle;
  };
  const watchedClearTimeout = (handle: NodeJS.Timeout): void => {
    spy?.clear(Number(handle));
    clearTimeout(handle);
  };
  const sandbox: Record<string, unknown> = {
    self, caches, fetch: net.fetch, URL, console,
    setTimeout: watchedSetTimeout, clearTimeout: watchedClearTimeout,
  };
  sandbox.globalThis = sandbox;
  vm.createContext(sandbox);
  vm.runInContext(WORKER_SOURCE, sandbox, { filename: "public/sw.js" });
  const sw = self.sw as WorkerExports;

  // The shell, and the build's assets, as the service would serve them (Anubis's
  // `static_dir`). `missing` takes one back out, which is a deploy half done.
  net.serve("/", options.shellStatus ?? 200, options.shell ?? SHELL_HTML);
  for (const url of sw.PRECACHE) {
    if (options.missing?.includes(url) === true) continue;
    net.serve(url, 200, `/* ${url} */`);
  }

  async function fire(type: string, event: Record<string, unknown>): Promise<void> {
    const waited: Array<Promise<unknown>> = [];
    const withWait = { ...event, waitUntil: (p: Promise<unknown>) => { waited.push(p); } };
    for (const fn of listeners.get(type) ?? []) fn(withWait);
    await Promise.all(waited);
  }

  return {
    sw, caches, net, calls,
    install: () => fire("install", {}),
    activate: () => fire("activate", {}),
    request: async (url, method = "GET") => {
      let answer: Promise<FakeResponse> | undefined;
      const request = { url: keyOf(url), method };
      for (const fn of listeners.get("fetch") ?? []) {
        fn({ request, respondWith: (p: Promise<FakeResponse>) => { answer = p; } });
      }
      if (answer === undefined) return { responded: false };
      const res = await answer;
      return { responded: true, body: res.body, status: res.status };
    },
  };
}

// ------------------------------------------------------------- the build hash

swTest("sw: the shell and the worker name the same build", () => {
  const r = rig();
  assert.match(r.sw.BUILD, /^[0-9a-f]{16}$/, "the build hash is content-derived by build.js");
  assert.ok(SHELL_HTML.includes(`src="${r.sw.ENTRY}"`),
    "index.html must load THIS build's entry module, or a cache-first shell would import another build's");
  assert.ok(SHELL_HTML.includes(`${r.sw.ASSETS}styles.css`));
  const hashes = new Set([...SHELL_HTML.matchAll(/\/assets\/([0-9a-f]{16})\//g)].map((m) => m[1]));
  assert.deepEqual([...hashes], [r.sw.BUILD], "the shell names exactly one build, and it is this one");
});

swTest("sw: the precache is explicit URLs, every compiled module, all under one hash", () => {
  const r = rig();
  const modules = readdirSync(new URL("../../src", import.meta.url))
    .filter((f) => f.endsWith(".ts"))
    .map((f) => `${r.sw.ASSETS}${f.replace(/\.ts$/, ".js")}`);
  // the landing screen's hero counts, and so does the header's wordmark and the shell's favicon:
  // the landing screen is meant to work offline, and an image the worker never stored
  // is a blank rectangle on the first screen of the page.
  const images = readdirSync(new URL("../../public/img", import.meta.url))
    .filter((f) => f.endsWith(".png") || f.endsWith(".svg")).map((f) => `${r.sw.ASSETS}${f}`);
  assert.ok(images.length > 0, "the hero has to be somewhere for the worker to precache");
  const expected = [`${r.sw.ASSETS}styles.css`, ...modules, ...images].sort();
  assert.deepEqual([...r.sw.PRECACHE].sort(), expected,
    "a module missing here is a page that half-works offline; one under another hash is a skew");
  assert.ok(!r.sw.PRECACHE.includes("/"),
    "`/` is fetched and checked at install, never added blind with the rest");
});

// --------------------------------------------------------------- the routing

swTest("sw: /api/* is network-only, on every form the two endpoints take", () => {
  const { strategyFor } = rig().sw;
  for (const path of [
    "/api", "/api/invoice", "/api/invoice/inv_9f3a",
    "/api/invoice/inv_9f3a?wait=open", "/api/invoice/inv_9f3a?wait=expired", "/api/anything/else",
  ]) {
    assert.equal(strategyFor(new URL(path, ORIGIN)), "network-only",
      `${path} must never be cached: a stale paid shows a code for an order that later failed`);
  }
});

swTest("sw: / is the shell whatever query or hash it carries", () => {
  const { strategyFor } = rig().sw;
  for (const path of ["/", "/index.html", "/?order=inv_9f3a", "/#/codes", "/?order=inv_9f3a#/x"]) {
    assert.equal(strategyFor(new URL(path, ORIGIN)), "shell", path);
  }
});

swTest("sw: only THIS build's assets are cache-first", () => {
  const r = rig();
  assert.equal(r.sw.strategyFor(new URL(`${r.sw.ASSETS}main.js`, ORIGIN)), "asset");
  assert.equal(r.sw.strategyFor(new URL(`${r.sw.ASSETS}styles.css`, ORIGIN)), "asset");
  assert.equal(r.sw.strategyFor(new URL("/assets/0000000000000000/main.js", ORIGIN)), "bypass",
    "another build's URL is not ours to answer: its cache is gone");
});

swTest("sw: js.stripe.com is never this worker's business", () => {
  const { strategyFor } = rig().sw;
  for (const url of [
    "https://js.stripe.com/v3/", "https://js.stripe.com/basil/stripe.js",
    "https://api.stripe.com/v1/payment_intents", "https://hooks.stripe.com/3d_secure",
  ]) {
    assert.equal(strategyFor(new URL(url)), "bypass", `${url} — Stripe forbids caching or self-hosting it`);
  }
});

swTest("sw: anything else same-origin is left to the browser", () => {
  const { strategyFor } = rig().sw;
  for (const path of ["/sw.js", "/favicon.ico", "/webhooks/stripe", "/styles.css", "/src/main.js"]) {
    assert.equal(strategyFor(new URL(path, ORIGIN)), "bypass", path);
  }
});

// --------------------------------------------------------------- installation

swTest("sw: install precaches the shell and this build's assets, and nothing else", async () => {
  const r = rig();
  await r.install();
  const cache = r.caches.stores.get(r.sw.CACHE)!;
  assert.deepEqual([...(await cache.keys())].sort(),
    [`${ORIGIN}/`, ...r.sw.PRECACHE.map((u) => `${ORIGIN}${u}`)].sort());
  assert.deepEqual([...r.caches.stores.keys()], [r.sw.CACHE], "one cache, named for the build");
  const shellRequest = r.net.requests.find((q) => q.url === `${ORIGIN}/`)!;
  assert.equal(shellRequest.init?.cache, "reload",
    "the shell is re-read rather than taken from the HTTP cache it may already be stale in");
});

swTest("sw: install refuses a document that is not this build's shell", async () => {
  const r = rig({ shell: ANUBIS_CHALLENGE });
  await assert.rejects(r.install(), /not this build's shell/);
  assert.equal(r.caches.stores.size, 0, "nothing survives: a challenge page enshrined as the shell is permanent");
  assert.ok(r.caches.deleted.includes(r.sw.CACHE));
});

swTest("sw: and a challenge page reaches no cache even for a moment", async () => {
  const r = rig({ shell: ANUBIS_CHALLENGE });
  await r.install().catch(() => { /* asserted above */ });
  for (const store of r.caches.stores.values()) {
    for (const res of store.entries.values()) {
      assert.ok(!res.body.includes("anubis_challenge"), "the challenge is in a cache");
    }
  }
});

swTest("sw: install fails, and leaves nothing, when the shell does not load", async () => {
  for (const status of [403, 503]) {
    const r = rig({ shellStatus: status, shell: "" });
    await assert.rejects(r.install(), new RegExp(`answered ${status}`));
    assert.equal(r.caches.stores.size, 0);
  }
});

swTest("sw: a precache that cannot be completed leaves no half-installed cache", async () => {
  const r = rig({ missing: [`${rig().sw.ASSETS}flow.js`] });
  await assert.rejects(r.install(), /flow\.js answered 404/);
  assert.equal(r.caches.stores.size, 0,
    "a half-filled cache would serve a shell whose modules 404 for as long as the build lived");
});

swTest("sw: the shell is cloned before it is read, or no install could ever store it", async () => {
  const r = rig();
  await r.install();
  const cache = r.caches.stores.get(r.sw.CACHE)!;
  const stored = (await cache.match("/"))!;
  assert.equal(stored.body, SHELL_HTML);
  assert.equal(stored.bodyUsed, false,
    "what was put in the cache is the undisturbed response: reading it to check the marker must use a clone");
});

// the atomicity has two directions, and only one of them was covered. `sw.js`
// cannot be part of its own hash, so editing the worker alone re-runs `install`
// against the cache the ACTIVE build is being served from.

swTest("sw: a failed RE-INSTALL leaves the shipped build's cache untouched", async () => {
  const r = rig();
  await r.install();
  const before = [...(await r.caches.stores.get(r.sw.CACHE)!.keys())].sort();
  // A background update check that meets the challenge, a 5xx, or nothing.
  r.net.serve("/", 200, ANUBIS_CHALLENGE);
  await assert.rejects(r.install(), /not this build's shell/);
  const cache = r.caches.stores.get(r.sw.CACHE);
  assert.ok(cache !== undefined,
    "deleting this would take offline support from the build that is working");
  assert.deepEqual([...(await cache.keys())].sort(), before, "and every entry is still in it");
  assert.ok(!r.caches.deleted.includes(r.sw.CACHE));
});

swTest("sw: a re-install with no network at all leaves it alone too", async () => {
  const r = rig();
  await r.install();
  r.net.offline = true;
  await assert.rejects(r.install(), TypeError);
  // Read back through the storage, not through a cache object captured before:
  // deleting a cache the worker holds a handle to is exactly the bug.
  const cache = r.caches.stores.get(r.sw.CACHE);
  assert.ok(cache !== undefined, "the buyer on a train keeps their page");
  assert.equal((await cache.match("/"))?.body, SHELL_HTML);
});

swTest("sw: a FIRST install that fails still leaves nothing behind", async () => {
  const r = rig({ shell: ANUBIS_CHALLENGE });
  await assert.rejects(r.install(), /not this build's shell/);
  assert.equal(r.caches.stores.size, 0, "there was no build here to protect");
});

swTest("sw: install skipWaits, so a redeploy does not wait for every tab to close", async () => {
  const r = rig();
  await r.install();
  assert.equal(r.calls.skipWaiting, 1,
    "without this the new worker sits in `waiting` and the old build is served indefinitely");
});

swTest("sw: an install that fails does not skipWaiting over a half-filled cache", async () => {
  const r = rig({ shellStatus: 500 });
  await assert.rejects(() => r.install());
  assert.equal(r.calls.skipWaiting, 0,
    "taking over with no cache would serve a shell whose modules are not there");
});

// ---------------------------------------------------------------- activation

swTest("sw: activation deletes every cache whose hash is not this build", async () => {
  const r = rig();
  await r.caches.open("sxb-0000000000000000");
  await r.caches.open("sxb-1111111111111111");
  await r.install();
  await r.activate();
  assert.deepEqual([...r.caches.stores.keys()], [r.sw.CACHE], "the old builds are gone");
  assert.deepEqual(r.caches.deleted.sort(), ["sxb-0000000000000000", "sxb-1111111111111111"]);
});

swTest("sw: activation keeps this build's cache, which is what offline is made of", async () => {
  const r = rig();
  await r.install();
  await r.activate();
  const cache = r.caches.stores.get(r.sw.CACHE)!;
  assert.ok((await cache.keys()).length > 0);
  assert.ok(!r.caches.deleted.includes(r.sw.CACHE));
});

swTest("sw: activation does not claim the open pages either", async () => {
  const r = rig();
  await r.install();
  await r.activate();
  assert.equal(r.calls.claim, 0, "a page keeps the build it loaded with until it is loaded again");
});

// --------------------------------------------------------------------- fetch

swTest("sw: an API request is not answered here, and never touches the Cache API", async () => {
  const r = rig();
  await r.install();
  const cache = r.caches.stores.get(r.sw.CACHE)!;
  const openedBefore = r.caches.opened.length;
  cache.matched.length = 0;
  cache.written.length = 0;

  for (const [url, method] of [
    [`${ORIGIN}/api/invoice`, "POST"],
    [`${ORIGIN}/api/invoice/inv_9f3a`, "GET"],
    [`${ORIGIN}/api/invoice/inv_9f3a?wait=open`, "GET"],
  ] as const) {
    const served = await r.request(url, method);
    assert.equal(served.responded, false, `${method} ${url} must go to the network as if no worker existed`);
  }
  assert.deepEqual(cache.matched, [], "not even a read: a cached paid would show a code for an order that failed");
  assert.deepEqual(cache.written, [], "and nothing about money is ever put into the Cache API");
  assert.equal(r.caches.opened.length, openedBefore, "no cache is so much as opened for /api/*");
});

swTest("sw: a page load is answered from the cache when the network is gone", async () => {
  const r = rig();
  await r.install();
  r.net.offline = true;
  for (const url of ["/", "/?order=inv_9f3a", "/?order=inv_9f3a#/x", "/index.html"]) {
    const served = await r.request(`${ORIGIN}${url}`);
    assert.equal(served.responded, true, url);
    assert.equal(served.body, SHELL_HTML, `${url} — ?order= is a page, not a key`);
  }
});

// The failure this exists to prevent: a redeploy that a returning buyer never sees. The
// new worker waits for every tab holding the old one to close, and a reload does not
// release it, so a cache-first shell kept naming the old build's modules indefinitely,
// which shipped a checkout generating codes the service could not redeem.
swTest("sw: a redeployed shell reaches the page on a reload, with the old worker still active", async () => {
  const r = rig();
  await r.install();
  const REDEPLOYED = SHELL_HTML.replace("main.js", "main.js?v=next");
  r.net.serve("/", 200, REDEPLOYED);
  for (const url of ["/", "/?order=inv_9f3a", "/index.html"]) {
    const served = await r.request(`${ORIGIN}${url}`);
    assert.equal(served.responded, true, url);
    assert.equal(served.body, REDEPLOYED, `${url} kept serving the cached shell after a redeploy`);
  }
  // and the fresh document is passed through, never stored: the challenge page must not
  // be able to enter the cache this way either
  const cache = r.caches.stores.get(r.sw.CACHE)!;
  assert.deepEqual(cache.written.filter((w) => w === "/"), []);
});

swTest("sw: the shell's deadline is cleared on every path, so no request leaves a timer armed", async () => {
  // the offline path throws past anything after the await, which is where the clear used to be
  const armed: number[] = [];
  const r = rig({ timers: { set: (id) => armed.push(id), clear: (id) => { armed.splice(armed.indexOf(id), 1); } } });
  await r.install();
  r.net.offline = true;
  await r.request(`${ORIGIN}/`);
  assert.deepEqual(armed, [], "the offline path left a timer running");
  r.net.offline = false;
  await r.request(`${ORIGIN}/`);
  assert.deepEqual(armed, [], "and so did the answered path");
});

stalledTest("sw: a network that accepts and never answers falls back rather than hanging the page", async () => {
  // The tunnel case the precache exists for: `fetch` does not reject on a stalled connection,
  // and `navigator.onLine` still says online, so only a deadline gets the buyer their page.
  const r = rig();
  await r.install();
  r.net.stall("/");
  const started = Date.now();
  const served = await r.request(`${ORIGIN}/`);
  assert.equal(served.responded, true);
  assert.equal(served.body, SHELL_HTML, "the precached shell is what a stalled network is for");
  assert.ok(Date.now() - started < 10_000, "and it must not wait out the browser's own timeout");
});

swTest("sw: a shell the listener cannot serve falls back to the cache, not to an error", async () => {
  const r = rig();
  await r.install();
  r.net.serve("/", 503, "the listener is restarting");
  const served = await r.request(`${ORIGIN}/`);
  assert.equal(served.responded, true);
  assert.equal(served.body, SHELL_HTML, "a 5xx must not take the page away from a buyer");
});

swTest("sw: the shell and every module are served with the network gone", async () => {
  const r = rig();
  await r.install();
  r.net.offline = true;
  const before = r.net.requests.length;
  for (const url of r.sw.PRECACHE) {
    const served = await r.request(`${ORIGIN}${url}`);
    assert.equal(served.responded, true, url);
    assert.equal(served.body, `/* ${url} */`, url);
  }
  assert.equal(r.net.requests.length, before, "an asset is cache-first: offline is the same path, not a special case");
});

swTest("sw: an asset this build never precached falls through to the network, uncached", async () => {
  const r = rig();
  await r.install();
  const cache = r.caches.stores.get(r.sw.CACHE)!;
  cache.written.length = 0;
  r.net.serve(`${r.sw.ASSETS}later.js`, 200, "/* a module added after this worker shipped */");
  const served = await r.request(`${ORIGIN}${r.sw.ASSETS}later.js`);
  assert.equal(served.responded, true);
  assert.equal(served.body, "/* a module added after this worker shipped */");
  assert.deepEqual(cache.written, [], "this worker caches one build at install and writes nothing at request time");
});

swTest("sw: nothing whatsoever is written to the Cache API after install", async () => {
  const r = rig();
  await r.install();
  const cache = r.caches.stores.get(r.sw.CACHE)!;
  cache.written.length = 0;
  await r.request(`${ORIGIN}/`);
  await r.request(`${ORIGIN}/?order=inv_9f3a`);
  await r.request(`${ORIGIN}${r.sw.ASSETS}main.js`);
  await r.request(`${ORIGIN}/api/invoice/inv_9f3a`);
  await r.request(`${ORIGIN}/api/invoice`, "POST");
  await r.request("https://js.stripe.com/v3/");
  await r.request(`${ORIGIN}/favicon.ico`);
  assert.deepEqual(cache.written, []);
});

swTest("sw: a request that is not a GET is never answered from a cache", async () => {
  const r = rig();
  await r.install();
  const cache = r.caches.stores.get(r.sw.CACHE)!;
  cache.written.length = 0;
  // Both of these have a precached entry under their exact URL, so what stops
  // them is the method and nothing else.
  for (const url of ["/", `${r.sw.ASSETS}main.js`]) {
    for (const method of ["POST", "HEAD"]) {
      const served = await r.request(`${ORIGIN}${url}`, method);
      assert.equal(served.responded, false, `${method} ${url} must reach the service`);
    }
  }
  assert.deepEqual(cache.written, [], "and none of them wrote anything either");
});

swTest("sw: Stripe.js is fetched by the page, never by this worker", async () => {
  const r = rig();
  await r.install();
  const before = r.net.requests.length;
  const served = await r.request("https://js.stripe.com/v3/");
  assert.equal(served.responded, false, "another origin is answered as if no worker existed");
  assert.equal(r.net.requests.length, before, "and the worker does not fetch it itself");
});
