// A classic worker script, so its export is the worker's own global: `self`. `test/sw.test.ts`
// runs this exact file in a Node `vm` with a fake Cache API and reads `self.sw`.

/** Rewritten by `build.js` from the bytes of the compiled modules, the stylesheet and the images. */
const BUILD = "b36892a45dde8359";
const ASSETS = `/assets/${BUILD}/`;
/** One cache per build, so eviction is "everything that is not this one". */
const CACHE = `sxb-${BUILD}`;
const SHELL = "/";
// past this the cached shell is served instead; an answer that arrives later is dropped
const SHELL_TIMEOUT_MS = 3000;
/** Proves a document really is this build's shell and not an interstitial. */
const ENTRY = `${ASSETS}main.js`;

// Explicit URLs under one build hash, so a shell and the modules it imports can never skew.
// `/` is not here: it is fetched and checked separately, being the one response an
// interstitial can substitute. The images are precached because the stylesheet asks for them
// by relative URL, which puts them under this hash too.
const PRECACHE = [
  `${ASSETS}styles.css`,
  `${ASSETS}hero-dark.png`,
  `${ASSETS}hero-light.png`,
  `${ASSETS}wordmark-dark.svg`,
  `${ASSETS}wordmark-light.svg`,
  `${ASSETS}symbol-dark.svg`,
  `${ASSETS}symbol-light.svg`,
  `${ASSETS}api.js`,
  `${ASSETS}order.js`,
  `${ASSETS}format.js`,
  `${ASSETS}domain.js`,
  `${ASSETS}catalog.js`,
  `${ASSETS}codes.js`,
  `${ASSETS}flow.js`,
  `${ASSETS}icons.js`,
  `${ASSETS}main.js`,
  `${ASSETS}parse.js`,
  `${ASSETS}qr.js`,
  `${ASSETS}routing.js`,
  `${ASSETS}screens.js`,
  `${ASSETS}store.js`,
  // our card module, not Stripe's script, which is another origin and never cached
  `${ASSETS}stripe.js`,
];

/** The whole routing decision, as a pure function of the URL. */
function strategyFor(url) {
  if (url.origin !== self.location.origin) return "bypass";
  // Before anything that could reach a cache. A cached `paid` would show a code for an order
  // that later failed, and a cached `open` would hide a settled one; `Cache-Control: no-store`
  // covers the HTTP cache, and this line covers the Cache API.
  if (url.pathname === "/api" || url.pathname.startsWith("/api/")) return "network-only";
  if (url.pathname === SHELL || url.pathname === "/index.html") return "shell";
  if (url.pathname.startsWith(ASSETS)) return "asset";
  return "bypass";
}

/** Leaves the cache either complete or absent: a half-filled one serves a shell whose modules
 * 404 offline, for as long as that build lives. */
async function precache() {
  // This file is not part of its own hash, so a change to it alone re-installs against the
  // same cache name and `caches.open` hands back the one the active worker is serving from.
  // What this install created is all it may destroy.
  const existed = (await caches.keys()).includes(CACHE);
  const cache = await caches.open(CACHE);
  try {
    // Anubis serves its challenge as HTML at the page's own path, so the shell is verified
    // before it is stored. `reload` so the answer is not one the HTTP cache had lying about.
    const shell = await fetch(SHELL, { cache: "reload", credentials: "same-origin" });
    if (!shell.ok) throw new Error(`sw: the shell answered ${shell.status}`);
    const html = await shell.clone().text();
    if (!html.includes(ENTRY)) throw new Error("sw: refusing to cache a document that is not this build's shell");
    await cache.put(SHELL, shell);
    await cache.addAll(PRECACHE);
  } catch (e) {
    // A failed re-install must leave the shipped build alone: offline support is what a buyer
    // on a train has left, and an update check meeting a 5xx is no reason to take it away.
    if (!existed) await caches.delete(CACHE);
    throw e;
  }
}

self.addEventListener("install", (event) => {
  // After the precache, never before: a worker that took over with a half-filled cache would
  // serve a shell whose modules are not there. Without `skipWaiting` at all, a new build only
  // activated once every tab holding the old worker had closed, and a reload does not release
  // one, so plain reloads kept getting the old build until a hard reload.
  //
  // `clients.claim()` stays out: it would swap the worker under a page whose modules are
  // already loaded, which is the skew this whole file exists to prevent.
  event.waitUntil(precache().then(() => self.skipWaiting()));
});

self.addEventListener("activate", (event) => {
  // Safe even with an old page still open: every asset URL carries its build hash, so a
  // page whose cache has gone falls through to the network rather than to another build's file.
  event.waitUntil(caches.keys().then((names) => Promise.all(
    names.filter((name) => name !== CACHE).map((name) => caches.delete(name)),
  )));
});

/** Cache-first, safe because an asset URL carries the build hash, so an entry can never be
 * another build's file. Nothing is written here: this worker caches one build, once. */
async function fromPrecache(request) {
  const cache = await caches.open(CACHE);
  const hit = await cache.match(request);
  return hit !== undefined && hit !== null ? hit : fetch(request);
}

/** Network-first, and the one response that must be: the shell is the only file whose URL
 * carries no build hash, so it decides which build a page runs. Served cache-first, a redeploy
 * could not reach a returning buyer at all, which once shipped a checkout generating codes
 * the service could not redeem. */
async function shellFirst(request) {
  let deadline;
  try {
    // SHELL rather than the request: every page URL is the same document, so one fetch serves
    // them all and it is the key the fallback is stored under. `reload` because the HTTP cache
    // can pin a build just as the Cache API can.
    const network = fetch(SHELL, { cache: "reload", credentials: "same-origin" });
    // a captive portal or a tunnel accepts the connection and never answers, and `fetch` does
    // not reject on that: without a deadline the page is blank for the browser's own timeout
    const stall = new Promise((resolve) => { deadline = setTimeout(() => { resolve(undefined); }, SHELL_TIMEOUT_MS); });
    const fresh = await Promise.race([network, stall]);
    if (fresh !== undefined && fresh.ok) return fresh;
  } catch {
    // offline, or the listener is down: the precached shell is what is left
  } finally {
    // the offline path throws past a clear that followed the await, and left the worker awake
    clearTimeout(deadline);
  }
  const cache = await caches.open(CACHE);
  const hit = await cache.match(SHELL);
  return hit !== undefined && hit !== null ? hit : fetch(request);
}

self.addEventListener("fetch", (event) => {
  const request = event.request;
  if (request.method !== "GET") return;
  const strategy = strategyFor(new URL(request.url));
  // Not responding is what keeps `/api/*` out of the Cache API: a catch-all handler would
  // defeat `Cache-Control: no-store` on the way past.
  if (strategy === "network-only" || strategy === "bypass") return;
  // `?order=<id>` and `#/codes` are the same document, so the query string is not part of the key.
  event.respondWith(strategy === "shell" ? shellFirst(request) : fromPrecache(request));
});

self.sw = { BUILD, CACHE, ASSETS, SHELL, ENTRY, PRECACHE, strategyFor };
