// A stub DOM small enough to be obviously honest: createElement,
// createElementNS, text nodes, attributes, listeners, and the handful of

/** Only the fields a handler in this codebase reads. */
export interface StubEvent {
  button: number;
  metaKey: boolean;
  ctrlKey: boolean;
  shiftKey: boolean;
  altKey: boolean;
  defaultPrevented: boolean;
  preventDefault(): void;
}

export class StubText {
  constructor(public data: string) {}
  get textContent(): string { return this.data; }
}

export type Child = StubElement | StubText;

/** Who `document.activeElement` answers with. One per process, as a browser has. */
const focusHolder: { at: StubElement | null } = { at: null };

/** The namespace an inline SVG element must be created in to render at all. */
export const SVG_NS = "http://www.w3.org/2000/svg";

export class StubElement {
  readonly attrs = new Map<string, string>();
  children: Child[] = [];
  readonly listeners = new Map<string, Array<(event: StubEvent) => void>>();
  /** The width one panel occupies; the track is the only element that reads it. */
  clientWidth = 560;
  scrollLeft = 0;
  lastScrollBehavior = "";
  disabled = false;
  /** Set by `append`/`replaceChildren`, so `remove` can find its holder. */
  parent: StubElement | undefined;

  /** Null for `createElement`, the namespace URI for `createElementNS`. A real browser draws nothing for an
   * `<svg>` made in the HTML namespace, so a test that cares whether a symbol would render has to see which
   * was used. This is the whole of what the stub knows about namespaces. */
  constructor(readonly tagName: string, readonly namespaceURI: string | null = null) {}

  setAttribute(k: string, v: string): void { this.attrs.set(k, v); }
  getAttribute(k: string): string | null { return this.attrs.get(k) ?? null; }
  hasAttribute(k: string): boolean { return this.attrs.has(k); }
  removeAttribute(k: string): void { this.attrs.delete(k); }

  append(...kids: Child[]): void {
    for (const kid of kids) if (kid instanceof StubElement) kid.parent = this;
    this.children.push(...kids);
  }

  replaceChildren(...kids: Child[]): void {
    for (const kid of kids) if (kid instanceof StubElement) kid.parent = this;
    this.children = [...kids];
  }

  /** As `Element.remove`: takes itself out of whatever holds it, and nothing if nothing does. */
  remove(): void {
    const holder = this.parent;
    if (holder === undefined) return;
    holder.children = holder.children.filter((c) => c !== this);
    this.parent = undefined;
  }
  replaceChild(next: Child, prev: Child): void {
    const at = this.children.indexOf(prev);
    if (at < 0) throw new Error("replaceChild: the node is not a child of this element");
    this.children[at] = next;
  }
  get firstChild(): Child | null { return this.children[0] ?? null; }

  addEventListener(type: string, fn: (event: StubEvent) => void): void {
    const list = this.listeners.get(type) ?? [];
    list.push(fn);
    this.listeners.set(type, list);
  }
  removeEventListener(type: string, fn: (event: StubEvent) => void): void {
    const list = this.listeners.get(type);
    if (list === undefined) return;
    const at = list.indexOf(fn);
    if (at >= 0) list.splice(at, 1);
  }
  /** How many listeners are attached, so a leak is observable at all. */
  listenerCount(type: string): number { return this.listeners.get(type)?.length ?? 0; }
  dispatch(type: string, init: Partial<StubEvent> = {}): StubEvent {
    const event: StubEvent = {
      button: 0, metaKey: false, ctrlKey: false, shiftKey: false, altKey: false,
      defaultPrevented: false,
      preventDefault() { this.defaultPrevented = true; },
      ...init,
    };
    for (const fn of [...(this.listeners.get(type) ?? [])]) fn(event);
    return event;
  }
  /** A real browser suppresses activation on a disabled control, so a test that clicks one must observe
   * nothing happening rather than the handler running. `init` carries the modifier keys a link handler
   * has to distinguish. */
  click(init: Partial<StubEvent> = {}): StubEvent {
    if (this.hasAttribute("disabled")) {
      return { button: 0, metaKey: false, ctrlKey: false, shiftKey: false, altKey: false, defaultPrevented: false, preventDefault() { /* suppressed */ } };
    }
    return this.dispatch("click", init);
  }

  /** Focus is a document-wide fact, so the element records the call and the document below records who holds
   * it. No focus ring, no tab order, no containment: what is provable is which element was asked for focus,
   * which is exactly what Escape returning it to the menu button turns on. */
  focused = 0;
  focus(): void {
    this.focused += 1;
    focusHolder.at = this;
  }

  scrollTo(opts: { left: number; behavior?: string }): void {
    this.scrollLeft = opts.left;
    this.lastScrollBehavior = opts.behavior ?? "";
  }

  get textContent(): string { return this.children.map((c) => c.textContent).join(""); }
  set textContent(v: string) { this.children = [new StubText(v)]; }

  /** Depth-first, matching only the `tag`, `.class` and `tag.class` forms used in tests. */
  all(selector: string): StubElement[] {
    const [tag, cls] = selector.split(".");
    const hit = (tag === undefined || tag === "" || this.tagName === tag)
      && (cls === undefined || (this.getAttribute("class") ?? "").split(" ").includes(cls));
    const found = hit ? [this as StubElement] : [];
    for (const c of this.children) if (c instanceof StubElement) found.push(...c.all(selector));
    return found;
  }
  querySelector(selector: string): StubElement | null { return this.all(selector)[0] ?? null; }

  /** Every string of text in the tree, in order. */
  get texts(): string[] {
    const out: string[] = [];
    for (const c of this.children) {
      if (c instanceof StubText) out.push(c.data);
      else out.push(...c.texts);
    }
    return out;
  }

  /** The whole subtree as one string: tag names, every attribute name and value, and every text node. A guard
   * that reads only `textContent` misses a code smuggled into `title`, `data-*`, `aria-label`, `href` or
   * `value`, where a tooltip or a screen reader would happily surface it. */
  serialize(): string {
    const attrs = [...this.attrs].map(([k, v]) => ` ${k}="${v}"`).join("");
    const inner = this.children.map((c) => (c instanceof StubText ? c.data : c.serialize())).join("");
    return `<${this.tagName}${attrs}>${inner}</${this.tagName}>`;
  }
}

/** The clipboard a real browser gives a page: present only in a secure context, and free to reject even then
 * (no transient activation, permission refused). Both are switchable, since on the code screen a silent
 * failure would leave the buyer believing they had copied the only copy of their code. */
export interface Clipboard {
  readonly writes: string[];
  /** `writeText` rejects. */
  fail: boolean;
  /** `navigator.clipboard` is undefined, as on an insecure origin. */
  absent: boolean;
}

/** What `navigator.serviceWorker.register` was asked and, the point of it, what `#app` held at the moment it
 * was asked. Offline support and Anubis require registration only after a load that produced the real shell,
 * and an empty `#app` at that moment is exactly the load that did not. */
export interface ServiceWorkers {
  /** `shell` is the tag and class of what `#app` held then; `div.track` is this build's. */
  readonly registrations: Array<{ url: string; appChildren: number; shell: string }>;
  /** `register` rejects, as a browser does on a 404 or a syntax error in the worker. */
  fail: boolean;
}

/** `navigator.onLine`, which the offline note is read from. */
export interface Connectivity {
  online: boolean;
}

/**
 * Installs `document` and a clipboard recorder. the ban on showing an unpaid
 * code covers the clipboard too, so what is written to it has to be observable.
 */
export function installDocument(): {
  app: StubElement; chrome: StubElement; documentElement: StubElement;
  clipboard: Clipboard; document: StubDocument;
  workers: ServiceWorkers; connectivity: Connectivity;
} {
  const app = new StubElement("main");
  app.setAttribute("id", "app");
  // The shell's header slot. `main.ts` builds the wordmark and the menu into
  // it, and throws without it, exactly as it does for `#app`.
  const chrome = new StubElement("div");
  chrome.setAttribute("id", "chrome");
  // `<head>`, so a script tag appended to it is observable, and a container and nothing more: appending a
  // script does not fetch, execute or define a global, and no `load` or `error` event fires on its own, a
  // test dispatching whichever it is modelling. Loading Stripe.js for real still needs a browser.
  const head = new StubElement("head");
  // `<html>`, which is where the chosen theme is written.
  const documentElement = new StubElement("html");
  focusHolder.at = null;
  // The shell's footer, which is inerted with the screen while the menu is open.
  const contact = new StubElement("footer");
  contact.setAttribute("id", "contact");
  const byId = new Map<string, StubElement>([
    ["app", app], ["chrome", chrome], ["contact", contact], ["head", head],
  ]);
  const clipboard: Clipboard = { writes: [], fail: false, absent: false };
  const workers: ServiceWorkers = { registrations: [], fail: false };
  const connectivity: Connectivity = { online: true };
  const doc: StubDocument = {
    hidden: false,
    head,
    documentElement,
    get activeElement() { return focusHolder.at; },
    byId,
    listeners: new Map<string, Array<() => void>>(),
    createElement: (tag: string) => new StubElement(tag),
    createElementNS: (ns: string, tag: string) => new StubElement(tag, ns),
    createTextNode: (data: string) => new StubText(data),
    getElementById: (id: string) => byId.get(id) ?? null,
    addEventListener(type: string, fn: () => void) {
      const list = this.listeners.get(type) ?? [];
      list.push(fn);
      this.listeners.set(type, list);
    },
    dispatch(type: string) { for (const fn of [...(this.listeners.get(type) ?? [])]) fn(); },
  };
  Object.defineProperty(globalThis, "document", { configurable: true, value: doc });
  const api = {
    writeText: (v: string): Promise<void> => {
      if (clipboard.fail) return Promise.reject(new Error("NotAllowedError"));
      clipboard.writes.push(v);
      return Promise.resolve();
    },
  };
  const serviceWorker = {
    register: (url: string): Promise<{ scope: string }> => {
      const first = app.children[0];
      const shell = first instanceof StubElement ? `${first.tagName}.${first.getAttribute("class") ?? ""}` : "";
      workers.registrations.push({ url, appChildren: app.children.length, shell });
      return workers.fail
        ? Promise.reject(new Error("SecurityError"))
        : Promise.resolve({ scope: "/" });
    },
  };
  Object.defineProperty(globalThis, "navigator", {
    configurable: true,
    value: {
      get clipboard() { return clipboard.absent ? undefined : api; },
      get onLine() { return connectivity.online; },
      serviceWorker,
    },
  });
  return { app, chrome, documentElement, clipboard, document: doc, workers, connectivity };
}

export interface StubDocument {
  hidden: boolean;
  /** Where a script tag lands. See `installDocument`: it is inert. */
  head: StubElement;
  /** `<html>`, which carries `data-theme` when the buyer overrides the system. */
  documentElement: StubElement;
  readonly activeElement: StubElement | null;
  /** What `getElementById` answers from. A test seeds the shell's meta elements here. */
  byId: Map<string, StubElement>;
  listeners: Map<string, Array<() => void>>;
  createElement(tag: string): StubElement;
  createElementNS(ns: string, tag: string): StubElement;
  createTextNode(data: string): StubText;
  getElementById(id: string): StubElement | null;
  addEventListener(type: string, fn: () => void): void;
  dispatch(type: string): void;
}

// ------------------------------------------------------- location and history

/** A real back/forward stack, so `history.back()` is the browser's Back. `pushState` and `replaceState`
 * update `location` synchronously, as a browser does: syncing only on popstate hid that anything reading
 * `location` right after a push saw the old URL. */
export class StubHistory {
  readonly stack: string[] = ["/"];
  at = 0;
  constructor(private readonly onPop: () => void, private readonly onNavigate: () => void = () => {}) {}

  private resolve(url: string): string {
    const current = this.stack[this.at]!;
    const [path] = current.split(/[?#]/);
    if (url.startsWith("#")) {
      const [base] = current.split("#");
      return `${base}${url}`;
    }
    if (url.startsWith("?")) return `${path}${url}`;
    return url;
  }

  pushState(_state: unknown, _title: string, url: string): void {
    this.stack.length = this.at + 1;
    this.stack.push(this.resolve(url));
    this.at += 1;
    this.onNavigate();
  }

  replaceState(_state: unknown, _title: string, url: string): void {
    this.stack[this.at] = this.resolve(url);
    this.onNavigate();
  }

  back(): void {
    if (this.at === 0) { this.left = true; return; }
    this.at -= 1;
    this.onPop();
  }

  /** Set when Back was pressed with nothing behind it: the "leaves the site". */
  left = false;

  get url(): string { return this.stack[this.at]!; }
}

export interface StubLocation { pathname: string; search: string; hash: string }

export function locationOf(url: string): StubLocation {
  const hashAt = url.indexOf("#");
  const hash = hashAt >= 0 ? url.slice(hashAt) : "";
  const rest = hashAt >= 0 ? url.slice(0, hashAt) : url;
  const queryAt = rest.indexOf("?");
  return {
    pathname: queryAt >= 0 ? rest.slice(0, queryAt) : rest,
    search: queryAt >= 0 ? rest.slice(queryAt) : "",
    hash,
  };
}

export class MemStorage {
  readonly m = new Map<string, string>();
  getItem(k: string): string | null { return this.m.get(k) ?? null; }
  setItem(k: string, v: string): void { this.m.set(k, v); }
  removeItem(k: string): void { this.m.delete(k); }
}
