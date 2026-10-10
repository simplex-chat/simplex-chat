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

const focusHolder: { at: StubElement | null } = { at: null };

// The namespace an inline SVG element must be created in to render.
export const SVG_NS = "http://www.w3.org/2000/svg";

export class StubElement {
  readonly attrs = new Map<string, string>();
  children: Child[] = [];
  readonly listeners = new Map<string, Array<(event: StubEvent) => void>>();
  clientWidth = 560;
  scrollLeft = 0;
  lastScrollBehavior = "";
  disabled = false;
  parent: StubElement | undefined;

  // A browser draws nothing for an <svg> made in the HTML namespace, so a test must see which namespace was used.
  constructor(readonly tagName: string, readonly namespaceURI: string | null = null) {}

  setAttribute(k: string, v: string): void { this.attrs.set(k, v); }
  getAttribute(k: string): string | null { return this.attrs.get(k) ?? null; }
  hasAttribute(k: string): boolean { return this.attrs.has(k); }
  removeAttribute(k: string): void { this.attrs.delete(k); }

  // Backed by the class attribute, so all() and getAttribute stay in sync, matching the browser.
  get classList() {
    const read = (): string[] => (this.getAttribute("class") ?? "").split(/\s+/).filter(Boolean);
    const write = (list: string[]): void => {
      if (list.length > 0) this.setAttribute("class", list.join(" "));
      else this.removeAttribute("class");
    };
    return {
      contains: (name: string): boolean => read().includes(name),
      add: (name: string): void => { const l = read(); if (!l.includes(name)) write([...l, name]); },
      remove: (name: string): void => write(read().filter((n) => n !== name)),
      toggle: (name: string, force?: boolean): boolean => {
        const on = force ?? !read().includes(name);
        if (on) { const l = read(); if (!l.includes(name)) write([...l, name]); }
        else write(read().filter((n) => n !== name));
        return on;
      },
    };
  }

  append(...kids: Child[]): void {
    for (const kid of kids) if (kid instanceof StubElement) kid.parent = this;
    this.children.push(...kids);
  }

  replaceChildren(...kids: Child[]): void {
    for (const kid of kids) if (kid instanceof StubElement) kid.parent = this;
    this.children = [...kids];
  }

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
  // A real browser suppresses activation on a disabled control, so clicking one must do nothing.
  click(init: Partial<StubEvent> = {}): StubEvent {
    if (this.hasAttribute("disabled")) {
      return { button: 0, metaKey: false, ctrlKey: false, shiftKey: false, altKey: false, defaultPrevented: false, preventDefault() {} };
    }
    return this.dispatch("click", init);
  }

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

  // Depth-first; matches only the tag, .class and tag.class forms.
  all(selector: string): StubElement[] {
    const [tag, cls] = selector.split(".");
    const hit = (tag === undefined || tag === "" || this.tagName === tag)
      && (cls === undefined || (this.getAttribute("class") ?? "").split(" ").includes(cls));
    const found = hit ? [this as StubElement] : [];
    for (const c of this.children) if (c instanceof StubElement) found.push(...c.all(selector));
    return found;
  }
  querySelector(selector: string): StubElement | null { return this.all(selector)[0] ?? null; }

  get texts(): string[] {
    const out: string[] = [];
    for (const c of this.children) {
      if (c instanceof StubText) out.push(c.data);
      else out.push(...c.texts);
    }
    return out;
  }

  // The whole subtree as one string, including attributes that textContent would miss.
  serialize(): string {
    const attrs = [...this.attrs].map(([k, v]) => ` ${k}="${v}"`).join("");
    const inner = this.children.map((c) => (c instanceof StubText ? c.data : c.serialize())).join("");
    return `<${this.tagName}${attrs}>${inner}</${this.tagName}>`;
  }
}

// The clipboard a browser gives a page is present only in a secure context, and it can reject even then.
export interface Clipboard {
  readonly writes: string[];
  fail: boolean;
  absent: boolean;
}

export interface ServiceWorkers {
  readonly registrations: Array<{ url: string; appChildren: number; shell: string }>;
  fail: boolean;
}

export interface Connectivity {
  online: boolean;
}

export function installDocument(): {
  app: StubElement; chrome: StubElement; documentElement: StubElement;
  clipboard: Clipboard; document: StubDocument;
  workers: ServiceWorkers; connectivity: Connectivity;
} {
  const app = new StubElement("main");
  app.setAttribute("id", "app");
  const chrome = new StubElement("div");
  chrome.setAttribute("id", "chrome");
  // Appending a script here does not fetch, execute, or fire load/error; a test dispatches whichever it models.
  const head = new StubElement("head");
  const documentElement = new StubElement("html");
  focusHolder.at = null;
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
  head: StubElement;
  documentElement: StubElement;
  readonly activeElement: StubElement | null;
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

// pushState and replaceState update location synchronously, as a browser does.
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
