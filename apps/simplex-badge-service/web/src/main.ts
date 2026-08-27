// Entry point. index.html loads this as a module and the browser resolves the
// relative specifiers below itself: tsc does not rewrite them, which is why
// they keep their ".js" extension and why the whole graph is served under one
// prefix (decision 7, D4).

import {loadCatalog} from "./catalog.js"
import {startShell} from "./ui.js"

const FAILED = "This page failed to load. Please reload, or contact support using the link below."

const app = document.getElementById("app")
if (app) {
  try {
    const shell = startShell(app)
    // Started, not awaited: the wizard is on screen while the prices are on
    // their way, and every option is disabled until they arrive. `fetch` is
    // wrapped rather than passed, because passing it unbound loses `this`.
    void loadCatalog((path) => fetch(path), shell)
  } catch (err) {
    // A shell that fails to start must still say so: the alternative is an
    // empty page with the reason only in the console (D2).
    app.replaceChildren(failureBanner())
    throw err
  }
} else {
  // Only reachable if index.html loses <main id="app"> — a template edit, or a
  // D4 substitution that mangles the page. That is exactly when a blank screen
  // would be hardest to diagnose, so it gets the same visible message.
  document.body.prepend(failureBanner())
  throw new Error('main.js found no element with id "app" to render into.')
}

function failureBanner(): HTMLElement {
  const banner = document.createElement("p")
  banner.className = "banner"
  banner.setAttribute("role", "alert")
  banner.textContent = FAILED
  return banner
}
