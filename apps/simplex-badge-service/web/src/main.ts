// Entry point. index.html loads this as a module and the browser resolves the
// relative specifiers below itself: tsc does not rewrite them, which is why
// they keep their ".js" extension and why the whole graph is served under one
// prefix (decision 7, D4).

import {startShell} from "./ui.js"

const app = document.getElementById("app")
if (app) {
  try {
    startShell(app)
  } catch (err) {
    // A shell that fails to start must still say so: the alternative is an
    // empty page with the reason only in the console (D2).
    const banner = document.createElement("p")
    banner.className = "banner"
    banner.setAttribute("role", "alert")
    banner.textContent = "This page failed to load. Please reload, or contact support using the link below."
    app.replaceChildren(banner)
    throw err
  }
}
