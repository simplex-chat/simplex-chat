// Placeholder entry point; the wizard shell replaces it.
// The relative specifier keeps its ".js" extension into the emitted module:
// tsc does not rewrite specifiers and the browser resolves them itself.
import { renderPlaceholder } from "./ui.js";
const app = document.getElementById("app");
if (app)
    renderPlaceholder(app);
