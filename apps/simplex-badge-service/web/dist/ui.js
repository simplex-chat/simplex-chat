// The wizard shell: the only module that touches the DOM.
//
// One question per screen, hash-routed with no page reload. Forward navigation
// is history.pushState, which does not fire popstate, so the shell renders
// directly; back and forward are history traversals, which do fire it, so the
// listener re-renders from the hash. There is no hashchange listener: every
// hash change originates here.
//
// Everything decidable without a browser lives in router.ts and view.ts.
import { hashForScreen, nextScreen, screenIdForHash } from "./router.js";
import { firstUnansweredScreen, questionOfScreen, screenView } from "./view.js";
const CHOOSE_AN_OPTION = "Choose an option to continue.";
// D7 replaces this branch with the POST to /api/checkout.
const PAYMENT_UNAVAILABLE = "Payment is not available yet.";
/** Build elements from a view tree. No innerHTML: text is always a text node. */
export function toDom(el) {
    const node = document.createElement(el.tag);
    for (const name of Object.keys(el.attrs))
        node.setAttribute(name, el.attrs[name]);
    for (const child of el.children)
        node.append(childNode(child));
    return node;
}
function childNode(child) {
    return typeof child === "string" ? child : toDom(child);
}
/**
 * Render the screen the current hash names into `root`, and keep it in step
 * with history.
 *
 * `initial` seeds the answers, which is how D5's prefill skips the screens a
 * query parameter has already answered: a seeded question is not asked, and
 * the visit starts at the first one that has no answer.
 */
export function startShell(root, initial = {}) {
    const banner = errorBanner();
    const answers = { ...initial };
    let current = firstUnansweredScreen(answers);
    const shell = {
        showError(message) {
            banner.textContent = message;
            banner.hidden = false;
        },
        go(id) {
            history.pushState(null, "", hashForScreen(id));
            render(id, true);
        },
        refresh() {
            render(current, false);
        },
        answers() {
            return { ...answers };
        },
    };
    function clearError() {
        banner.hidden = true;
        banner.textContent = "";
    }
    function render(id, moveFocus) {
        current = id;
        clearError();
        const screen = toDom(screenView(id, answers));
        root.replaceChildren(banner, screen);
        // The heading is the start of the new screen for a keyboard or screen
        // reader user, who would otherwise stay on a button that no longer exists.
        const heading = screen.querySelector("h1");
        if (heading) {
            heading.setAttribute("tabindex", "-1");
            if (moveFocus)
                heading.focus();
        }
    }
    // An unknown hash renders a real screen rather than nothing, and the address
    // bar is corrected in place so that back does not return to it. The fallback
    // is the first *unanswered* screen, not the first screen: with D5's prefill
    // seeded, sending a visitor back to a question already answered by their URL
    // would undo the whole point of the parameters.
    function screenFromLocation() {
        const id = screenIdForHash(location.hash);
        if (id !== null && location.hash === hashForScreen(id))
            return id;
        const target = id ?? firstUnansweredScreen(answers);
        history.replaceState(null, "", hashForScreen(target));
        return target;
    }
    function onSubmit(event) {
        event.preventDefault();
        const question = questionOfScreen(current);
        if (!question) {
            // The only other screen with a form is #/checkout, whose Pay button D7 wires.
            shell.showError(PAYMENT_UNAVAILABLE);
            return;
        }
        const form = event.target;
        if (!(form instanceof HTMLFormElement))
            return;
        const chosen = form.querySelector("input[type=radio]:checked");
        if (!chosen) {
            shell.showError(CHOOSE_AN_OPTION);
            return;
        }
        answers[question] = chosen.value;
        const next = nextScreen(current);
        if (next)
            shell.go(next);
    }
    root.addEventListener("submit", onSubmit);
    // Choosing an option answers the complaint the banner is showing.
    root.addEventListener("change", clearError);
    window.addEventListener("popstate", () => render(screenFromLocation(), true));
    render(screenFromLocation(), false);
    return shell;
}
function errorBanner() {
    const banner = document.createElement("p");
    banner.className = "banner";
    // role=alert so the message is announced when it appears, not only seen.
    banner.setAttribute("role", "alert");
    banner.hidden = true;
    return banner;
}
