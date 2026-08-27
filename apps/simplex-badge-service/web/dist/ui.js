// Placeholder view layer; the wizard shell replaces it.
export function renderPlaceholder(root) {
    const heading = document.createElement("h1");
    heading.textContent = "SimpleX supporter badges";
    root.replaceChildren(heading);
}
