import type { Method } from "./domain.js";

const SVG_NS = "http://www.w3.org/2000/svg";

function svg(attrs: Record<string, string>, ...kids: SVGElement[]): SVGElement {
  const node = document.createElementNS(SVG_NS, "svg");
  node.setAttribute("xmlns", SVG_NS);
  for (const [k, v] of Object.entries(attrs)) node.setAttribute(k, v);
  for (const kid of kids) node.append(kid);
  return node;
}

function shape(tag: string, attrs: Record<string, string>): SVGElement {
  const node = document.createElementNS(SVG_NS, tag);
  for (const [k, v] of Object.entries(attrs)) node.setAttribute(k, v);
  return node;
}

export function hamburger(): SVGElement {
  const box = { class: "bars", viewBox: MARK_VIEWBOX, "aria-hidden": "true", focusable: "false" };
  const bar = (y: string): SVGElement => shape("line", {
    x1: "3", y1: y, x2: "21", y2: y,
    stroke: "currentColor", "stroke-width": "2", "stroke-linecap": "round",
  });
  return svg(box, bar("7"), bar("12"), bar("17"));
}

const BADGE_STOPS = {
  supporter: [["0%", "#29f5ff"], ["5%", "#29f5ff"], ["95%", "#527eed"], ["100%", "#3669e9"]],
  legend: [["0%", "#29f5ff"], ["5%", "#26dee8"], ["50%", "#3064ea"], ["100%", "#001064"]],
} as const satisfies Readonly<Record<string, ReadonlyArray<readonly [string, string]>>>;

export type BadgeTier = keyof typeof BADGE_STOPS;

export function hasBadgeArt(tier: string): tier is BadgeTier {
  return Object.prototype.hasOwnProperty.call(BADGE_STOPS, tier);
}

const BADGE_BODY = "M98.25,8.25h120a90,90,0,0,1,90,90v219a90,90,0,0,1-90,90h-120a90,90,0,0,1-90-90v-219a90,90,0,0,1,90-90Z";

const BADGE_GLYPH = "M107.25,214.5h-8.62A40.51,40.51,0,0,1,58.12,174V117.37a40.51,40.51,0,0,1,40.51-40.5h8.62a40.5,40.5,0,0,1,40.5,40.5V174a40.5,40.5,0,0,1-40.5,40.5Zm18-93.38a21.75,21.75,0,0,0-21.75-21.75h-1.12a21.75,21.75,0,0,0-21.76,21.75v49.13A21.75,21.75,0,0,0,102.38,192h1.12a21.75,21.75,0,0,0,21.75-21.75V121.12Z M218.25,214.5h-8.63a40.5,40.5,0,0,1-40.5-40.5V117.37a40.51,40.51,0,0,1,40.5-40.5h8.63a40.5,40.5,0,0,1,40.5,40.5V174a40.5,40.5,0,0,1-40.5,40.5Zm18-93.38a21.75,21.75,0,0,0-21.75-21.75h-1.13a21.75,21.75,0,0,0-21.75,21.75v49.13A21.75,21.75,0,0,0,213.37,192h1.13a21.75,21.75,0,0,0,21.75-21.75V121.12Z M114.37,133.88h88.88a13.13,13.13,0,1,1,0,26.25H114.37a13.13,13.13,0,0,1,0-26.25Z";

const BADGE_VIEWBOX = "8.25 8.25 300 399";

let gradientSeq = 0;

// createElementNS everywhere: an <svg> made with createElement in an HTML document is an HTMLUnknownElement
// and draws nothing. No builder takes an argument that could carry order data, so no code, address or
// reference reaches a `d`, a `fill` or an `aria-label`.
// Path data is copied verbatim from the app's own SVGs; the brand mark ships as a file the stylesheet draws.
export function badgeIcon(tier: BadgeTier): SVGElement {
  const id = `sxb-badge-${tier}-${(gradientSeq += 1)}`;
  const gradient = shape("linearGradient", { id, x1: "0", y1: "0", x2: "0", y2: "1" });
  for (const [offset, color] of BADGE_STOPS[tier]) {
    gradient.append(shape("stop", { offset, "stop-color": color }));
  }
  const defs = shape("defs", {});
  defs.append(gradient);
  return svg({ class: "badge-art", viewBox: BADGE_VIEWBOX, "aria-hidden": "true", focusable: "false" },
    defs,
    shape("path", { fill: `url(#${id})`, d: BADGE_BODY }),
    shape("path", { fill: "#ffffff", "fill-rule": "nonzero", d: BADGE_GLYPH }),
  );
}

const MARK_VIEWBOX = "0 0 24 24";

const BITCOIN = "M23.638 14.904c-1.602 6.43-8.113 10.34-14.542 8.736C2.67 22.05-1.244 15.525.362 9.105 1.962 2.67 8.475-1.243 14.9.358c6.43 1.605 10.342 8.115 8.738 14.548v-.002zm-6.35-4.613c.24-1.59-.974-2.45-2.64-3.03l.54-2.153-1.315-.33-.525 2.107c-.345-.087-.705-.167-1.064-.25l.526-2.127-1.32-.33-.54 2.165c-.285-.067-.565-.132-.84-.2l-1.815-.45-.35 1.407s.975.225.955.236c.535.136.63.486.615.766l-1.477 5.92c-.075.166-.24.406-.614.314.015.02-.96-.24-.96-.24l-.66 1.51 1.71.426.93.242-.54 2.19 1.32.327.54-2.17c.36.1.705.19 1.05.273l-.51 2.154 1.32.33.545-2.19c2.24.427 3.93.257 4.64-1.774.57-1.637-.03-2.58-1.217-3.196.854-.193 1.5-.76 1.68-1.93h.01zm-3.01 4.22c-.404 1.64-3.157.75-4.05.53l.72-2.9c.896.23 3.757.67 3.33 2.37zm.41-4.24c-.37 1.49-2.662.735-3.405.55l.654-2.64c.744.18 3.137.524 2.75 2.084v.006z";
const MONERO = "M12 0C5.365 0 0 5.373 0 12.015c0 1.335.228 2.607.618 3.81h3.577V5.729L12 13.545l7.805-7.815v10.095h3.577c.389-1.203.618-2.475.618-3.81C24 5.375 18.635 0 12 0zm-1.788 15.307l-3.417-3.421v6.351H1.758C3.87 21.689 7.678 24 12 24s8.162-2.311 10.245-5.764h-5.04v-6.351l-3.386 3.421-1.788 1.79-1.814-1.79h-.005z";

const CARD_STROKE = "currentColor";

export function methodMark(method: Method): SVGElement {
  const box = { class: "mark", viewBox: MARK_VIEWBOX, "aria-hidden": "true", focusable: "false" };
  switch (method) {
    case "btc":
      return svg(box, shape("path", { fill: "#F7931A", d: BITCOIN }));
    case "xmr":
      return svg(box, shape("path", { fill: "#FF6600", d: MONERO }));
    case "card":
      return svg(box,
        shape("rect", {
          x: "2", y: "5", width: "20", height: "14", rx: "2",
          fill: "none", stroke: CARD_STROKE, "stroke-width": "2",
        }),
        shape("line", {
          x1: "2", y1: "10", x2: "22", y2: "10",
          stroke: CARD_STROKE, "stroke-width": "2",
        }),
      );
  }
}
