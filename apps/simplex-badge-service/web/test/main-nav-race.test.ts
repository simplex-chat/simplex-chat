// renderCodes takes the root, so without its hash guard a late refresh paints the list over whatever screen the buyer moved to.
import assert from "node:assert/strict";
import { headingOf, inViewOf, installPage, settle, timedTest, until } from "./boot.js";
import { openReply, ORDER_ID, seededStorage } from "./open-order.js";

const navTest = timedTest(3000);

const page = installPage({ storage: seededStorage(), url: `/?order=${ORDER_ID}` });
const { app, history, location } = page;
page.respondWith(openReply);
await import("../src/main.js");

const inView = (): ReturnType<typeof inViewOf> => inViewOf(app);

navTest("main: a history refresh landing after the buyer moved on does not take the screen", async () => {
  await until(() => headingOf(inView()).startsWith("Send"), "the payment screen");

  page.respondWith(openReply);
  page.chrome.all("button.menu-item").find((b) => b.textContent === "Your codes")!.click();
  assert.equal(headingOf(inView()), "Your codes", "the list is drawn from the store at once");

  page.respondWith(openReply);
  history.pushState(null, "", `/?order=${ORDER_ID}`);
  page.fire("popstate");
  assert.equal(location.hash, "", "the hash the guard reads has moved off the list");

  await settle(10);
  assert.equal(headingOf(inView()), "Send 1.482 XMR",
    `the refresh must not put the list back over the order: ${headingOf(inView())}`);
});
