import { expect, test, type Page } from "@playwright/test";

const PAYER_URL = process.env.LIGHTNING_PAYER_URL;
const PAYMENT_SECONDS = 60;
const FEE_LIMIT_SAT = "1000";
const SUCCEEDED = "SUCCEEDED";
const PAYMENT_READ_MS = 60_000;
const LIGHTNING_LINK = /^lightning:lnbc/;
const BADGE_CODE = /^SB(-[0-9A-Z]{5}){4}$/;

test.skip(PAYER_URL === undefined || process.env.BASE_URL === undefined,
  "needs BASE_URL, the badges site of a service with BTCPay, and LIGHTNING_PAYER_URL, a payer's Lightning node with a channel to its store");

const inView = (page: Page) => page.locator("section.panel:not([inert])");
const heading = (page: Page) => inView(page).locator("h1").first();
const primary = (page: Page) => inView(page).locator("button.primary:not([disabled])").first();

async function orderWithLightning(page: Page): Promise<{ orderId: string; bolt11: string }> {
  await page.goto("/");
  await primary(page).click();
  await inView(page).locator("button.choice", { hasText: "Supporter" }).click();
  await primary(page).click();
  await inView(page).locator("button.choice", { hasText: "1 month" }).click();
  await primary(page).click();
  await expect(heading(page)).toHaveText("Check your order");
  await inView(page).locator("button.choice", { hasText: "Lightning" }).click();
  await expect(primary(page)).toHaveText(/ with Lightning$/);
  await primary(page).click();
  await expect(heading(page)).toHaveText(/^Send [0-9.]+ BTC$/);
  await expect(inView(page).getByText("Lightning invoice", { exact: true })).toBeVisible();
  const link = await inView(page).getByRole("link", { name: "Open in wallet" }).getAttribute("href");
  expect(link, "the wallet link").toMatch(LIGHTNING_LINK);
  const orderId = new URL(page.url()).searchParams.get("order");
  expect(orderId, "the order's link").not.toBeNull();
  return { orderId: orderId!, bolt11: decodeURIComponent(link!.slice("lightning:".length)) };
}

async function payAtNode(bolt11: string): Promise<void> {
  const res = await fetch(`${PAYER_URL}/v2/router/send`, {
    method: "POST",
    body: JSON.stringify({ payment_request: bolt11, timeout_seconds: PAYMENT_SECONDS, fee_limit_sat: FEE_LIMIT_SAT, no_inflight_updates: true }),
  });
  const answer = await res.text();
  const last = answer.trim().split("\n").at(-1)!;
  expect((JSON.parse(last) as { result?: { status?: string } }).result?.status, answer).toBe(SUCCEEDED);
}

test("a badge bought with Lightning shows its code once its invoice, linked from the page, is paid at a Lightning node", async ({ page }) => {
  const { bolt11 } = await orderWithLightning(page);
  await payAtNode(bolt11);
  await expect(heading(page)).toHaveText("Paid. Here is your code.", { timeout: PAYMENT_READ_MS });
  await expect(inView(page).getByText(BADGE_CODE)).toBeVisible();
});

test("a cancelled Lightning badge order, its invoice paid at a node afterwards, is shown as cancelled with what arrived", async ({ page }) => {
  const { orderId, bolt11 } = await orderWithLightning(page);
  page.once("dialog", (dialog) => void dialog.accept());
  await inView(page).getByRole("button", { name: "Cancel this invoice" }).click();
  await page.goto(`/?order=${orderId}`);
  await expect(heading(page)).toHaveText("Invoice canceled");
  await expect(inView(page).getByText(/^Do not pay its Lightning invoice: /)).toBeVisible();
  await payAtNode(bolt11);
  await expect(async () => {
    await page.goto(`/?order=${orderId}`);
    await expect(inView(page).getByText(/^[0-9.]+ BTC arrived for this canceled invoice$/)).toBeVisible({ timeout: 2_000 });
  }).toPass({ timeout: PAYMENT_READ_MS });
  await expect(heading(page)).toHaveText("Invoice canceled");
});
