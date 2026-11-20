import { timedTest } from "./boot.js";
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { join } from "node:path";
import { ApiError, cancelInvoice, createInvoice } from "../src/api.js";

const refusalTest = timedTest(2000);

const FIXTURES = join(process.cwd(), "../../../tests/fixtures/store-service/rest-api");

interface Recorded { status: number; body: { type: string }; retryAfter?: string }

const recorded = (name: string): Recorded => JSON.parse(readFileSync(join(FIXTURES, `${name}.json`), "utf8")) as Recorded;

const answering = ({ status, body, retryAfter }: Recorded): typeof fetch =>
  (async () => new Response(JSON.stringify(body), { status, headers: retryAfter ? { "retry-after": retryAfter } : {} })) as typeof fetch;

const create = (f: typeof fetch) => createInvoice({ priceId: "p", method: "btc", codeHash: "h" }, f);
const cancel = (f: typeof fetch) => cancelInvoice("inv", f);

const REFUSALS: Array<[string, (f: typeof fetch) => Promise<unknown>]> = [
  ["invoice-bad-request", create],
  ["invoice-catalog-changed", create],
  ["invoice-code-conflict", create],
  ["invoice-internal", create],
  ["invoice-provider-unavailable", create],
  ["invoice-rate-limited", create],
  ["cancel-funded", cancel],
  ["cancel-method-not-allowed", cancel],
  ["cancel-not-found", cancel],
  ["cancel-not-open", cancel],
];

for (const [name, call] of REFUSALS) {
  refusalTest(`api: the service's recorded ${name} refusal reads as its code`, async () => {
    const answer = recorded(name);
    await assert.rejects(() => call(answering(answer)), (e: unknown) =>
      e instanceof ApiError && e.code !== "unknown" && e.code === answer.body.type && e.status === answer.status);
  });
}
