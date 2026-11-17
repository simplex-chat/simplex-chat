import { readFileSync, writeFileSync } from "node:fs"
import { join } from "node:path"
import { pathToFileURL } from "node:url"

const PHASE_MS = 300_000
const NO_LINKS = { contact: "", channel: "" }

const [command, service, ...args] = process.argv.slice(2)
const webapp = process.env.NAMES_WEBAPP
if (!webapp || !service) {
  console.error("usage: NAMES_WEBAPP=<webapp dir> order.mjs register <service url> <code> <label> <out .json> | set-link <service url> <name-code .json> <address>")
  process.exit(1)
}

const ownFetch = globalThis.fetch
globalThis.fetch = (input, init) => typeof input === "string" && input.startsWith("/") ? ownFetch(`${service}${input}`, init) : ownFetch(input, init)
const site = (file) => import(pathToFileURL(join(webapp, "src", file)).href)
const api = await site("api.ts")
const flow = await site("flow.ts")

async function untilPhase(id, phases) {
  const end = Date.now() + PHASE_MS
  let view = await api.readRegistration(id)
  while (!phases.includes(view.phase)) {
    if (["failed", "taken"].includes(view.phase) || Date.now() > end) {
      throw new Error(`registration ${id} is ${view.phase}, not ${phases.join(" or ")}${view.error ? `: ${view.error}` : ""}`)
    }
    view = await api.readRegistration(id, view.version)
  }
  console.log(`registration ${id}: ${view.phase}`)
  return view
}

async function untilRevealable(id) {
  const view = await untilPhase(id, ["committed"])
  await new Promise((r) => setTimeout(r, Math.max(0, (view.revealAfter ?? 0) * 1000 - Date.now())))
  return api.readRegistration(id)
}

async function register(code, label, out) {
  const quote = await flow.quoteOf(label)
  if (!quote) throw new Error(`${label} is not available`)
  const draft = { label, links: NO_LINKS, chain: flow.chainOf(quote), years: quote.years, termStart: quote.termStart, ownerCode: flow.newOwnerCode(), terms: api.currentTerms(quote) }
  const built = await flow.orderRequest(draft)
  const { registrationId } = await api.registerWithCode(built.request, code, quote.chainId)
  await flow.revealOrder(await untilRevealable(registrationId), flow.recordFor(draft, built, "code", 0, { registrationId, invoiceId: null }))
  await untilPhase(registrationId, ["registered"])
  const name = { name: `${label}.${quote.tld}`, label, nameCode: draft.ownerCode, registrationId }
  writeFileSync(out, JSON.stringify(name, null, 2) + "\n")
  console.log(`${name.name} is registered, held by its name code`)
}

async function setLink(file, address) {
  const { nameCode } = JSON.parse(readFileSync(file, "utf8"))
  const { record, view } = await flow.restore(nameCode)
  const answer = await api.searchName(view.labelHash, record.label.length)
  if (answer.status !== "registered" || answer.recordsNonce == null) throw new Error(`${record.label} has no records to edit: ${JSON.stringify(answer)}`)
  const tx = await flow.saveLinks(record, view, answer.recordsNonce, { contact: address })
  console.log(`${record.label}.${view.tld} contact link set to ${address} in transaction ${tx}`)
}

try {
  switch (command) {
    case "register":
      await register(...args)
      break
    case "set-link":
      await setLink(...args)
      break
    default:
      throw new Error(`unknown command ${command}`)
  }
} catch (e) {
  console.error(e instanceof api.ApiError ? `error: ${e.code}, HTTP ${e.status} ${JSON.stringify(e.body)}` : e)
  process.exit(1)
}
