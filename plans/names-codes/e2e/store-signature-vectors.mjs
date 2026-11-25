import { createRequire } from "node:module"
import { createHash } from "node:crypto"
import { readFileSync } from "node:fs"
const W = "/project/git/simplex.domains/webapp/"
const ENS = "/project/tmp/claude-1000/-project-git-simplex-chat-4/5b3db9f9-78f1-4006-b413-b087b96f64ef/scratchpad/nsc/ens-contracts/artifacts/contracts/"
const require = createRequire(W + "package.json")
const viem = require("viem")
const { mnemonicToAccount, privateKeyToAccount } = require("viem/accounts")
const { entropyToMnemonic } = require("@scure/bip39")
const { wordlist } = require("@scure/bip39/wordlists/english")
const { secp256k1 } = require("@noble/curves/secp256k1")
const { encodeAbiParameters, encodeFunctionData, hashMessage, hashTypedData, keccak256, labelhash, namehash, recoverMessageAddress, getAddress, toHex } = viem
const abiOf = (p) => JSON.parse(readFileSync(ENS + p, "utf8")).abi
const sponsorAbi = abiOf("simplex/SimplexSponsor.sol/SimplexSponsor.json")
const controllerAbi = abiOf("simplex/SimplexController.sol/SimplexController.json")
const resolverAbi = abiOf("simplex/SimplexResolver.sol/SimplexResolver.json")
const oracleAbi = abiOf("simplex/SimplexPriceOracle.sol/SimplexPriceOracle.json")
const out = (k, v) => console.log(`${k}: ${typeof v === "bigint" ? v.toString() : v}`)
const hex = (h) => h.slice(2)
const ZERO = "0x" + "00".repeat(32)
const fromEntropy = (e) => mnemonicToAccount(entropyToMnemonic(Uint8Array.from(Buffer.from(e, "hex")), wordlist))

// 1. NameRegistrationTests: the known registration, one link per key
{
  const acct = fromEntropy("00".repeat(16))
  out("known owner", acct.address)
  const sponsor = "0x9a676e781a523b5d0c0e43731313a708cb607508", resolver = "0x2279b7a0a67db372996a5fab50d91eaa73d2ebe6"
  const node = namehash("bakery1.simplex")
  const setText = (k, v) => encodeFunctionData({ abi: resolverAbi, functionName: "setText", args: [node, k, v] })
  const build = async (data) => {
    const reg = { label: "bakery1", owner: acct.address, duration: 63072000n, secret: "0x" + "11".repeat(32), resolver, data, reverseRecord: 0, referrer: ZERO }
    const typed = {
      domain: { name: "SimplexSponsor", version: "1", chainId: 31337, verifyingContract: sponsor },
      types: { RegisterName: [{ name: "label", type: "string" }, { name: "owner", type: "address" }, { name: "duration", type: "uint256" }, { name: "resolver", type: "address" }, { name: "data", type: "bytes[]" }, { name: "nonce", type: "uint256" }, { name: "deadline", type: "uint256" }] },
      primaryType: "RegisterName", message: { label: reg.label, owner: reg.owner, duration: reg.duration, resolver, data, nonce: 0n, deadline: 4102444800n },
    }
    const sig = await acct.signTypedData(typed)
    const calldata = encodeFunctionData({ abi: sponsorAbi, functionName: "registerWithCredit", args: [reg, 0n, 4102444800n, sig] })
    const mc = controllerAbi.find((f) => f.name === "makeCommitment")
    const commitment = keccak256(encodeAbiParameters(mc.inputs, [reg]))
    return { calldata, commitment }
  }
  const withRecords = await build([setText("simplex.contact", "https://smp.simplex.im/a#contact"), setText("simplex.channel", "https://smp1.simplex.im/c#one")])
  out("register with records", hex(withRecords.calldata))
  out("register without records", hex((await build([])).calldata))
  out("commit", hex(encodeFunctionData({ abi: controllerAbi, functionName: "commit", args: [withRecords.commitment] })))
}

// 2. Signatures: the webapp's fixture wallet
const ENTROPY = "6f2d1c9a4b7e3f80d15a2c6e9b04f7a3"
const acct = fromEntropy(ENTROPY)
out("owner", acct.address)
const TERM_START = Date.UTC(2027, 0, 1) / 1000, BEFORE_START = Date.UTC(2026, 9, 21, 9, 15) / 1000
const duration = 2n * 31536000n + BigInt(TERM_START - BEFORE_START)
out("timestamp", BEFORE_START)
out("duration", duration)
const resolver = "0x2279b7a0a67db372996a5fab50d91eaa73d2ebe6", sponsor = "0x9a676e781a523b5d0c0e43731313a708cb607508"
const node = namehash("dynamis.simplex")
out("node", node)
const contactCall = encodeFunctionData({ abi: resolverAbi, functionName: "setText", args: [node, "simplex.contact", "https://smp8.simplex.im/a#contact"] })
const reg = { label: "dynamis", owner: acct.address, duration, secret: "0x" + "11".repeat(32), resolver, data: [contactCall], reverseRecord: 0, referrer: ZERO }
const commitment = keccak256(encodeAbiParameters(controllerAbi.find((f) => f.name === "makeCommitment").inputs, [reg]))
out("commitment", commitment)
const rnTyped = {
  domain: { name: "SimplexSponsor", version: "1", chainId: 31337, verifyingContract: sponsor },
  types: { RegisterName: [{ name: "label", type: "string" }, { name: "owner", type: "address" }, { name: "duration", type: "uint256" }, { name: "resolver", type: "address" }, { name: "data", type: "bytes[]" }, { name: "nonce", type: "uint256" }, { name: "deadline", type: "uint256" }] },
  primaryType: "RegisterName", message: { label: "dynamis", owner: acct.address, duration, resolver, data: [contactCall], nonce: 0n, deadline: 1800000000n },
}
out("RegisterName digest", hashTypedData(rnTyped))
const rdomain = { name: "SimplexResolver", version: "1", chainId: 31337, verifyingContract: resolver }
out("SetText digest", hashTypedData({ domain: rdomain, primaryType: "SetText", types: { SetText: [{ name: "node", type: "bytes32" }, { name: "key", type: "string" }, { name: "value", type: "string" }, { name: "nonce", type: "uint256" }, { name: "deadline", type: "uint256" }] }, message: { node, key: "simplex.contact", value: "https://x", nonce: 3n, deadline: 99n } }))
out("ClearRecords digest", hashTypedData({ domain: rdomain, primaryType: "ClearRecords", types: { ClearRecords: [{ name: "node", type: "bytes32" }, { name: "nonce", type: "uint256" }, { name: "deadline", type: "uint256" }] }, message: { node, nonce: 4n, deadline: 99n } }))

const sealed = "q3Zk0Yx7c2VhbGVkLW9yZGVyLWZpeHR1cmU"
const sealedHash = "0x" + createHash("sha256").update(sealed).digest("hex")
const codeHash = createHash("sha256").update("SN7BJTBP29596R3HJ825YM7").digest("base64url")
const regId = "t2Jk8RmQ4vXw9NcP3yLb6g"
const msgs = {
  order: `simplex.domains order ${labelhash("dynamis")} 7 ${duration} ${commitment} ${sealedHash} ${BEFORE_START}`,
  find: `simplex.domains find ${acct.address.toLowerCase()} ${BEFORE_START}`,
  name: `simplex.domains name ${regId} ${labelhash("another")} 7 ${duration} ${commitment} ${sealedHash} ${BEFORE_START}`,
  code: `simplex.domains code ${regId} ${codeHash} 7 ${BEFORE_START}`,
}
for (const [k, m] of Object.entries(msgs)) {
  const signature = await acct.signMessage({ message: m })
  if (await recoverMessageAddress({ message: m, signature }) !== acct.address) throw new Error("recover " + k)
  out(`${k} message`, m)
  out(`${k} digest`, hashMessage(m))
  out(`${k} signature`, signature)
}
// the order signature with s replaced by n - s and v flipped: the same signer, refused on chain
{
  const sig = await acct.signMessage({ message: msgs.order })
  const r = BigInt("0x" + sig.slice(2, 66)), s = BigInt("0x" + sig.slice(66, 130)), v = parseInt(sig.slice(130), 16)
  const high = "0x" + r.toString(16).padStart(64, "0") + (secp256k1.CURVE.n - s).toString(16).padStart(64, "0") + (v === 27 ? "1c" : "1b")
  out("order high-s signature", high)
}
out("labelhash another", labelhash("another"))
out("labelhash dynamis", labelhash("dynamis"))
out("code hash", codeHash)
out("sealed hash", sealedHash)

// 3. Abi return data: viem's encoding of each output type
const enc = (types, values) => hex(encodeAbiParameters(types.map((type) => ({ type })), values))
out("ret uint256", enc(["uint256"], [123456789012345678901234567890n]))
out("ret address", enc(["address"], [acct.address]))
out("ret bool", enc(["bool"], [true]))
out("ret bytes32", enc(["bytes32"], [node]))
out("ret string", enc(["string"], ["https://smp8.simplex.im/a#contact"]))
out("ret empty string", enc(["string"], [""]))
out("ret bytes", enc(["bytes"], [contactCall]))
const pricesOut = oracleAbi.find((f) => f.name === "prices").outputs
out("prices outputs", JSON.stringify(pricesOut.map((o) => o.type + " " + (o.components ?? []).map((c) => c.type).join(","))))
out("ret prices", hex(encodeAbiParameters(pricesOut, [5000n, [{ labelLength: 7n, priceCentsPerYear: 10000n }, { labelLength: 6n, priceCentsPerYear: 20000n }]])))
out("ret prices empty", hex(encodeAbiParameters(pricesOut, [5000n, []])))
out("contact call", hex(contactCall))
