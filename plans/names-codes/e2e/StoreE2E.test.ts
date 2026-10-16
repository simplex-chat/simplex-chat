// The simplex.domains store flow against PR #36 (PR #35 plus the signed duration), in process: owner code
// and wallet, commit, the owner's intent, registerWithCredit by the registrar, record edits, restore from
// the code, and redeeming.
import { createCipheriv, createDecipheriv, createHash, createHmac, pbkdf2Sync, randomBytes } from 'node:crypto'
import hre from 'hardhat'
import {
  encodeFunctionData,
  getAddress,
  labelhash,
  namehash,
  toHex,
  zeroHash,
} from 'viem'
import { english, mnemonicToAccount, privateKeyToAccount } from 'viem/accounts'
import { describe, expect, it } from 'vitest'

import {
  AMPLE_ALLOWANCE,
  deployNamesV2,
  eip712Types,
  MIN_REGISTRATION,
  YEAR,
} from './fixtures/namesV2.js'

const connection = await hre.network.connect()
const [ownerClient, guardianClient, registrarClient] =
  await connection.viem.getWalletClients()
const owner = ownerClient.account
const guardian = guardianClient.account
const registrar = registrarClient.account
const publicClient = await connection.viem.getPublicClient()

// the owner code, as Alain's formatNameCode / parseNameCode (Badges/Code.hs)
const ALPHABET = '0123456789ABCDEFGHJKMNPQRSTVWXYZ'
const sha256 = (b: Uint8Array) => createHash('sha256').update(b).digest()
// the owner code carries the first 22 bits of the entropy's SHA-256
const CHECKSUM_BITS = 22n
const ENTROPY_BYTES = 16
const OWNER_CODE_CHARS = 30
const BASE = 32n
const checksumOf = (entropy: Uint8Array) => BigInt('0x' + sha256(entropy).toString('hex')) >> (256n - CHECKSUM_BITS)

function formatOwnerCode(entropy: Uint8Array): string {
  const payload = BigInt('0x' + Buffer.from(entropy).toString('hex'))
  let n = payload * 2n ** CHECKSUM_BITS + checksumOf(entropy)
  const digits: string[] = []
  for (let i = 0; i < OWNER_CODE_CHARS; i++) {
    digits.unshift(ALPHABET[Number(n % BASE)])
    n /= BASE
  }
  return 'SN-' + digits.join('').match(/.{5}/g)!.join('-')
}

function parseOwnerCode(text: string): Uint8Array {
  const folded = text.toUpperCase().replace(/[^0-9A-Z]/g, '').replace(/[IL]/g, '1').replace(/O/g, '0')
  if (!folded.startsWith('SN')) throw new Error('not SN')
  const body = folded.slice(2)
  if (body.length !== OWNER_CODE_CHARS) throw new Error('length')
  let n = 0n
  for (const c of body) {
    const v = ALPHABET.indexOf(c)
    if (v < 0) throw new Error('char')
    n = n * BASE + BigInt(v)
  }
  const payload = n >> CHECKSUM_BITS
  const entropy = Buffer.from(payload.toString(16).padStart(ENTROPY_BYTES * 2, '0'), 'hex')
  if (n % 2n ** CHECKSUM_BITS !== checksumOf(entropy)) throw new Error('checksum')
  return entropy
}

// BIP-39: 128 bits of entropy and 4 bits of its SHA-256, as 12 words
function entropyToMnemonic(entropy: Uint8Array): string {
  const bits =
    [...entropy].map((b) => b.toString(2).padStart(8, '0')).join('') +
    sha256(entropy)[0].toString(2).padStart(8, '0').slice(0, 4)
  return bits.match(/.{11}/g)!.map((b) => english[parseInt(b, 2)]).join(' ')
}

function walletFromCode(code: string) {
  const mnemonic = entropyToMnemonic(parseOwnerCode(code))
  // viem's default path is m/44'/60'/0'/0/0, account 0
  const account = mnemonicToAccount(mnemonic)
  // the BIP-39 seed, PBKDF2-SHA512 of the phrase with salt "mnemonic", as the page computes it with WebCrypto
  const seed = pbkdf2Sync(mnemonic, 'mnemonic', 2048, 64, 'sha512')
  return { account, seed }
}

// the commitment secret, derived from the code so the code alone finishes an order
const commitmentSecret = (seed: Uint8Array, label: string) =>
  toHex(createHmac('sha256', Buffer.from(seed)).update('simplex.domains commitment ' + label).digest())

// the order's label and record values, sealed under a key from the code so another browser can rebuild the commitment
type Sealed = { label: string; contact: string; channel: string }
const sealingKey = (seed: Uint8Array) => createHmac('sha256', Buffer.from(seed)).update('simplex.domains sealed').digest()
// snrc-resolve.py splits a record's links on it
const LINK_SEPARATOR = ';'
const RECORD_MAX_BYTES = 4096
const LABEL_MAX = 63
// base64url of the 12-byte IV, the JSON of a longest label and two longest record values, and the 16-byte tag
const SEALED_MAX = 11095
const GCM_IV = 12
const GCM_TAG = 16

function seal(seed: Uint8Array, order: Sealed): string {
  const iv = randomBytes(GCM_IV)
  const cipher = createCipheriv('aes-256-gcm', sealingKey(seed), iv)
  const body = Buffer.concat([cipher.update(JSON.stringify(order)), cipher.final(), cipher.getAuthTag()])
  return Buffer.concat([iv, body]).toString('base64url')
}

function unseal(seed: Uint8Array, sealed: string): Sealed {
  const raw = Buffer.from(sealed, 'base64url')
  const decipher = createDecipheriv('aes-256-gcm', sealingKey(seed), raw.subarray(0, GCM_IV))
  decipher.setAuthTag(raw.subarray(raw.length - GCM_TAG))
  return JSON.parse(Buffer.concat([decipher.update(raw.subarray(GCM_IV, raw.length - GCM_TAG)), decipher.final()]).toString())
}

describe('simplex.domains store, end to end on PR #36', () => {
  it('owner code round-trips, and the abandon…about vector gives #7475 account 0', () => {
    const entropy = randomBytes(ENTROPY_BYTES)
    const code = formatOwnerCode(entropy)
    expect(code).toMatch(/^SN-([0-9A-Z]{5}-){5}[0-9A-Z]{5}$/)
    expect(Buffer.from(parseOwnerCode(code)).equals(entropy)).toBe(true)
    // the mockups' code and its stated entropy
    expect(formatOwnerCode(Buffer.from('6f2d1c9a4b7e3f80d15a2c6e9b04f7a3', 'hex'))).toBe(
      'SN-DWPHS-6JBFR-ZR1MA-T5HQ9-P17QM-F5GM5',
    )
    // and the address the mockups show for it, 0x014f…9b73
    expect(walletFromCode('SN-DWPHS-6JBFR-ZR1MA-T5HQ9-P17QM-F5GM5').account.address).toBe(
      '0x014f9c09CcEeDEF1B713110d04D3a07d5B5D9b73',
    )
    expect(mnemonicToAccount(entropyToMnemonic(Buffer.alloc(ENTROPY_BYTES))).address).toBe(
      '0x9858EfFD232B4033E47d90003D41EC34EcaEda94',
    )
  })

  it('registers to the code wallet with records, then redeems to the app wallet', async () => {
    const f = await deployNamesV2(connection, { owner: owner.address, beneficiary: guardian.address })
    await f.sponsor.write.setRegistrarAllowance([registrar.address, AMPLE_ALLOWANCE], { account: guardian })

    // page: owner code and wallet
    const code = formatOwnerCode(randomBytes(ENTROPY_BYTES))
    const wallet = walletFromCode(code)
    const label = 'dynamis'

    // page: search sends only the hash; the chain agrees it is available
    expect(await f.controller.read.available([label])).toBe(true)

    // page: the contact link as a setText call on the default resolver (a channel would follow it)
    const nameNode = namehash(`${label}.simplex`)
    const contact = 'https://smp8.simplex.im/a#Y8m3qT0vLw2'
    const data = [
      encodeFunctionData({ abi: f.resolver.abi, functionName: 'setText', args: [nameNode, 'simplex.contact', contact] }),
    ]
    const resolver = await f.controller.read.defaultResolver()
    const reg = {
      label,
      owner: wallet.account.address,
      duration: 2n * YEAR,
      secret: commitmentSecret(wallet.seed, label),
      resolver,
      data,
      reverseRecord: 0,
      referrer: zeroHash,
    } as const
    expect(reg.duration).toBe(MIN_REGISTRATION)

    // page: commitment; service: commit from the registrar key
    const commitment = await f.controller.read.makeCommitment([reg])
    await f.controller.write.commit([commitment], { account: registrar })

    // wait minCommitmentAge (1 s in the fixture)
    await connection.networkHelpers.time.increase(2)

    // page at the reveal: the owner's RegisterName intent, signed with the code wallet's key
    const chainId = await publicClient.getChainId()
    const nonce = await f.sponsor.read.nonces([wallet.account.address])
    // revealBefore: the commitment lapses then anyway
    const deadline = (await f.controller.read.commitments([commitment])) + (await f.controller.read.maxCommitmentAge())
    const signature = await wallet.account.signTypedData({
      domain: { name: 'SimplexSponsor', version: '1', chainId, verifyingContract: f.sponsor.address },
      types: eip712Types,
      primaryType: 'RegisterName',
      message: { label, owner: wallet.account.address, duration: reg.duration, resolver, data, nonce, deadline },
    })

    // service: registerWithCredit, dry-run first, from the registrar key; no ETH value
    const allowanceBefore = await f.sponsor.read.registrarAllowance([registrar.address])
    const listPrice = await f.controller.read.rentPriceUSD([label, reg.duration])
    // the sponsored path works while public sales are closed
    expect(await f.controller.read.publicSalesOpen()).toBe(false)
    // the intent signs the duration, so the service cannot register a longer term than the owner signed
    await expect(
      f.sponsor.simulate.registerWithCredit([{ ...reg, duration: 3n * YEAR }, nonce, deadline, signature], { account: registrar.address }),
    ).rejects.toThrow(/InvalidSignature/)
    await f.sponsor.simulate.registerWithCredit([reg, nonce, deadline, signature], { account: registrar.address })
    await f.sponsor.write.registerWithCredit([reg, nonce, deadline, signature], { account: registrar })

    // the name belongs to the code wallet, with its record, and the allowance paid for it
    const tokenId = BigInt(labelhash(label))
    expect(getAddress(await f.baseRegistrar.read.ownerOf([tokenId]))).toBe(wallet.account.address)
    expect(getAddress(await f.ens.read.owner([nameNode]))).toBe(wallet.account.address)
    expect(await f.resolver.read.text([nameNode, 'simplex.contact'])).toBe(contact)
    expect(await f.sponsor.read.registrarAllowance([registrar.address])).toBe(allowanceBefore - listPrice.base - listPrice.premium)
    expect(await f.controller.read.available([label])).toBe(false)

    // a second order for the same name: the service's dry run reverts NameNotAvailable
    const other = privateKeyToAccount(toHex(randomBytes(32)))
    const reg2 = { ...reg, owner: other.address, secret: toHex(randomBytes(32)), data: [] as `0x${string}`[] }
    await f.controller.write.commit([await f.controller.read.makeCommitment([reg2])], { account: registrar })
    await connection.networkHelpers.time.increase(2)
    const sig2 = await other.signTypedData({
      domain: { name: 'SimplexSponsor', version: '1', chainId, verifyingContract: f.sponsor.address },
      types: eip712Types,
      primaryType: 'RegisterName',
      message: { label, owner: other.address, duration: reg2.duration, resolver, data: [], nonce: 0n, deadline },
    })
    await expect(
      f.sponsor.simulate.registerWithCredit([reg2, 0n, deadline, sig2], { account: registrar.address }),
    ).rejects.toThrow(/NameNotAvailable/)
    // with an expired intent the sponsor reverts before the controller sees the name, so the chain's owner and available() tell it is taken
    const past = (await publicClient.getBlock()).timestamp - 1n
    const expiredSig = await other.signTypedData({
      domain: { name: 'SimplexSponsor', version: '1', chainId, verifyingContract: f.sponsor.address },
      types: eip712Types,
      primaryType: 'RegisterName',
      message: { label, owner: other.address, duration: reg2.duration, resolver, data: [], nonce: 0n, deadline: past },
    })
    await expect(
      f.sponsor.simulate.registerWithCredit([reg2, 0n, past, expiredSig], { account: registrar.address }),
    ).rejects.toThrow(/SignatureExpired/)
    expect(getAddress(await f.baseRegistrar.read.ownerOf([BigInt(labelhash(label))]))).not.toBe(other.address)
    expect(await f.controller.read.available([label])).toBe(false)

    // a later link edit: the code wallet signs SetText, the service relays setTextWithSig, paying gas
    const channel = 'https://smp9.simplex.im/c#Rk27xN4pDe9'
    const textNonce = await f.resolver.read.nonces([wallet.account.address])
    const textSig = await wallet.account.signTypedData({
      domain: { name: 'SimplexResolver', version: '1', chainId, verifyingContract: f.resolver.address },
      types: eip712Types, primaryType: 'SetText',
      message: { node: nameNode, key: 'simplex.channel', value: channel, nonce: textNonce, deadline },
    })
    await f.resolver.write.setTextWithSig([nameNode, 'simplex.channel', channel, textNonce, deadline, textSig], { account: registrar })
    expect(await f.resolver.read.text([nameNode, 'simplex.channel'])).toBe(channel)
    // the next edit is signed once the first is mined, with the nonce the chain now has
    const clearNonce = await f.resolver.read.nonces([wallet.account.address])
    expect(clearNonce).toBe(textNonce + 1n)
    const clearSig = await wallet.account.signTypedData({
      domain: { name: 'SimplexResolver', version: '1', chainId, verifyingContract: f.resolver.address },
      types: eip712Types, primaryType: 'ClearRecords',
      message: { node: nameNode, nonce: clearNonce, deadline },
    })
    await f.resolver.write.clearRecordsWithSig([nameNode, clearNonce, deadline, clearSig], { account: registrar })
    expect(await f.resolver.read.text([nameNode, 'simplex.contact'])).toBe('')
    expect(await f.resolver.read.text([nameNode, 'simplex.channel'])).toBe('')

    // app: redeem. The code wallet offers the name to the app wallet, the app wallet claims it;
    // the service relays both, paying gas
    const app = privateKeyToAccount(toHex(randomBytes(32)))
    const names = { name: 'SimplexNames', version: '1', chainId, verifyingContract: f.baseRegistrar.address } as const
    const approveNonce = await f.baseRegistrar.read.nonces([wallet.account.address])
    const approveSig = await wallet.account.signTypedData({
      domain: names, types: eip712Types, primaryType: 'ApproveName',
      message: { from: wallet.account.address, to: app.address, tokenId, ephemeralPubKey: '0x', viewTag: '0x00', nonce: approveNonce, deadline },
    })
    await f.baseRegistrar.write.approveWithSig(
      [wallet.account.address, app.address, tokenId, approveNonce, deadline, approveSig, '0x', '0x00'],
      { account: registrar },
    )
    const offer = await f.baseRegistrar.read.offers([tokenId])
    const claimNonce = await f.baseRegistrar.read.nonces([app.address])
    const claimSig = await app.signTypedData({
      domain: names, types: eip712Types, primaryType: 'ClaimName',
      message: { from: wallet.account.address, to: app.address, tokenId, offer, nonce: claimNonce, deadline },
    })
    await f.baseRegistrar.write.claimWithSig(
      [wallet.account.address, app.address, tokenId, offer, claimNonce, deadline, claimSig],
      { account: registrar },
    )
    expect(getAddress(await f.baseRegistrar.read.ownerOf([tokenId]))).toBe(app.address)
    expect(getAddress(await f.ens.read.owner([nameNode]))).toBe(app.address)
  })

  it('another browser with only the owner code opens the sealed order, rebuilds the commitment and registers', async () => {
    const f = await deployNamesV2(connection, { owner: owner.address, beneficiary: guardian.address })
    await f.sponsor.write.setRegistrarAllowance([registrar.address, AMPLE_ALLOWANCE], { account: guardian })
    const recordsFor = (label: string, tld: string, order: Sealed) => {
      const node = namehash(`${label}.${tld}`)
      const text = (key: string, value: string) =>
        encodeFunctionData({ abi: f.resolver.abi, functionName: 'setText', args: [node, key, value] })
      const records: [string, string][] = [['simplex.contact', order.contact], ['simplex.channel', order.channel]]
      return records.filter(([, value]) => value !== '').map(([key, value]) => text(key, value))
    }
    const registrationFor = (seed: Uint8Array, owner: `0x${string}`, order: Sealed, resolver: `0x${string}`, tld: string, years: bigint) => ({
      label: order.label, owner, duration: years * YEAR, secret: commitmentSecret(seed, order.label),
      resolver, data: recordsFor(order.label, tld, order), reverseRecord: 0, referrer: zeroHash,
    } as const)

    // browser A: checkout builds the commitment from the search answer and seals the label and links
    const search = { resolver: await f.controller.read.defaultResolver(), tld: 'simplex' }
    const years = 2n
    const code = formatOwnerCode(randomBytes(ENTROPY_BYTES))
    const a = walletFromCode(code)
    const order: Sealed = { label: 'lighthouse', contact: ['https://smp8.simplex.im/a#Y8m3qT0vLw2', 'https://smp5.simplex.im/a#Qp4nV7cZ1sE'].join(LINK_SEPARATOR), channel: 'https://smp9.simplex.im/c#Rk27xN4pDe9' }
    const commitment = await f.controller.read.makeCommitment([registrationFor(a.seed, a.account.address, order, search.resolver, search.tld, years)])
    const sealed = seal(a.seed, order)

    // service: commits, then serves the registration view from its row and the chain
    await f.controller.write.commit([commitment], { account: registrar })
    const view = {
      owner: a.account.address,
      commitment,
      sealed,
      years,
      resolver: search.resolver,
      sponsor: f.sponsor.address,
      chainId: await publicClient.getChainId(),
      tld: search.tld,
      intentNonce: await f.sponsor.read.nonces([a.account.address]),
      revealBefore: (await f.controller.read.commitments([commitment])) + (await f.controller.read.maxCommitmentAge()),
    }
    await connection.networkHelpers.time.increase(2)

    // a wrong code cannot open it
    expect(() => unseal(walletFromCode(formatOwnerCode(randomBytes(ENTROPY_BYTES))).seed, view.sealed)).toThrow()

    // browser B: the typed code and the registration view only
    const b = walletFromCode(code.toLowerCase().replace(/-/g, ' '))
    expect(b.account.address).toBe(view.owner)
    const opened = unseal(b.seed, view.sealed)
    const reg = registrationFor(b.seed, b.account.address, opened, view.resolver, view.tld, view.years)
    expect(await f.controller.read.makeCommitment([reg])).toBe(view.commitment)

    const signature = await b.account.signTypedData({
      domain: { name: 'SimplexSponsor', version: '1', chainId: view.chainId, verifyingContract: view.sponsor },
      types: eip712Types, primaryType: 'RegisterName',
      message: { label: reg.label, owner: reg.owner, duration: reg.duration, resolver: view.resolver, data: reg.data, nonce: view.intentNonce, deadline: view.revealBefore },
    })
    await f.sponsor.write.registerWithCredit([reg, view.intentNonce, view.revealBefore, signature], { account: registrar })
    const node = namehash('lighthouse.simplex')
    expect(getAddress(await f.ens.read.owner([node]))).toBe(view.owner)
    expect(await f.resolver.read.text([node, 'simplex.contact'])).toBe(order.contact)
    expect(await f.resolver.read.text([node, 'simplex.channel'])).toBe(order.channel)

    // the service's bound fits the longest order exactly
    const longest: Sealed = { label: 'a'.repeat(LABEL_MAX), contact: 'c'.repeat(RECORD_MAX_BYTES), channel: 'h'.repeat(RECORD_MAX_BYTES) }
    expect(seal(a.seed, longest).length).toBe(SEALED_MAX)
  })

  it('a commitment registers until a second before revealBefore, and commits again only a second after it', async () => {
    const f = await deployNamesV2(connection, { owner: owner.address, beneficiary: guardian.address })
    await f.sponsor.write.setRegistrarAllowance([registrar.address, AMPLE_ALLOWANCE], { account: guardian })
    const resolver = await f.controller.read.defaultResolver()
    const chainId = await publicClient.getChainId()
    const maxAge = await f.controller.read.maxCommitmentAge()
    const order = (label: string) => {
      const w = privateKeyToAccount(toHex(randomBytes(32)))
      const reg = { label, owner: w.address, duration: 2n * YEAR, secret: toHex(randomBytes(32)), resolver, data: [] as `0x${string}`[], reverseRecord: 0, referrer: zeroHash } as const
      return { w, reg }
    }
    const intent = (o: ReturnType<typeof order>, deadline: bigint) =>
      o.w.signTypedData({
        domain: { name: 'SimplexSponsor', version: '1', chainId, verifyingContract: f.sponsor.address },
        types: eip712Types, primaryType: 'RegisterName',
        message: { label: o.reg.label, owner: o.w.address, duration: o.reg.duration, resolver, data: [], nonce: 0n, deadline },
      })

    // `older` is committed a second before `younger`, so younger's last second is older's revealBefore
    const older = order('lapsing')
    const younger = order('justintime')
    const olderCommitment = await f.controller.read.makeCommitment([older.reg])
    await f.controller.write.commit([olderCommitment], { account: registrar })
    const revealBefore = (await f.controller.read.commitments([olderCommitment])) + maxAge
    await connection.networkHelpers.time.setNextBlockTimestamp(revealBefore - maxAge + 1n)
    await f.controller.write.commit([await f.controller.read.makeCommitment([younger.reg])], { account: registrar })
    await expect(f.controller.simulate.commit([olderCommitment], { account: registrar.address })).rejects.toThrow(/UnexpiredCommitmentExists/)

    // at revealBefore the older commitment can neither register nor be committed again; the younger registers
    await connection.networkHelpers.time.setNextBlockTimestamp(revealBefore)
    await expect(
      f.sponsor.write.registerWithCredit([older.reg, 0n, revealBefore, await intent(older, revealBefore)], { account: registrar }),
    ).rejects.toThrow(/CommitmentTooOld/)
    await expect(f.controller.write.commit([olderCommitment], { account: registrar })).rejects.toThrow(/UnexpiredCommitmentExists/)
    await f.sponsor.write.registerWithCredit([younger.reg, 0n, revealBefore + 1n, await intent(younger, revealBefore + 1n)], { account: registrar })
    expect((await publicClient.getBlock()).timestamp).toBe(revealBefore)
    expect(getAddress(await f.baseRegistrar.read.ownerOf([BigInt(labelhash(younger.reg.label))]))).toBe(younger.w.address)

    await connection.networkHelpers.time.setNextBlockTimestamp(revealBefore + 1n)
    await f.controller.write.commit([olderCommitment], { account: registrar })
    expect(await f.controller.read.commitments([olderCommitment])).toBe(revealBefore + 1n)
  })

  it('an intent past its deadline reverts SignatureExpired', async () => {
    const f = await deployNamesV2(connection, { owner: owner.address, beneficiary: guardian.address })
    await f.sponsor.write.setRegistrarAllowance([registrar.address, AMPLE_ALLOWANCE], { account: guardian })
    const w = privateKeyToAccount(toHex(randomBytes(32)))
    const resolver = await f.controller.read.defaultResolver()
    const reg = { label: 'nebula1', owner: w.address, duration: 2n * YEAR, secret: toHex(randomBytes(32)), resolver, data: [] as `0x${string}`[], reverseRecord: 0, referrer: zeroHash } as const
    await f.controller.write.commit([await f.controller.read.makeCommitment([reg])], { account: registrar })
    const now = (await publicClient.getBlock()).timestamp
    const deadline = now + 10n
    const sig = await w.signTypedData({
      domain: { name: 'SimplexSponsor', version: '1', chainId: await publicClient.getChainId(), verifyingContract: f.sponsor.address },
      types: eip712Types, primaryType: 'RegisterName',
      message: { label: reg.label, owner: w.address, duration: reg.duration, resolver, data: [], nonce: 0n, deadline },
    })
    await connection.networkHelpers.time.increase(60)
    await expect(f.sponsor.simulate.registerWithCredit([reg, 0n, deadline, sig], { account: registrar.address })).rejects.toThrow(/SignatureExpired/)
  })

  it('refuses a reverse record, records without a resolver, and a duration under two years', async () => {
    const f = await deployNamesV2(connection, { owner: owner.address, beneficiary: guardian.address })
    const w = privateKeyToAccount(toHex(randomBytes(32)))
    const resolver = await f.controller.read.defaultResolver()
    const base = { label: 'orbital', owner: w.address, duration: 2n * YEAR, secret: toHex(randomBytes(32)), resolver, data: [] as `0x${string}`[], reverseRecord: 0, referrer: zeroHash } as const
    const setText = encodeFunctionData({ abi: f.resolver.abi, functionName: 'setText', args: [namehash('orbital.simplex'), 'simplex.contact', 'x'] })
    await expect(f.controller.read.makeCommitment([{ ...base, reverseRecord: 1 }])).rejects.toThrow(/ReverseRecordNotSupported/)
    await expect(f.controller.read.makeCommitment([{ ...base, resolver: '0x0000000000000000000000000000000000000000', data: [setText] }])).rejects.toThrow(/ResolverRequiredWhenDataSupplied/)
    await expect(f.controller.read.makeCommitment([{ ...base, duration: 2n * YEAR - 1n }])).rejects.toThrow(/DurationTooShort/)
  })
})
