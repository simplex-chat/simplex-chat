// Checks a deployLocal.ts deployment on a running anvil as the store service will use it:
// the service key signs raw transactions over HTTP, and the deploy's real commitment ages apply.
import { randomBytes } from 'node:crypto'
import { readFileSync } from 'node:fs'
import hre from 'hardhat'
import { BaseError, ContractFunctionRevertedError, createWalletClient, decodeErrorResult, defineChain, encodeFunctionData, getAddress, http, labelhash, namehash, toHex, zeroHash } from 'viem'
import { privateKeyToAccount } from 'viem/accounts'
import { describe, expect, it } from 'vitest'

import { eip712Types, YEAR } from './fixtures/namesV2.js'

const deployment = JSON.parse(readFileSync(process.env.LOCAL_CHAIN_OUT || 'local-chain.json', 'utf8'))

// the PR's deploy scripts' settings, which deployLocal.ts copies
const DEPLOY_MIN_COMMITMENT_AGE = 60n
const DEPLOY_MAX_COMMITMENT_AGE = 86400n
const DEPLOY_MAX_LABEL_LENGTH = 63n

const connection = await hre.network.connect('localhost')
const publicClient = await connection.viem.getPublicClient()
const chain = defineChain({
  id: deployment.chainId,
  name: 'local',
  nativeCurrency: { name: 'Ether', symbol: 'ETH', decimals: 18 },
  rpcUrls: { default: { http: [deployment.rpcUrl] } },
})
const service = createWalletClient({ account: privateKeyToAccount(deployment.serviceKey), chain, transport: http(deployment.rpcUrl) })
const asService = { client: { public: publicClient, wallet: service } }
// a contract's simulate does not take the wallet's account
const fromService = { account: service.account.address }
const controller = await connection.viem.getContractAt('SimplexController', deployment.controller, asService)
const baseRegistrar = await connection.viem.getContractAt('BaseRegistrarImplementation', deployment.baseRegistrar, asService)
const sponsor = await connection.viem.getContractAt('SimplexSponsor', deployment.sponsor, asService)
const resolver = await connection.viem.getContractAt('SimplexResolver', deployment.resolver, asService)
// the sponsor passes the controller's and base registrar's reverts through, so decode with their errors too
const revertAbi = [...sponsor.abi, ...controller.abi, ...baseRegistrar.abi].filter((item) => item.type === 'error')

async function revertName(call: Promise<unknown>): Promise<string> {
  try {
    await call
  } catch (e) {
    const reverted = e instanceof BaseError ? e.walk((x) => x instanceof ContractFunctionRevertedError) : null
    if (reverted instanceof ContractFunctionRevertedError && reverted.raw)
      return decodeErrorResult({ abi: revertAbi, data: reverted.raw }).errorName
    throw e
  }
  throw new Error('the call did not revert')
}
const mined = async (hash: `0x${string}`) =>
  expect((await publicClient.waitForTransactionReceipt({ hash })).status).toBe('success')

describe('local deployment of PR #36 for the store service', () => {
  it('is wired as the service config says', async () => {
    expect(await publicClient.getChainId()).toBe(deployment.chainId)
    expect(getAddress(await controller.read.sponsor())).toBe(deployment.sponsor)
    expect(getAddress(await controller.read.defaultResolver())).toBe(deployment.resolver)
    expect(getAddress(await controller.read.beneficiary())).toBe(deployment.guardian)
    expect(getAddress(await sponsor.read.controller())).toBe(deployment.controller)
    expect(await controller.read.minCommitmentAge()).toBe(DEPLOY_MIN_COMMITMENT_AGE)
    expect(await controller.read.maxCommitmentAge()).toBe(DEPLOY_MAX_COMMITMENT_AGE)
    expect(await baseRegistrar.read.maxLabelLength()).toBe(DEPLOY_MAX_LABEL_LENGTH)
    expect(await baseRegistrar.read.controllers([deployment.controller])).toBe(true)
    expect(service.account.address).toBe(deployment.registrar)
    expect(await sponsor.read.registrarAllowance([deployment.registrar])).toBeGreaterThan(0n)
  })

  it('the service commits, waits minCommitmentAge, and registers with the owner intent', async () => {
    const owner = privateKeyToAccount(toHex(randomBytes(32)))
    const label = 'local' + randomBytes(4).toString('hex')
    const node = namehash(`${label}.${deployment.tld}`)
    const contact = 'https://smp8.simplex.im/a#Y8m3qT0vLw2'
    const data = [encodeFunctionData({ abi: resolver.abi, functionName: 'setText', args: [node, 'simplex.contact', contact] })]
    const reg = { label, owner: owner.address, duration: 2n * YEAR, secret: toHex(randomBytes(32)), resolver: deployment.resolver, data, reverseRecord: 0, referrer: zeroHash } as const
    const tooLong = { ...reg, label: 'a'.repeat(deployment.maxLabelLength + 1), data: [] as `0x${string}`[] }

    const commitment = await controller.read.makeCommitment([reg])
    await mined(await controller.write.commit([commitment]))
    await mined(await controller.write.commit([await controller.read.makeCommitment([tooLong])]))

    // revealBefore of the first commitment; the second, mined in the same second or later, lapses no earlier
    const deadline = (await controller.read.commitments([commitment])) + BigInt(deployment.maxCommitmentAge)
    const sign = (r: typeof reg | typeof tooLong, nonce: bigint) =>
      owner.signTypedData({
        domain: { name: 'SimplexSponsor', version: '1', chainId: deployment.chainId, verifyingContract: deployment.sponsor },
        types: eip712Types, primaryType: 'RegisterName',
        message: { label: r.label, owner: r.owner, duration: r.duration, resolver: r.resolver, data: r.data, nonce, deadline },
      })
    const nonce = await sponsor.read.nonces([owner.address])
    const signature = await sign(reg, nonce)
    expect(await revertName(sponsor.simulate.registerWithCredit([reg, nonce, deadline, signature], fromService))).toBe('CommitmentTooNew')

    await connection.networkHelpers.time.increase(deployment.minCommitmentAge + 1)
    const allowanceBefore = await sponsor.read.registrarAllowance([deployment.registrar])
    const price = await controller.read.rentPriceUSD([label, reg.duration])
    await sponsor.simulate.registerWithCredit([reg, nonce, deadline, signature], fromService)
    await mined(await sponsor.write.registerWithCredit([reg, nonce, deadline, signature]))

    expect(getAddress(await baseRegistrar.read.ownerOf([BigInt(labelhash(label))]))).toBe(owner.address)
    expect(await resolver.read.text([node, 'simplex.contact'])).toBe(contact)
    expect(await sponsor.read.registrarAllowance([deployment.registrar])).toBe(allowanceBefore - price.base - price.premium)
    expect(await sponsor.read.nonces([owner.address])).toBe(nonce + 1n)

    const longNonce = await sponsor.read.nonces([owner.address])
    expect(
      await revertName(sponsor.simulate.registerWithCredit([tooLong, longNonce, deadline, await sign(tooLong, longNonce)], fromService)),
    ).toBe('LabelTooLong')
  })
})
