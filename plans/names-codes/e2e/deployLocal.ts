// Deploys PR #36's .simplex stack to a running anvil for the store service to use,
// wired as fixtures/namesV2.ts wires it, with the commitment ages and label cap of deploy/simplex/00 and 02.
import { writeFileSync } from 'node:fs'
import hre from 'hardhat'
import { encodeFunctionData, getAddress, isAddressEqual, labelhash, namehash, zeroAddress, zeroHash } from 'viem'
import { privateKeyToAccount } from 'viem/accounts'

import { AMPLE_ALLOWANCE, PRICE_BASE, PRICE_EXCEPTIONS } from './fixtures/namesV2.js'

const TLD = process.env.SIMPLEX_TLD || 'simplex'
const MIN_COMMITMENT_AGE = 60n
const MAX_COMMITMENT_AGE = 86400n
const MIN_CHAR_LENGTH = 6
// the DNS label limit in bytes
const MAX_LABEL_LENGTH = 63n
// Chainlink's 8 decimals; 1e8 makes 1 attoUSD cost 1 wei, as in the fixture
const FEED_DECIMALS = 8
const FEED_PRICE = 100000000n
// account 2 of anvil's well-known development mnemonic, so the service can sign with it locally
const REGISTRAR_KEY = '0x5de4111afa1a4b94908f83103eb1f1706367c2e68ca870fc3fb9a804cdab365a'
const OUT = process.env.LOCAL_CHAIN_OUT || 'local-chain.json'

const connection = await hre.network.connect()
const viem = connection.viem
const [ownerClient, guardianClient, registrarClient] = await viem.getWalletClients()
const owner = ownerClient.account
const guardian = guardianClient.account
const registrar = registrarClient.account
if (!isAddressEqual(registrar.address, privateKeyToAccount(REGISTRAR_KEY).address))
  throw new Error(`account 2 is ${registrar.address}, not anvil's default`)
const publicClient = await viem.getPublicClient()
const tldNode = namehash(TLD)

const ens = await viem.deployContract('ENSRegistry', [])
const baseRegistrar = await viem.deployContract('BaseRegistrarImplementation', [ens.address, tldNode])
await ens.write.setSubnodeOwner([zeroHash, labelhash(TLD), baseRegistrar.address])
await baseRegistrar.write.setMaxLabelLength([MAX_LABEL_LENGTH])

const feed = await viem.deployContract('DummyOracle', [FEED_PRICE])
const priceOracle = await viem.deployContract('SimplexPriceOracle', [feed.address, FEED_DECIMALS, PRICE_BASE, PRICE_EXCEPTIONS])

const implementation = await viem.deployContract('SimplexController', [])
const initData = encodeFunctionData({
  abi: implementation.abi,
  functionName: 'initialize',
  args: [
    baseRegistrar.address,
    priceOracle.address,
    MIN_COMMITMENT_AGE,
    MAX_COMMITMENT_AGE,
    ens.address,
    { tldNode, tldSuffix: `.${TLD}`, minCharLength: MIN_CHAR_LENGTH, smpxNft: zeroAddress, nftGateEnabled: false },
    owner.address,
  ],
})
const proxy = await viem.deployContract('SimplexControllerProxy', [implementation.address, initData])
const controller = await viem.getContractAt('SimplexController', proxy.address)

const subnameRegistrar = await viem.deployContract('SubnameRegistrar', [ens.address, baseRegistrar.address])
const resolver = await viem.deployContract('SimplexResolver', [ens.address, subnameRegistrar.address, controller.address, zeroAddress])
await subnameRegistrar.write.setResolver([resolver.address])

await baseRegistrar.write.addController([controller.address])
await baseRegistrar.write.setSubnameHook([subnameRegistrar.address])
await controller.write.setDefaultResolver([resolver.address], { account: owner })
await controller.write.setBeneficiary([guardian.address], { account: owner })

const sponsor = await viem.deployContract('SimplexSponsor', [controller.address])
await controller.write.setSponsor([sponsor.address], { account: owner })
await sponsor.write.setRegistrarAllowance([registrar.address, AMPLE_ALLOWANCE], { account: guardian })

const chainId = await publicClient.getChainId()
const deployment = {
  rpcUrl: 'http://127.0.0.1:8545',
  chainId,
  tld: TLD,
  controller: getAddress(controller.address),
  sponsor: getAddress(sponsor.address),
  baseRegistrar: getAddress(baseRegistrar.address),
  resolver: getAddress(resolver.address),
  ens: getAddress(ens.address),
  owner: getAddress(owner.address),
  guardian: getAddress(guardian.address),
  registrar: getAddress(registrar.address),
  serviceKey: REGISTRAR_KEY,
  maxLabelLength: Number(await baseRegistrar.read.maxLabelLength()),
  minCommitmentAge: Number(await controller.read.minCommitmentAge()),
  maxCommitmentAge: Number(await controller.read.maxCommitmentAge()),
}
writeFileSync(OUT, JSON.stringify(deployment, null, 2) + '\n')

console.log(`wrote ${OUT}\n
; with [names], a run outside the Docker image sets [listener] names_static_dir to the names build
[names]
tld = ${TLD}
terms_version = 2026-10-23
terms_url = https://simplex.chat/

[chain]
rpc_url = ${deployment.rpcUrl}
chain_id = ${chainId}
controller = ${deployment.controller}
sponsor = ${deployment.sponsor}
base_registrar = ${deployment.baseRegistrar}
resolver = ${deployment.resolver}
service_key = ${REGISTRAR_KEY}
; 1 for the local chain only; 3 in production
confirmations = 1`)
