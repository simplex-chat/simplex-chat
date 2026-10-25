#!/usr/bin/env node
const fs = require('fs');
const crypto = require('crypto');
const { setTimeout: sleep } = require('timers/promises');

const PACKAGE_NAME = 'chat.simplex.app';
const API = `https://androidpublisher.googleapis.com/androidpublisher/v3/applications/${PACKAGE_NAME}`;
const TOKEN_URL = 'https://oauth2.googleapis.com/token';
const CSV_FILE = 'play-prices.csv';
const COLUMNS = ['type', 'product_id', 'base_plan_id', 'region', 'currency', 'current_price', 'price'];
const NANOS = 1_000_000_000n;
const MAX_ATTEMPTS = 5;

function parseArgs() {
  const [cmd, keyFile, ...flags] = process.argv.slice(2);
  if (cmd === 'export' && keyFile && (flags.length === 0 || (flags.length === 2 && flags[0] === '-o'))) {
    return { cmd, keyFile, outFile: flags[1] ?? CSV_FILE };
  }
  if (cmd === 'import' && keyFile && flags.every(flag => ['--dry-run', '--migrate-decreases'].includes(flag))) {
    return { cmd, keyFile, dryRun: flags.includes('--dry-run'), migrateDecreases: flags.includes('--migrate-decreases') };
  }
  console.log(`Usage:
  node play-prices.js export <service-account.json> [-o <file>]
  node play-prices.js import <service-account.json> [--dry-run] [--migrate-decreases]
export writes ${CSV_FILE} or <file> and refuses to overwrite an existing file; import reads ${CSV_FILE}.
`);
  process.exit(1);
}

async function getAccessToken(keyFile) {
  const key = JSON.parse(fs.readFileSync(keyFile, 'utf8'));
  const now = Math.floor(Date.now() / 1000);
  const header = { alg: 'RS256', typ: 'JWT', kid: key.private_key_id };
  const claims = {
    iss: key.client_email,
    scope: 'https://www.googleapis.com/auth/androidpublisher',
    aud: TOKEN_URL,
    iat: now,
    exp: now + 3600,
  };
  const unsigned = [header, claims].map(part => Buffer.from(JSON.stringify(part)).toString('base64url')).join('.');
  const signature = crypto.sign('sha256', Buffer.from(unsigned), key.private_key).toString('base64url');
  const res = await fetch(TOKEN_URL, {
    method: 'POST',
    body: new URLSearchParams({
      grant_type: 'urn:ietf:params:oauth:grant-type:jwt-bearer',
      assertion: `${unsigned}.${signature}`,
    }),
  });
  const text = await res.text();
  if (!res.ok) throw new Error(`${res.status} ${TOKEN_URL}: ${text}`);
  return JSON.parse(text).access_token;
}

async function api(token, method, path, body) {
  for (let attempt = 1; ; attempt++) {
    const res = await fetch(`${API}${path}`, {
      method,
      headers: { Authorization: `Bearer ${token}`, 'Content-Type': 'application/json' },
      body: body && JSON.stringify(body),
    });
    const text = await res.text();
    if (res.ok) return text ? JSON.parse(text) : {};
    const retryable = res.status === 429 || res.status >= 500;
    if (!retryable || attempt === MAX_ATTEMPTS) throw new Error(`${res.status} ${method} ${path}: ${text}`);
    const delay = 2 ** attempt * 1000;
    console.warn(`  ${res.status} ${method} ${path}, retrying in ${delay / 1000}s`);
    await sleep(delay);
  }
}

async function listAll(token, collection) {
  const items = [];
  let query = '';
  do {
    const page = await api(token, 'GET', `/${collection}${query}`);
    items.push(...(page[collection] || []));
    query = page.nextPageToken ? `?pageToken=${encodeURIComponent(page.nextPageToken)}` : '';
  } while (query);
  return items;
}

async function fetchRegionPrices(token) {
  const subscriptions = await listAll(token, 'subscriptions');
  const oneTimeProducts = await listAll(token, 'oneTimeProducts');
  return [
    ...subscriptions.flatMap(subscription => {
      const product = {
        type: 'subscription',
        resource: subscription,
        updatePath: `/subscriptions/${subscription.productId}`,
        updateMask: 'basePlans',
      };
      return (subscription.basePlans || []).flatMap(plan => regionPricesOf(product, plan.basePlanId, plan.regionalConfigs));
    }),
    ...oneTimeProducts.flatMap(oneTimeProduct => {
      const product = {
        type: 'onetime',
        resource: oneTimeProduct,
        updatePath: `/onetimeproducts/${oneTimeProduct.productId}`,
        updateMask: 'purchaseOptions',
      };
      return regionPricesOf(product, '', purchaseOption(oneTimeProduct).regionalPricingAndAvailabilityConfigs);
    }),
  ];
}

function purchaseOption(oneTimeProduct) {
  const options = oneTimeProduct.purchaseOptions;
  if (options.length !== 1) {
    throw new Error(`one-time product ${oneTimeProduct.productId} has ${options.length} purchase options, the CSV supports one`);
  }
  return options[0];
}

function regionPricesOf(product, basePlanId, configs = []) {
  return configs
    .filter(config => config.price)
    .map(config => ({
      key: [product.type, product.resource.productId, basePlanId, config.regionCode].join(','),
      product,
      basePlanId,
      config,
    }));
}

function nanosOf(money) {
  return BigInt(money.units ?? 0) * NANOS + BigInt(money.nanos ?? 0);
}

function toMoney(currencyCode, nanos) {
  return { currencyCode, units: String(nanos / NANOS), nanos: Number(nanos % NANOS) };
}

function formatPrice(nanos) {
  const fraction = String(nanos % NANOS).padStart(9, '0').replace(/0+$/, '');
  return fraction ? `${nanos / NANOS}.${fraction}` : `${nanos / NANOS}`;
}

function parsePrice(text, currency) {
  const match = /^(\d+)(?:\.(\d{1,9}))?$/.exec(text);
  if (!match) throw new Error(`price "${text}" is not a decimal number`);
  const nanos = BigInt(match[1]) * NANOS + BigInt((match[2] ?? '').padEnd(9, '0'));
  if (nanos === 0n) throw new Error('price must be positive');
  const digits = new Intl.NumberFormat('en', { style: 'currency', currency }).resolvedOptions().maximumFractionDigits;
  if (nanos % 10n ** BigInt(9 - digits) !== 0n) throw new Error(`price ${text}: ${currency} allows ${digits} fractional digits`);
  return nanos;
}

async function exportPrices(keyFile, outFile) {
  if (fs.existsSync(outFile)) throw new Error(`${outFile} already exists, nothing was written`);
  const token = await getAccessToken(keyFile);
  const rows = (await fetchRegionPrices(token)).map(({ key, config }) => {
    const price = formatPrice(nanosOf(config.price));
    return [key, config.price.currencyCode, price, price].join(',');
  });
  fs.writeFileSync(outFile, [COLUMNS.join(','), ...rows, ''].join('\n'), { flag: 'wx' });
  console.log(`Exported ${rows.length} rows → ${outFile}`);
}

function readCsv() {
  const [header, ...lines] = fs.readFileSync(CSV_FILE, 'utf8').replace(/^\u{FEFF}/u, '').split(/\r?\n/);
  if (header !== COLUMNS.join(',')) throw new Error(`${CSV_FILE}: the header must be ${COLUMNS.join(',')}`);
  return lines.flatMap((text, i) => (text ? [{ line: i + 2, fields: text.split(',') }] : []));
}

function matchRows(regionPrices, rows) {
  const regionPricesByKey = new Map(regionPrices.map(regionPrice => [regionPrice.key, regionPrice]));
  const rowKeys = new Set();
  const changes = [];
  const errors = [];
  for (const { line, fields } of rows) {
    const key = fields.slice(0, 4).join(',');
    try {
      if (rowKeys.has(key)) throw new Error(`duplicate row for ${key}`);
      rowKeys.add(key);
      changes.push(rowChange(key, regionPricesByKey.get(key), fields));
    } catch (e) {
      errors.push(`line ${line}: ${e.message}`);
    }
  }
  for (const key of regionPricesByKey.keys()) {
    if (!rowKeys.has(key)) errors.push(`missing row for ${key}`);
  }
  if (errors.length) throw new Error(`${CSV_FILE} is invalid, nothing was sent:\n  ${errors.join('\n  ')}`);
  return changes;
}

function rowChange(key, regionPrice, fields) {
  if (!regionPrice) throw new Error(`${key} has no price in Google Play`);
  if (fields.length !== COLUMNS.length) throw new Error(`expected ${COLUMNS.length} columns, found ${fields.length}`);
  const currency = fields[4];
  const { currencyCode } = regionPrice.config.price;
  if (currency !== currencyCode) throw new Error(`currency ${currency}, Google Play uses ${currencyCode}`);
  return { ...regionPrice, current: nanosOf(regionPrice.config.price), price: parsePrice(fields[6], currencyCode) };
}

async function fetchRegionsVersion(token) {
  const converted = await api(token, 'POST', '/pricing:convertRegionPrices', { price: { currencyCode: 'USD', units: '1' } });
  return converted.regionVersion.version;
}

function planRequests(changes, regionsVersion, migrateDecreases) {
  return [...Map.groupBy(changes, change => change.product)].flatMap(([product, productChanges]) => {
    const query = new URLSearchParams({ updateMask: product.updateMask, 'regionsVersion.version': regionsVersion });
    const update = {
      label: `update ${product.type} ${product.resource.productId}`,
      changes: productChanges,
      method: 'PATCH',
      path: `${product.updatePath}?${query}`,
      body: product.resource,
    };
    const decreases = productChanges.filter(change => change.price < change.current);
    return migrateDecreases && product.type === 'subscription' && decreases.length
      ? [update, migrationRequest(product.resource.productId, decreases, regionsVersion)]
      : [update];
  });
}

function migrationRequest(productId, decreases, regionsVersion) {
  const oldestAllowedPriceVersionTime = new Date().toISOString();
  return {
    label: `migrate subscription ${productId} subscribers to lower prices`,
    changes: decreases,
    method: 'POST',
    path: `/subscriptions/${productId}/basePlans:batchMigratePrices`,
    body: {
      requests: [...Map.groupBy(decreases, change => change.basePlanId)].map(([basePlanId, planDecreases]) => ({
        packageName: PACKAGE_NAME,
        productId,
        basePlanId,
        regionalPriceMigrations: planDecreases.map(change => ({
          regionCode: change.config.regionCode,
          oldestAllowedPriceVersionTime,
        })),
        regionsVersion: { version: regionsVersion },
      })),
    },
  };
}

function printPlan(requests) {
  for (const { label, changes } of requests) {
    console.log(label);
    for (const change of changes) {
      const currency = change.config.price.currencyCode;
      console.log(`  ${change.key} ${currency} ${formatPrice(change.current)} → ${formatPrice(change.price)}`);
    }
  }
}

async function sendRequests(token, requests) {
  for (const [i, request] of requests.entries()) {
    try {
      await api(token, request.method, request.path, request.body);
    } catch (e) {
      const applied = requests.slice(0, i).map(r => r.label).join('\n  ') || 'none';
      const remaining = requests.slice(i).map(r => r.label).join('\n  ');
      throw new Error(`${request.label} failed: ${e.message}\nApplied:\n  ${applied}\nNot applied:\n  ${remaining}`);
    }
    console.log(`Done: ${request.label}`);
  }
}

async function importPrices(keyFile, dryRun, migrateDecreases) {
  const token = await getAccessToken(keyFile);
  const rows = readCsv();
  const changes = matchRows(await fetchRegionPrices(token), rows).filter(change => change.price !== change.current);
  for (const change of changes) change.config.price = toMoney(change.config.price.currencyCode, change.price);
  const regionsVersion = await fetchRegionsVersion(token);
  const requests = planRequests(changes, regionsVersion, migrateDecreases);
  console.log(`Regions version ${regionsVersion}, ${requests.length} requests${dryRun ? ' (dry run, no updates are sent)' : ''}`);
  printPlan(requests);
  if (dryRun) return;
  await sendRequests(token, requests);
  console.log('Import finished');
}

async function main() {
  const { cmd, keyFile, outFile, dryRun, migrateDecreases } = parseArgs();
  if (cmd === 'export') await exportPrices(keyFile, outFile);
  else await importPrices(keyFile, dryRun, migrateDecreases);
}

main().catch(e => {
  console.error(e.message);
  process.exit(1);
});
