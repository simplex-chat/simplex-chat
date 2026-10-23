#!/usr/bin/env node
const fs = require('fs');
const path = require('path');
const jwt = require('jsonwebtoken');
const fetch = require('node-fetch');
const { parse } = require('csv-parse/sync');
const { stringify } = require('csv-stringify/sync');

// ============ HARDCODE THIS ============
const APP_ID = '1605771084'; // your numeric app ID
// =======================================
const CSV_FILE = 'apple-prices.csv';
const POINTS_FILE = 'apple-price-points.csv';

function usage() {
  console.log(`Usage:
  node apple-prices.js export <issuer_id> <path/to/AuthKey_XXXX.p8> [-o <file>]
  node apple-prices.js points <issuer_id> <path/to/AuthKey_XXXX.p8> [-o <file>]
  node apple-prices.js import <issuer_id> <path/to/AuthKey_XXXX.p8> [--start-date YYYY-MM-DD] [--preserve true|false] [--dry-run]

export writes ${CSV_FILE} and points writes ${POINTS_FILE} unless -o is given; neither overwrites an existing file.
import reads ${CSV_FILE} and changes the prices that differ from the current App Store prices.
`);
  process.exit(1);
}

function parseArgs() {
  const args = process.argv.slice(2);
  const cmd = args[0];
  if (!cmd || !['export', 'points', 'import'].includes(cmd)) usage();

  const issuerId = args[1];
  const p8Path = args[2];
  if (!issuerId || !p8Path) {
    console.error('Missing issuer_id or key file');
    process.exit(1);
  }

  const basename = path.basename(p8Path, '.p8');
  const keyId = basename.replace(/^AuthKey_/, '');
  if (!keyId || keyId === basename) {
    console.error('Key file must be named AuthKey_XXXXXXXX.p8');
    process.exit(1);
  }

  let output = { export: CSV_FILE, points: POINTS_FILE }[cmd];
  let startDate = null;
  let preserveCurrent = true;
  let dryRun = false;

  for (let i = 3; i < args.length; i++) {
    if (args[i] === '-o' && cmd !== 'import' && args[i + 1]) output = args[++i];
    else if (args[i] === '--start-date' && /^\d{4}-\d{2}-\d{2}$/.test(args[i + 1])) startDate = args[++i];
    else if (args[i] === '--preserve' && ['true', 'false'].includes(args[i + 1])) preserveCurrent = args[++i] === 'true';
    else if (args[i] === '--dry-run') dryRun = true;
    else usage();
  }

  return { cmd, issuerId, keyId, p8Path, output, startDate, preserveCurrent, dryRun };
}

const { cmd, issuerId, keyId, p8Path, output, startDate, preserveCurrent, dryRun } = parseArgs();
const privateKey = fs.readFileSync(p8Path, 'utf8');
const today = new Date().toISOString().slice(0, 10);
const RATE_LIMIT_RETRIES = 6;

function getToken() {
  return jwt.sign(
    { iss: issuerId, aud: 'appstoreconnect-v1' },
    privateKey,
    { algorithm: 'ES256', expiresIn: '5m', header: { kid: keyId, typ: 'JWT' } }
  );
}

async function api(endpoint, options = {}, retry = 0) {
  const url = new URL(endpoint, 'https://api.appstoreconnect.apple.com').href;
  const res = await fetch(url, {
    ...options,
    headers: {
      Authorization: `Bearer ${getToken()}`,
      Accept: 'application/json',
      'Content-Type': 'application/json',
      ...options.headers,
    },
  });
  const text = await res.text();
  if (res.status === 429 && retry < RATE_LIMIT_RETRIES) {
    const minutes = 2 ** retry;
    console.warn(`Rate limit exceeded, retrying in ${minutes} min: ${url}`);
    await sleep(minutes * 60 * 1000);
    return api(endpoint, options, retry + 1);
  }
  if (!res.ok) {
    throw new Error(`${res.status} ${url}: ${text}`);
  }
  return text ? JSON.parse(text) : {};
}

async function apiList(endpoint) {
  const data = [];
  const included = [];
  let next = endpoint;
  while (next) {
    const res = await api(next);
    data.push(...res.data);
    included.push(...(res.included ?? []));
    next = res.links?.next;
  }
  return { data, included };
}

async function sleep(ms) {
  return new Promise(r => setTimeout(r, ms));
}

// ====================== EXPORT ======================
async function readProducts() {
  const products = [];

  const groups = await apiList(`/v1/apps/${APP_ID}/subscriptionGroups?limit=200`);
  for (const group of groups.data) {
    const subs = await apiList(`/v1/subscriptionGroups/${group.id}/subscriptions?limit=200`);
    for (const sub of subs.data) {
      console.log(`Reading subscription ${sub.attributes.productId}`);
      products.push({
        type: 'subscription',
        id: sub.id,
        productId: sub.attributes.productId,
        pricePointsPath: `/v1/subscriptions/${sub.id}/pricePoints`,
        prices: await readSubscriptionPrices(sub),
      });
    }
  }

  const iaps = await apiList(`/v1/apps/${APP_ID}/inAppPurchasesV2?limit=200`);
  for (const iap of iaps.data) {
    console.log(`Reading IAP ${iap.attributes.productId}`);
    products.push({
      type: 'iap',
      id: iap.id,
      productId: iap.attributes.productId,
      pricePointsPath: `/v2/inAppPurchases/${iap.id}/pricePoints`,
      prices: await readIapPrices(iap),
    });
  }

  return products;
}

async function readSubscriptionPrices(sub) {
  const res = await apiList(`/v1/subscriptions/${sub.id}/prices?include=subscriptionPricePoint,territory&limit=200`);
  const records = priceRecords(res, 'subscriptionPricePoint');
  const monthly = records
    .filter(r => r.planType === 'MONTHLY')
    .map(r => `  ${r.territory} ${r.startDate ?? '-'} ${r.point.customerPrice} ${r.currency}`);
  if (monthly.length > 0) {
    throw new Error(`${sub.attributes.productId} has MONTHLY plan prices:\n${monthly.join('\n')}`);
  }
  return territoryPrices(sub.attributes.productId, records);
}

async function readIapPrices(iap) {
  const records = [];
  for (const kind of ['manualPrices', 'automaticPrices']) {
    const res = await apiList(`/v1/inAppPurchasePriceSchedules/${iap.id}/${kind}?include=inAppPurchasePricePoint,territory&limit=200`);
    records.push(...priceRecords(res, 'inAppPurchasePricePoint'));
  }
  return territoryPrices(iap.attributes.productId, records);
}

function priceRecords({ data, included }, pricePointRelationship) {
  const find = ({ type, id }) => included.find(i => i.type === type && i.id === id);
  return data.map(({ attributes, relationships }) => {
    const territory = find(relationships.territory.data);
    const point = find(relationships[pricePointRelationship].data);
    return {
      territory: territory.id,
      currency: territory.attributes.currency,
      startDate: attributes.startDate,
      endDate: attributes.endDate,
      planType: attributes.planType,
      point: { id: point.id, customerPrice: point.attributes.customerPrice },
    };
  });
}

function territoryPrices(productId, records) {
  const territories = [...new Set(records.map(r => r.territory))];
  return territories.map(territory => {
    const history = records
      .filter(r => r.territory === territory)
      .sort((a, b) => (a.startDate ?? '').localeCompare(b.startDate ?? ''));
    const current = history.findLast(r => (r.startDate ?? '') <= today && (!r.endDate || r.endDate > today));
    if (!current) {
      throw new Error(`${productId} ${territory}: no current price`);
    }
    return {
      territory,
      current,
      scheduled: history.find(r => (r.startDate ?? '') > today),
    };
  });
}

async function readPricePoints(product, territory) {
  const { data } = await apiList(`${product.pricePointsPath}?filter[territory]=${territory}&limit=8000`);
  return data
    .map(p => ({ id: p.id, customerPrice: p.attributes.customerPrice }))
    .sort((a, b) => Number(a.customerPrice) - Number(b.customerPrice));
}

async function exportPrices() {
  if (fs.existsSync(output)) throw new Error(`${output} already exists`);
  const products = await readProducts();
  const rows = products.flatMap(product => product.prices.map(({ territory, current, scheduled }) => ({
    type: product.type,
    product_id: product.productId,
    territory,
    currency: current.currency,
    current_price: current.point.customerPrice,
    scheduled_price: scheduled?.point.customerPrice ?? '',
    scheduled_date: scheduled?.startDate ?? '',
    price: current.point.customerPrice,
  })));

  fs.writeFileSync(output, stringify(rows, { header: true }), { flag: 'wx' });
  console.log(`\nExported ${rows.length} rows → ${output}`);
}

async function exportPricePoints() {
  if (fs.existsSync(output)) throw new Error(`${output} already exists`);
  const products = await readProducts();
  const rows = [];
  for (const type of ['subscription', 'iap']) {
    const product = products.find(p => p.type === type);
    console.log(`Reading price points of ${type} ${product.productId}`);
    for (const { territory, current } of product.prices) {
      for (const point of await readPricePoints(product, territory)) {
        rows.push({ type, territory, currency: current.currency, customer_price: point.customerPrice });
      }
    }
  }

  fs.writeFileSync(output, stringify(rows, { header: true }), { flag: 'wx' });
  console.log(`\nExported ${rows.length} price points → ${output}`);
}

// ====================== IMPORT ======================
async function findPricePoint(product, territory, value) {
  if (Number.isNaN(value)) return {};
  const points = await readPricePoints(product, territory);
  return {
    point: points.find(p => Number(p.customerPrice) === value),
    lower: points.findLast(p => Number(p.customerPrice) < value),
    higher: points.find(p => Number(p.customerPrice) > value),
  };
}

async function matchRows(products, rows) {
  const problems = [];
  const rowsByKey = new Map();
  for (const row of rows) {
    const key = `${row.product_id} ${row.territory}`;
    if (rowsByKey.has(key)) problems.push(`${key}: duplicate row`);
    rowsByKey.set(key, row);
  }

  const lines = [];
  for (const product of products) {
    console.log(`Checking prices of ${product.type} ${product.productId}`);
    for (const price of product.prices) {
      const key = `${product.productId} ${price.territory}`;
      const row = rowsByKey.get(key);
      rowsByKey.delete(key);
      if (!row) {
        problems.push(`${key}: missing row`);
        continue;
      }
      const value = Number(row.price);
      const changed = value !== Number(price.current.point.customerPrice);
      const { point, lower, higher } = changed
        ? await findPricePoint(product, price.territory, value)
        : { point: price.current.point };
      if (!point) {
        problems.push(`${key}: "${row.price}" ${price.current.currency} is not a price point, nearest are ${lower?.customerPrice ?? 'none'} and ${higher?.customerPrice ?? 'none'}`);
        continue;
      }
      lines.push({
        product,
        territory: price.territory,
        currency: price.current.currency,
        current: price.current.point,
        point,
        changed,
      });
    }
  }
  for (const key of rowsByKey.keys()) problems.push(`${key}: not an App Store product territory`);

  if (problems.length > 0) {
    throw new Error(`${problems.length} invalid rows, nothing was changed:\n${problems.join('\n')}`);
  }
  return lines;
}

function describeLine({ territory, currency, current, point, changed }) {
  return changed
    ? `${territory} ${current.customerPrice} -> ${point.customerPrice} ${currency}`
    : `${territory} ${point.customerPrice} ${currency}`;
}

function subscriptionChanges(lines) {
  return lines
    .filter(line => line.product.type === 'subscription' && line.changed)
    .map(line => {
      const increase = Number(line.point.customerPrice) > Number(line.current.customerPrice);
      return {
        label: `subscription ${line.product.productId} ${describeLine(line)}${increase ? ', increase' : ''}`,
        details: [],
        path: '/v1/subscriptionPrices',
        body: {
          data: {
            type: 'subscriptionPrices',
            attributes: increase ? { startDate, preserveCurrentPrice: preserveCurrent } : { startDate },
            relationships: {
              subscription: { data: { type: 'subscriptions', id: line.product.id } },
              subscriptionPricePoint: { data: { type: 'subscriptionPricePoints', id: line.point.id } },
            },
          },
        },
      };
    });
}

function manualPrice(product, point, from, until) {
  return {
    type: 'inAppPurchasePrices',
    attributes: { startDate: from, endDate: until },
    relationships: {
      inAppPurchaseV2: { data: { type: 'inAppPurchases', id: product.id } },
      inAppPurchasePricePoint: { data: { type: 'inAppPurchasePricePoints', id: point.id } },
    },
  };
}

async function iapScheduleChanges(products, lines) {
  const changes = [];
  for (const product of products.filter(p => p.type === 'iap')) {
    const productLines = lines.filter(line => line.product === product);
    if (!productLines.some(line => line.changed)) continue;

    const baseTerritory = (await api(`/v1/inAppPurchasePriceSchedules/${product.id}/baseTerritory`)).data.id;
    const manualPrices = productLines
      .flatMap(line => startDate && line.changed
        ? [manualPrice(product, line.current, null, startDate), manualPrice(product, line.point, startDate, null)]
        : [manualPrice(product, line.point, null, null)])
      .map((price, i) => ({ ...price, id: `\${price-${i}}` }));

    changes.push({
      label: `iap ${product.productId} price schedule, base territory ${baseTerritory}, ${productLines.length} territories`,
      details: productLines.map(describeLine),
      path: '/v1/inAppPurchasePriceSchedules',
      body: {
        data: {
          type: 'inAppPurchasePriceSchedules',
          relationships: {
            inAppPurchase: { data: { type: 'inAppPurchases', id: product.id } },
            baseTerritory: { data: { type: 'territories', id: baseTerritory } },
            manualPrices: { data: manualPrices.map(({ type, id }) => ({ type, id })) },
          },
        },
        included: manualPrices,
      },
    });
  }
  return changes;
}

function printChange({ label, details }) {
  console.log(label);
  for (const detail of details) console.log(`  ${detail}`);
}

async function applyChanges(changes) {
  for (const [i, change] of changes.entries()) {
    try {
      await api(change.path, { method: 'POST', body: JSON.stringify(change.body) });
    } catch (e) {
      throw new Error([
        `Failed: ${change.label}`,
        e.message,
        `Applied ${i} changes:`,
        ...changes.slice(0, i).map(c => `  ${c.label}`),
        `Not applied ${changes.length - i} changes:`,
        ...changes.slice(i).map(c => `  ${c.label}`),
      ].join('\n'));
    }
    printChange(change);
    await sleep(300);
  }
}

async function importPrices() {
  const rows = parse(fs.readFileSync(CSV_FILE, 'utf8'), {
    columns: true,
    skip_empty_lines: true,
    bom: true,
  });
  const products = await readProducts();
  const lines = await matchRows(products, rows);
  const changes = [
    ...await iapScheduleChanges(products, lines),
    ...subscriptionChanges(lines),
  ];

  console.log(`\n${changes.length} changes, start date ${startDate ?? 'immediate'}, preserve current price on increases ${preserveCurrent} (dry-run=${dryRun})`);
  if (dryRun) {
    changes.forEach(printChange);
    return;
  }
  await applyChanges(changes);
  console.log('Import finished');
}

// ====================== RUN ======================
const commands = { export: exportPrices, points: exportPricePoints, import: importPrices };
commands[cmd]().catch(e => {
  console.error(e);
  process.exitCode = 1;
});
