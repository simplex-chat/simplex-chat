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

function parseArgs() {
  const args = process.argv.slice(2);
  const cmd = args[0];
  if (!cmd || !['export', 'import'].includes(cmd)) {
    console.log(`Usage:
  node prices.js export <issuer_id> <path/to/AuthKey_XXXX.p8>
  node prices.js import <issuer_id> <path/to/AuthKey_XXXX.p8> [--start-date YYYY-MM-DD] [--preserve true|false] [--dry-run]
`);
    process.exit(1);
  }

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

  let startDate = null;
  let preserveCurrent = true;
  let dryRun = false;

  for (let i = 3; i < args.length; i++) {
    if (args[i] === '--start-date' && args[i + 1]) startDate = args[++i];
    else if (args[i] === '--preserve' && args[i + 1]) preserveCurrent = args[++i] === 'true';
    else if (args[i] === '--dry-run') dryRun = true;
  }

  return { cmd, issuerId, keyId, p8Path, startDate, preserveCurrent, dryRun };
}

const { cmd, issuerId, keyId, p8Path, startDate, preserveCurrent, dryRun } = parseArgs();
const privateKey = fs.readFileSync(p8Path, 'utf8');

function getToken() {
  return jwt.sign(
    { iss: issuerId, aud: 'appstoreconnect-v1' },
    privateKey,
    { algorithm: 'ES256', expiresIn: '20m', header: { kid: keyId, typ: 'JWT' } }
  );
}

async function api(endpoint, options = {}) {
  const token = getToken();
  const res = await fetch(`https://api.appstoreconnect.apple.com${endpoint}`, {
    ...options,
    headers: {
      Authorization: `Bearer ${token}`,
      'Content-Type': 'application/json',
      ...options.headers,
    },
  });
  const text = await res.text();
  if (!res.ok) {
    throw new Error(`${res.status} ${endpoint}: ${text}`);
  }
  return text ? JSON.parse(text) : {};
}

async function sleep(ms) {
  return new Promise(r => setTimeout(r, ms));
}

// ====================== EXPORT ======================
async function exportPrices() {
  const rows = [];

  // Subscriptions
  const groups = await api(`/v1/apps/${APP_ID}/subscriptionGroups?limit=50`);
  for (const group of groups.data || []) {
    const subs = await api(`/v1/subscriptionGroups/${group.id}/subscriptions?limit=200`);
    for (const sub of subs.data || []) {
      console.log(`Exporting subscription ${sub.attributes.productId}`);
      let next = `/v1/subscriptions/${sub.id}/prices?include=subscriptionPricePoint,territory&limit=200`;
      while (next) {
        const res = await api(next);
        const points = (res.included || []).filter(i => i.type === 'subscriptionPricePoints');
        const territories = (res.included || []).filter(i => i.type === 'territories');

        for (const price of res.data || []) {
          const ppId = price.relationships?.subscriptionPricePoint?.data?.id;
          const terrId = price.relationships?.territory?.data?.id;
          const pp = points.find(p => p.id === ppId);
          const terr = territories.find(t => t.id === terrId);
          if (!pp) continue;

          rows.push({
            type: 'subscription',
            id: sub.id,
            product_id: sub.attributes.productId,
            name: sub.attributes.name || '',
            territory: terrId || '',
            customer_price: pp.attributes.customerPrice || '',
            currency: pp.attributes.currency || terr?.attributes?.currency || '',
            price_point_id: pp.id,
            source: price.attributes?.startDate ? 'scheduled' : 'current',
            desired_price: pp.attributes.customerPrice || '',
          });
        }
        next = res.links?.next || null;
      }
    }
  }

  // IAPs
  const iaps = await api(`/v1/apps/${APP_ID}/inAppPurchasesV2?limit=200`);
  for (const iap of iaps.data || []) {
    console.log(`Exporting IAP ${iap.attributes.productId}`);

    for (const kind of ['manualPrices', 'automaticPrices']) {
      try {
        let next = `/v1/inAppPurchasePriceSchedules/${iap.id}/${kind}?include=inAppPurchasePricePoint,territory&limit=200`;
        while (next) {
          const res = await api(next);
          const points = (res.included || []).filter(i => i.type === 'inAppPurchasePricePoints');
          const territories = (res.included || []).filter(i => i.type === 'territories');

          for (const price of res.data || []) {
            const ppId = price.relationships?.inAppPurchasePricePoint?.data?.id;
            const terrId = price.relationships?.territory?.data?.id;
            const pp = points.find(p => p.id === ppId);
            const terr = territories.find(t => t.id === terrId);
            if (!pp) continue;

            rows.push({
              type: 'iap',
              id: iap.id,
              product_id: iap.attributes.productId,
              name: iap.attributes.name || '',
              territory: terrId || '',
              customer_price: pp.attributes.customerPrice || '',
              currency: pp.attributes.currency || terr?.attributes?.currency || '',
              price_point_id: pp.id,
              source: kind === 'manualPrices' ? 'manual' : 'automatic',
              desired_price: pp.attributes.customerPrice || '',
            });
          }
          next = res.links?.next || null;
        }
      } catch (e) {
        console.warn(`  ${kind} skipped: ${e.message.split('\n')[0]}`);
      }
    }
  }

  fs.writeFileSync('prices.csv', stringify(rows, { header: true }));
  console.log(`\nExported ${rows.length} rows → prices.csv`);
}

// ====================== IMPORT ======================
async function findPricePoint(resourceId, territory, desiredPrice, type) {
  const endpoint = type === 'subscription'
    ? `/v1/subscriptions/${resourceId}/pricePoints?filter[territory]=${territory}&limit=200`
    : `/v2/inAppPurchases/${resourceId}/pricePoints?filter[territory]=${territory}&limit=200`;

  const data = await api(endpoint);
  const match = (data.data || []).find(p => p.attributes.customerPrice === String(desiredPrice));
  return match?.id || null;
}

async function importPrices() {
  const records = parse(fs.readFileSync('prices.csv', 'utf8'), {
    columns: true,
    skip_empty_lines: true,
  });

  const changes = records.filter(r => r.desired_price && r.desired_price !== r.customer_price);
  console.log(`Found ${changes.length} price changes (dry-run=${dryRun})`);

  // Group IAP changes by product
  const iapGroups = {};
  for (const row of changes) {
    if (row.type === 'iap') {
      if (!iapGroups[row.id]) iapGroups[row.id] = [];
      iapGroups[row.id].push(row);
    }
  }

  // Subscriptions (one territory at a time)
  for (const row of changes.filter(r => r.type === 'subscription')) {
    console.log(`[SUB] ${row.product_id} ${row.territory}: ${row.customer_price} → ${row.desired_price}`);
    const ppId = await findPricePoint(row.id, row.territory, row.desired_price, 'subscription');
    if (!ppId) {
      console.warn('  No exact price point found');
      continue;
    }
    if (dryRun) continue;

    const attributes = { preserveCurrentPrice: preserveCurrent };
    if (startDate) attributes.startDate = startDate;

    await api('/v1/subscriptionPrices', {
      method: 'POST',
      body: JSON.stringify({
        data: {
          type: 'subscriptionPrices',
          attributes,
          relationships: {
            subscription: { data: { type: 'subscriptions', id: row.id } },
            subscriptionPricePoint: { data: { type: 'subscriptionPricePoints', id: ppId } },
          },
        },
      }),
    });
    await sleep(300);
  }

  // IAPs (one schedule per product)
  for (const [iapId, rows] of Object.entries(iapGroups)) {
    const productId = rows[0].product_id;
    console.log(`[IAP] ${productId} — ${rows.length} territories`);

    const manualPrices = [];
    const included = [];
    let baseTerritory = 'USA';

    for (const row of rows) {
      const ppId = await findPricePoint(iapId, row.territory, row.desired_price, 'iap');
      if (!ppId) {
        console.warn(`  No price point for ${row.territory} ${row.desired_price}`);
        continue;
      }
      const tempId = `\${price-${row.territory}}`;
      manualPrices.push({ type: 'inAppPurchasePrices', id: tempId });
      included.push({
        type: 'inAppPurchasePrices',
        id: tempId,
        attributes: { startDate: startDate || null, endDate: null },
        relationships: {
          inAppPurchaseV2: { data: { type: 'inAppPurchases', id: iapId } },
          inAppPurchasePricePoint: { data: { type: 'inAppPurchasePricePoints', id: ppId } },
        },
      });
      if (row.territory === 'USA') baseTerritory = 'USA';
    }

    if (manualPrices.length === 0) continue;

    const payload = {
      data: {
        type: 'inAppPurchasePriceSchedules',
        relationships: {
          inAppPurchase: { data: { type: 'inAppPurchases', id: iapId } },
          baseTerritory: { data: { type: 'territories', id: baseTerritory } },
          manualPrices: { data: manualPrices },
        },
      },
      included,
    };

    if (dryRun) {
      console.log('  (dry-run) would POST schedule');
      continue;
    }

    await api('/v1/inAppPurchasePriceSchedules', {
      method: 'POST',
      body: JSON.stringify(payload),
    });
    await sleep(500);
  }

  console.log('Import finished');
}

// ====================== RUN ======================
if (cmd === 'export') exportPrices().catch(console.error);
else importPrices().catch(console.error);
