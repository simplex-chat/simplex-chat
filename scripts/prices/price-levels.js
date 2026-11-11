#!/usr/bin/env node
const fs = require('fs');

const BIG_MAC_URL = 'https://raw.githubusercontent.com/TheEconomist/big-mac-data/master/output-data/big-mac-raw-index.csv';
const EURO_AREA = ['AUT', 'BEL', 'BGR', 'CYP', 'DEU', 'ESP', 'EST', 'FIN', 'FRA', 'GRC', 'HRV', 'IRL', 'ITA', 'LTU', 'LUX', 'LVA', 'MLT', 'NLD', 'PRT', 'SVK', 'SVN'];
const APPLE_TERRITORIES = { XKX: 'XKS' };
const ISO2 = { TWN: 'TW' };
const COLUMNS = ['name', 'iso2', 'iso3', 'apple_territory', 'google_region', 'price_level', 'price_level_year', 'ppp_lcu_per_usd', 'atls_lcu_per_usd', 'big_mac_ratio', 'big_mac_date', 'big_mac_source'];

async function download(url) {
  const res = await fetch(url);
  if (!res.ok) throw new Error(`${res.status} ${url}`);
  return res;
}

async function worldBank(path, page = 1) {
  const res = await download(`https://api.worldbank.org/v2/${path}?format=json&per_page=20000&page=${page}`);
  const [header, records] = await res.json();
  if (!records) throw new Error(`World Bank ${path}: ${JSON.stringify(header)}`);
  return page < header.pages ? records.concat(await worldBank(path, page + 1)) : records;
}

async function economies() {
  const countries = await worldBank('country');
  return countries
    .filter(country => country.region.id !== 'NA')
    .map(country => ({ name: country.name, iso2: country.iso2Code, iso3: country.id }));
}

async function indicator(id) {
  const records = await worldBank(`country/all/indicator/${id}`);
  return records.filter(record => record.value !== null);
}

async function priceLevels() {
  const ppp = await indicator('PA.NUS.PPP');
  const rates = await indicator('PA.NUS.ATLS');
  const rateOf = new Map(rates.map(rate => [`${rate.countryiso3code} ${rate.date}`, rate.value]));
  const levels = new Map();
  for (const { countryiso3code, date, value } of ppp.sort((a, b) => a.date.localeCompare(b.date))) {
    const rate = rateOf.get(`${countryiso3code} ${date}`);
    if (rate) levels.set(countryiso3code, { value: value / rate, year: date, ppp: value, atls: rate });
  }
  return levels;
}

async function bigMacRatios() {
  const res = await download(BIG_MAC_URL);
  const [header, ...lines] = (await res.text()).trim().split('\n');
  const columns = header.split(',');
  const entries = lines.map(line => Object.fromEntries(line.split(',').map((field, i) => [columns[i], field])));
  const date = entries.map(entry => entry.date).sort().at(-1);
  const latest = entries.filter(entry => entry.date === date);
  const usaPrice = latest.find(entry => entry.iso_a3 === 'USA').dollar_price;
  return new Map(latest.map(entry => [entry.iso_a3, { name: entry.name, ratio: entry.dollar_price / usaPrice, date }]));
}

function bigMacOf(iso3, bigMacs) {
  if (bigMacs.has(iso3)) return { ...bigMacs.get(iso3), source: 'own' };
  if (EURO_AREA.includes(iso3)) return { ...bigMacs.get('EUZ'), source: 'EUZ' };
  return {};
}

function priceLevelRow({ name, iso2, iso3 }, levels, bigMacs) {
  const level = levels.get(iso3) ?? {};
  const bigMac = bigMacOf(iso3, bigMacs);
  return {
    name,
    iso2,
    iso3,
    apple_territory: APPLE_TERRITORIES[iso3] ?? iso3,
    google_region: iso2,
    price_level: level.value,
    price_level_year: level.year,
    ppp_lcu_per_usd: level.ppp,
    atls_lcu_per_usd: level.atls,
    big_mac_ratio: bigMac.ratio,
    big_mac_date: bigMac.date,
    big_mac_source: bigMac.source,
  };
}

function csvField(value) {
  const text = String(value ?? '');
  return /[",\n]/.test(text) ? `"${text.replaceAll('"', '""')}"` : text;
}

function csv(rows) {
  return [COLUMNS, ...rows.map(row => COLUMNS.map(column => row[column]))]
    .map(fields => fields.map(csvField).join(','))
    .join('\n') + '\n';
}

async function main() {
  const output = process.argv[2] ?? 'price-levels.csv';
  const [worldBankEconomies, levels, bigMacs] = await Promise.all([economies(), priceLevels(), bigMacRatios()]);
  const worldBankIso3 = new Set(worldBankEconomies.map(economy => economy.iso3));
  const bigMacOnly = [...bigMacs]
    .filter(([iso3]) => iso3 !== 'EUZ' && !worldBankIso3.has(iso3))
    .map(([iso3, { name }]) => ({ name, iso2: ISO2[iso3], iso3 }));
  const rows = [...worldBankEconomies, ...bigMacOnly]
    .sort((a, b) => a.iso3.localeCompare(b.iso3))
    .map(economy => priceLevelRow(economy, levels, bigMacs));
  fs.writeFileSync(output, csv(rows));
  console.log(`Wrote ${rows.length} rows to ${output}`);
}

main().catch(e => {
  console.error(e);
  process.exitCode = 1;
});
