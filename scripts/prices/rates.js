#!/usr/bin/env node
const fs = require('fs');
const { parseArgs } = require('util');

const RATES_URL = 'https://api.frankfurter.dev/v2/rates';

const USAGE = `Usage:
  node rates.js fetch [output.csv]
  node rates.js compare <baseline.csv> <current.csv> [--threshold 20] [--currencies <file.csv>]...`;

async function fetchRates(file) {
  const url = `${RATES_URL}?base=USD`;
  const res = await fetch(url);
  if (!res.ok) throw new Error(`${res.status} ${url}: ${await res.text()}`);
  const rates = (await res.json()).filter(({ quote }) => quote !== 'USD');
  const usd = { quote: 'USD', rate: 1, date: new Date().toISOString().slice(0, 10) };
  const rows = [usd, ...rates]
    .map(({ quote, rate, date }) => [quote, rate, date, RATES_URL])
    .sort(([a], [b]) => a.localeCompare(b));
  const lines = [['currency', 'units_per_usd', 'rate_date', 'source'], ...rows].map(row => row.join(','));
  fs.writeFileSync(file, lines.join('\n') + '\n');
  console.log(`Wrote ${rows.length} rows to ${file}`);
}

function readColumns(file, names) {
  const [header, ...rows] = fs.readFileSync(file, 'utf8').trim().split(/\r?\n/).map(line => line.split(','));
  const columns = names.map(name => header.indexOf(name));
  if (columns.includes(-1)) throw new Error(`${file}: header must include ${names.join(' and ')}`);
  return rows.map(row => columns.map(column => row[column]));
}

function readRates(file) {
  return new Map(readColumns(file, ['currency', 'units_per_usd']).map(([currency, units]) => {
    const unitsPerUsd = Number(units);
    if (!(unitsPerUsd > 0)) throw new Error(`${file}: invalid units_per_usd for ${currency}`);
    return [currency, unitsPerUsd];
  }));
}

function readCurrencies(file) {
  return readColumns(file, ['currency']).map(([currency]) => currency);
}

function compareRates(baselineFile, currentFile, threshold, currencyFiles) {
  const baseline = readRates(baselineFile);
  const current = readRates(currentFile);
  const currencies = currencyFiles.length > 0 ? currencyFiles.flatMap(file => readCurrencies(file)) : [...baseline.keys()];
  const compared = [...new Set(currencies)].sort();
  const changes = compared
    .filter(currency => baseline.has(currency) && current.has(currency))
    .map(currency => {
      const before = baseline.get(currency);
      const after = current.get(currency);
      return { currency, before, after, percent: (after / before - 1) * 100 };
    })
    .filter(({ percent }) => Math.abs(percent) > threshold);
  if (changes.length > 0) {
    console.log('currency,baseline,current,change_percent');
    changes.forEach(c => console.log(`${c.currency},${c.before},${c.after},${c.percent.toFixed(1)}`));
  }
  const missingFromCurrent = compared.filter(currency => !current.has(currency));
  const missingFromBaseline = compared.filter(currency => !baseline.has(currency));
  if (missingFromCurrent.length > 0) console.error(`missing from ${currentFile}: ${missingFromCurrent.join(' ')}`);
  if (missingFromBaseline.length > 0) console.error(`missing from ${baselineFile}: ${missingFromBaseline.join(' ')}`);
  if (missingFromCurrent.length > 0 || missingFromBaseline.length > 0) return 1;
  return changes.length > 0 ? 2 : 0;
}

async function run([cmd, ...args]) {
  if (cmd === 'fetch') {
    await fetchRates(args[0] ?? 'rates.csv');
    return 0;
  }
  if (cmd === 'compare') {
    const { values, positionals } = parseArgs({
      args,
      allowPositionals: true,
      options: {
        threshold: { type: 'string', default: '20' },
        currencies: { type: 'string', multiple: true, default: [] },
      },
    });
    const [baselineFile, currentFile] = positionals;
    const threshold = Number(values.threshold);
    if (positionals.length === 2 && threshold >= 0) return compareRates(baselineFile, currentFile, threshold, values.currencies);
  }
  console.log(USAGE);
  return 1;
}

run(process.argv.slice(2))
  .then(code => {
    process.exitCode = code;
  })
  .catch(e => {
    console.error(e);
    process.exitCode = 1;
  });
