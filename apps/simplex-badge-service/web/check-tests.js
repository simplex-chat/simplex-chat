#!/usr/bin/env node
import fs from 'fs';
import path from 'path';

const testDir = 'build/test';

if (!fs.existsSync(testDir)) {
  console.error(`Error: test directory not found: ${testDir}`);
  process.exit(1);
}

const files = fs.readdirSync(testDir, { recursive: true });
const testFiles = files.filter(f => typeof f === 'string' && f.endsWith('.test.js'));

if (testFiles.length === 0) {
  console.error('Error: No test files found in build/test/');
  process.exit(1);
}

console.log(`Found ${testFiles.length} test file(s)`);
