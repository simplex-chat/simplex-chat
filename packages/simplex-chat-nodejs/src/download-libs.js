const https = require('https');
const fs = require('fs');
const os = require('os');
const path = require('path');
const crypto = require('crypto');
const extract = require('extract-zip');

const GITHUB_REPO = 'simplex-chat/simplex-chat-libs';
const RELEASE_TAG = 'v7.1.0-beta.6';
const SHA256 = {
  'simplex-chat-libs-linux-x86_64-postgres.zip': '7e5040f6d314a4a8ad3b58d2f5668b00de3fdfdfedf7e669720f63cbfeb6b52d',
  'simplex-chat-libs-linux-x86_64.zip': '792f94b7df09c69f2e732fabbfb5b3c1badf766cdacfba1af8b663296c9d0bcf',
  'simplex-chat-libs-macos-aarch64.zip': '9c8849016a5d6faedc288d2222a89bca17ce891bb3bd719f02f6a4c81dfe5ce1',
  'simplex-chat-libs-macos-x86_64.zip': '4c612035790b3941633c74710010da7fd717d06b6bff00409322d7856be83216',
  'simplex-chat-libs-windows-x86_64.zip': '0de1e3e529b88d693e8aad9cc28b02e366916c48bd101deeed5d6e93d3a73942',
};
const BACKEND = (process.env.SIMPLEX_BACKEND || process.env.npm_config_simplex_backend || 'sqlite').toLowerCase();
const LIBS_DIR_OVERRIDE = process.env.SIMPLEX_LIBS_DIR || process.env.npm_config_simplex_libs_dir;
const ADDON_PATH_OVERRIDE = process.env.SIMPLEX_ADDON_PATH;
const LIB_NAMES = ['libsimplex.so', 'libsimplex.dylib', 'libsimplex.dll'];

if (BACKEND !== 'sqlite' && BACKEND !== 'postgres') {
  throw new Error(`Invalid SIMPLEX_BACKEND: "${BACKEND}". Must be "sqlite" or "postgres".`);
}

if (BACKEND === 'postgres' && (process.platform !== 'linux' || process.arch !== 'x64')) {
  throw new Error(`SIMPLEX_BACKEND=postgres is only supported on Linux x86_64.`);
}

const ROOT_DIR = path.resolve(process.env.SIMPLEX_CACHE_DIR || path.join(cacheDir(), 'simplex-chat', 'nodejs'), RELEASE_TAG);
const LIBS_DIR = path.join(ROOT_DIR, BACKEND)
const INSTALLED_FILE = path.join(LIBS_DIR, 'installed.txt');

// Detect platform and architecture
function getPlatformInfo() {
  const platform = process.platform;
  const arch = process.arch;

  let platformName;
  let archName;

  if (platform === 'linux') {
    platformName = 'linux';
  } else if (platform === 'darwin') {
    platformName = 'macos';
  } else if (platform === 'win32') {
    platformName = 'windows';
  } else {
    throw new Error(`Unsupported platform: ${platform}`);
  }

  if (arch === 'x64') {
    archName = 'x86_64';
  } else if (arch === 'arm64') {
    archName = 'aarch64';
  } else {
    throw new Error(`Unsupported architecture: ${arch}`);
  }

  return { platformName, archName };
}

// Cleanup on libs version mismatch
function cleanLibsDirectory() {
  if (fs.existsSync(LIBS_DIR)) {
    console.log('Cleaning old libraries...');
    fs.rmSync(LIBS_DIR, { recursive: true, force: true });
    fs.mkdirSync(LIBS_DIR, { recursive: true });
    console.log('✓ Old libraries removed');
  }
}

// Check if libraries are already installed with the correct version
function isAlreadyInstalled() {
  if (!fs.existsSync(INSTALLED_FILE)) {
    return false;
  }

  try {
    const installedVersion = fs.readFileSync(INSTALLED_FILE, 'utf-8').trim();
    const expectedVersion = `${RELEASE_TAG}:${BACKEND}`;
    if (installedVersion === expectedVersion) {
      console.log(`✓ Libraries version ${RELEASE_TAG}:${BACKEND} already installed`);
      return true;
    } else {
      console.log(`Version mismatch: installed ${installedVersion}, need ${expectedVersion}`);
      cleanLibsDirectory();
      return false;
    }
  } catch (err) {
    console.warn(`Could not read installed.txt: ${err.message}`);
    return false;
  }
}

// No version check: the files behind SIMPLEX_LIBS_DIR change on every rebuild.
function installFromOverride() {
  if (!fs.existsSync(LIBS_DIR_OVERRIDE)) {
    throw new Error(`SIMPLEX_LIBS_DIR does not exist: ${LIBS_DIR_OVERRIDE}`);
  }
  const lib = LIB_NAMES.find((name) => fs.existsSync(path.join(LIBS_DIR_OVERRIDE, name)));
  if (!lib) {
    throw new Error(`No ${LIB_NAMES.join(' / ')} in SIMPLEX_LIBS_DIR: ${LIBS_DIR_OVERRIDE}`);
  }
  console.log(`Using libraries from SIMPLEX_LIBS_DIR: ${LIBS_DIR_OVERRIDE}`);
  return path.resolve(LIBS_DIR_OVERRIDE, lib);
}

async function install() {
  try {
    if (LIBS_DIR_OVERRIDE) {
      return installFromOverride();
    }

    // Check if already installed
    if (isAlreadyInstalled()) {
      return libPath(LIBS_DIR);
    }

    const { platformName, archName } = getPlatformInfo();
    const repoName = GITHUB_REPO.split('/')[1];
    const backendSuffix = BACKEND === 'postgres' ? '-postgres' : '';
    const zipFilename = `${repoName}-${platformName}-${archName}${backendSuffix}.zip`;
    const ZIP_URL = `https://github.com/${GITHUB_REPO}/releases/download/${RELEASE_TAG}/${zipFilename}`;
    const ZIP_PATH = path.join(ROOT_DIR, zipFilename);
    const TEMP_EXTRACT_DIR = path.join(ROOT_DIR, '.temp-extract');

    console.log(`Detected: ${platformName} ${archName}`);
    console.log(`Backend: ${BACKEND}`);
    console.log(`Downloading: ${zipFilename}`);

    // Create libs directory
    if (!fs.existsSync(LIBS_DIR)) {
      fs.mkdirSync(LIBS_DIR, { recursive: true });
    }

    // Download zip with error handling
    await downloadFile(ZIP_URL, ZIP_PATH);
    verifyFile(zipFilename, ZIP_PATH);

    // Extract to temporary directory
    console.log('Extracting to temporary directory...');
    if (!fs.existsSync(TEMP_EXTRACT_DIR)) {
      fs.mkdirSync(TEMP_EXTRACT_DIR, { recursive: true });
    }
    await extract(ZIP_PATH, { dir: TEMP_EXTRACT_DIR });

    // Move libs folder contents to final location
    console.log('Moving libraries to libs/...');
    const libsSourcePath = path.join(TEMP_EXTRACT_DIR, 'libs');

    if (fs.existsSync(libsSourcePath)) {
      // Copy all files from libs folder to LIBS_DIR
      const files = fs.readdirSync(libsSourcePath);
      files.forEach(file => {
        const src = path.join(libsSourcePath, file);
        const dest = path.join(LIBS_DIR, file);

        if (fs.statSync(src).isDirectory()) {
          copyDirSync(src, dest);
        } else {
          fs.copyFileSync(src, dest);
        }
      });
    } else {
      throw new Error('libs folder not found in zip archive');
    }

    // Write installed.txt with version
    fs.writeFileSync(INSTALLED_FILE, `${RELEASE_TAG}:${BACKEND}`, 'utf-8');
    console.log(`✓ Wrote version ${RELEASE_TAG}:${BACKEND} to installed.txt`);

    // Cleanup
    fs.rmSync(TEMP_EXTRACT_DIR, { recursive: true, force: true });
    fs.unlinkSync(ZIP_PATH);
    console.log('✓ Installation complete');
    return libPath(LIBS_DIR);
  } catch (err) {
    console.error('✗ Failed:', err.message);
    throw err;
  }
}

async function installAddon() {
  if (ADDON_PATH_OVERRIDE) {
    return path.resolve(ADDON_PATH_OVERRIDE);
  }
  const { platformName, archName } = getPlatformInfo();
  const addonFilename = `simplex-chat-nodejs-${platformName}-${archName}.node`;
  const addonPath = path.join(ROOT_DIR, addonFilename);
  if (!fs.existsSync(addonPath)) {
    fs.mkdirSync(ROOT_DIR, { recursive: true });
    const downloadPath = `${addonPath}.download`;
    await downloadFile(`https://github.com/${GITHUB_REPO}/releases/download/${RELEASE_TAG}/${addonFilename}`, downloadPath);
    verifyFile(addonFilename, downloadPath);
    fs.renameSync(downloadPath, addonPath);
  }
  return addonPath;
}

function libPath(dir) {
  return path.join(dir, LIB_NAMES.find((name) => fs.existsSync(path.join(dir, name))));
}

function cacheDir() {
  if (process.platform === 'darwin') {
    return path.join(os.homedir(), 'Library', 'Caches');
  }
  if (process.platform === 'win32') {
    return process.env.LOCALAPPDATA;
  }
  return process.env.XDG_CACHE_HOME || path.join(os.homedir(), '.cache');
}

function verifyFile(filename, file) {
  const hash = crypto.createHash('sha256').update(fs.readFileSync(file)).digest('hex');
  if (hash !== SHA256[filename]) {
    throw new Error(`SHA-256 of ${filename} is ${hash}, expected ${SHA256[filename]}`);
  }
}

// Helper function to recursively copy directories
function copyDirSync(src, dest) {
  if (!fs.existsSync(dest)) {
    fs.mkdirSync(dest, { recursive: true });
  }
  const files = fs.readdirSync(src);
  files.forEach(file => {
    const srcFile = path.join(src, file);
    const destFile = path.join(dest, file);
    if (fs.statSync(srcFile).isDirectory()) {
      copyDirSync(srcFile, destFile);
    } else {
      fs.copyFileSync(srcFile, destFile);
    }
  });
}

function downloadFile(url, dest) {
  return new Promise((resolve, reject) => {
    const file = fs.createWriteStream(dest);

    https.get(url, { headers: { 'User-Agent': 'Node.js' } }, (response) => {
      // Handle redirects
      if (response.statusCode === 302 || response.statusCode === 301) {
        file.destroy();
        fs.unlink(dest, () => {});
        return downloadFile(response.headers.location, dest)
          .then(resolve)
          .catch(reject);
      }

      // Handle 404
      if (response.statusCode === 404) {
        file.destroy();
        fs.unlink(dest, () => {});
        reject(new Error(
          `Release artifact not found (404). Check:\n` +
          `  - Repository exists: ${url.split('/releases')[0]}\n` +
          `  - Release tag exists: ${RELEASE_TAG}\n` +
          `  - Artifact filename is correct`
        ));
        return;
      }

      // Handle 403
      if (response.statusCode === 403) {
        file.destroy();
        fs.unlink(dest, () => {});
        reject(new Error(
          `Access denied (403). The repository may be private.\n` +
          `Set GITHUB_TOKEN environment variable for private repos.`
        ));
        return;
      }

      // Handle other HTTP errors
      if (response.statusCode < 200 || response.statusCode >= 300) {
        file.destroy();
        fs.unlink(dest, () => {});
        reject(new Error(
          `HTTP ${response.statusCode}: Failed to download from ${url}`
        ));
        return;
      }

      response.pipe(file);

      file.on('finish', () => {
        file.close();
        resolve();
      });

      file.on('error', (err) => {
        fs.unlink(dest, () => {});
        reject(new Error(`File write error: ${err.message}`));
      });
    }).on('error', (err) => {
      file.destroy();
      fs.unlink(dest, () => {});
      reject(new Error(`Download error: ${err.message}`));
    });
  });
}

module.exports = { install, installAddon };
