const https = require('https');
const fs = require('fs');
const path = require('path');
const extract = require('extract-zip');

const GITHUB_REPO = 'simplex-chat/simplex-chat-libs';
const RELEASE_TAG = 'v6.5.0-beta.4';
const ROOT_DIR = process.cwd(); // Root of the package being installed
const LIBS_DIR = path.join(ROOT_DIR, 'libs')
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
    if (installedVersion === RELEASE_TAG) {
      console.log(`✓ Libraries version ${RELEASE_TAG} already installed`);
      return true;
    } else {
      console.log(`Version mismatch: installed ${installedVersion}, need ${RELEASE_TAG}`);
      cleanLibsDirectory();
      return false;
    }
  } catch (err) {
    console.warn(`Could not read installed.txt: ${err.message}`);
    return false;
  }
}

async function install() {
  try {
    // Check if already installed
    if (isAlreadyInstalled()) {
      return;
    }

    const { platformName, archName } = getPlatformInfo();
    const repoName = GITHUB_REPO.split('/')[1];
    const zipFilename = `${repoName}-${platformName}-${archName}.zip`;
    const ZIP_URL = `https://github.com/${GITHUB_REPO}/releases/download/${RELEASE_TAG}/${zipFilename}`;
    const ZIP_PATH = path.join(ROOT_DIR, zipFilename);
    const TEMP_EXTRACT_DIR = path.join(ROOT_DIR, '.temp-extract');

    console.log(`Detected: ${platformName} ${archName}`);
    console.log(`Downloading: ${zipFilename}`);

    // Create libs directory
    if (!fs.existsSync(LIBS_DIR)) {
      fs.mkdirSync(LIBS_DIR, { recursive: true });
    }

    // Download zip with error handling
    await downloadFile(ZIP_URL, ZIP_PATH);

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
    fs.writeFileSync(INSTALLED_FILE, RELEASE_TAG, 'utf-8');
    console.log(`✓ Wrote version ${RELEASE_TAG} to installed.txt`);

    // Cleanup
    fs.rmSync(TEMP_EXTRACT_DIR, { recursive: true, force: true });
    fs.unlinkSync(ZIP_PATH);
    console.log('✓ Installation complete');
  } catch (err) {
    console.error('✗ Failed:', err.message);
    process.exit(1);
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

install();
