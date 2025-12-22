---
title: How to reproduce build and verify the signature for releases
revision: 19.12.2025
---

## Table of Contents

- [How to reproduce build](#how-to-reproduce-builds)
   - [Server binaries](#server-binaries)
   - [Linux CLI and Desktop](#linux-cli-and-desktop)
   - [Android](#android)
- [Obtaining the release signing key](#obtaining-the-release-signing-key)
- [Verifying the signature](#-verifying-the-signature)

## How to reproduce builds

To reproduce the build you must have:

- Linux machine
- `x86-64` architecture
- Installed `docker`, `curl` and `git`

### Server binaries

1. Download script:

   ```sh
   curl -LO 'https://raw.githubusercontent.com/simplex-chat/simplexmq/refs/heads/master/scripts/simplexmq-reproduce-builds.sh'
   ```

2. Make it executable:

   ```sh
   chmod +x simplexmq-reproduce-builds.sh
   ```

3. Execute the script with the required tag:

   ```sh
   ./simplexmq-reproduce-builds.sh 'v6.3.1'
   ```

   The script executes these steps (please review the script to confirm):

   1) builds all server binaries for the release in docker container.
   2) downloads binaries from the same GitHub release and compares them with the built binaries.
   3) if they all match, generates _sha256sums file with their checksums.

   This will take a while.

4. After compilation, you should see the folder named as the tag and repository name (e.g., `v6.3.1-simplexmq`) with two subfolders:

   ```sh
   ls v6.3.1-simplexmq
   ```

   ```sh
   from-source  prebuilt  _sha256sums
   ```

   The file _sha256sums contains the hashes of all builds - you can compare it with the same file in GitHub release.

### Linux CLI and Desktop

1. Download script:

   ```sh
   curl -LO 'https://raw.githubusercontent.com/simplex-chat/simplex-chat/refs/heads/master/scripts/simplex-chat-reproduce-builds.sh'
   ```

2. Make it executable:

   ```sh
   chmod +x simplex-chat-reproduce-builds.sh
   ```

3. Execute the script with the required tag:

   ```sh
   ./simplex-chat-reproduce-builds.sh 'v6.4.8'
   ```

   The script executes these steps (please review the script to confirm):

   1) builds all Linux CLI and Dekstop binaries for the release in docker container.
   2) downloads binaries from the same GitHub release and compares them with the built binaries.
   3) if they all match, generates _sha256sums file with their checksums.

   This will take a while.

4. After compilation, you should see the folder named as the tag and reprository name (e.g., `v6.4.8-simplex-chat`) with two subfolders:

   ```sh
   ls v6.4.8-simplex-chat
   ```

   ```sh
   from-source  prebuilt  _sha256sums
   ```

   The file _sha256sums contains the hashes of all builds - you can compare it with the same file in GitHub release.

### Android

In addition to basic requirments, Android build will:

- Take ~150gb of disc space
- Take ~20h to build all the architectures (depends on core count)
- Require at least 16gb of RAM

1. Download script:

   ```sh
   curl -LO 'https://raw.githubusercontent.com/simplex-chat/simplex-chat/refs/heads/master/scripts/simplex-chat-reproduce-builds-android.sh'
   ```

2. Make it executable:

   ```sh
   chmod +x simplex-chat-reproduce-builds-android.sh
   ```

3. Execute the script with the required tag:

   ```sh
   ./simplex-chat-reproduce-builds-android.sh 'v6.5.0-beta.3'
   ```

   The script executes these steps (please review the script to confirm):

   1) builds all Android apks for the release in docker container.
   2) downloads apks from the same GitHub release
   3) compares the releases by copying the signature from downloaded apk to locally built apk
   4) if the resulting build bit-by-bit identical, print the message that this tag is reproducible

   This will take a while.


## Obtaining the release signing key

To verify the signature of `_sha256sums` or apks you need to obtain the signing key. You can do it from keyservers:

```sh
gpg --keyserver hkps://keys.openpgp.org --search build@simplex.chat
gpg --keyserver hkps://keyserver.ubuntu.com --search build@simplex.chat
```

```sh
gpg --list-keys build@simplex.chat
```

Once you obtain the signing key, cross-verify it's fingerprint to be:

```
BBDF 7BDA D154 8B16 836A F5B9 D53B DFD1 53C3 66BA
```

From:

- [Reddit](https://www.reddit.com/r/SimpleXChat/) (side panel)
- [Mastodon](https://mastodon.social/@simplex)
- [simplexchat.eth](https://app.ens.domains/simplexchat.eth)

Once you verify the fingerprint, set the imported key as "ultimately trusted":

```sh
echo -e "trust\n5\ny\nquit" | gpg --command-fd 0 --edit-key build@simplex.chat
```

## Verifying the signature

To verify the signature, download the *.asc file and the build. For example:

```sh
curl -LO 'https://github.com/simplex-chat/simplex-chat/releases/download/v6.5.0-beta.2/_sha256sums.asc'
curl -LO 'https://github.com/simplex-chat/simplex-chat/releases/download/v6.5.0-beta.2/_sha256sums'
```

Then, verify the signature:

```sh
gpg --verify _sha256sums.asc _sha256sums
```
