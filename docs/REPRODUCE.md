---
title: Verify and reproduce builds
permalink: /reproduce/index.html
revision: 19.12.2025
---

# Verifying and reproducing release builds

- [Obtain release signing key](#obtain-release-signing-key)
- [Verify release signature](#verify-release-signature)
- [How to reproduce builds](#how-to-reproduce-builds)
   - [Server binaries](#server-binaries)
   - [Linux desktop apps and CLI](#linux-desktop-apps-and-cli)
   - [Android apps](#android-apps)

## Obtain release signing key

To verify the signature of `_sha256sums` or apks you need to obtain the signing key. You can do it from keyservers:

```sh
gpg --keyserver hkps://keys.openpgp.org --search build@simplex.chat
gpg --keyserver hkps://keyserver.ubuntu.com --search build@simplex.chat
```

```sh
gpg --list-keys build@simplex.chat
```

Once you obtain the signing key, verify that its fingerprint is:

```
BBDF 7BDA D154 8B16 836A F5B9 D53B DFD1 53C3 66BA
```

Additionally, compare the key fingerprint with:

- [simplexchat.eth](https://app.ens.domains/simplexchat.eth) (release key record)
- [Mastodon](https://mastodon.social/@simplex) (profile)
- [Reddit](https://www.reddit.com/r/SimpleXChat/) (side panel)

You can set the imported key as "ultimately trusted":

```sh
echo -e "trust\n5\ny\nquit" | gpg --command-fd 0 --edit-key build@simplex.chat
```

## Verify release signature

**Linux dekstop apps and CLI**:

Download the file with executable hashes and the signature. For example, to verify the `v6.5.0-beta.3` release:

```sh
curl -LO 'https://github.com/simplex-chat/simplex-chat/releases/download/v6.5.0-beta.3/_sha256sums.asc'
curl -LO 'https://github.com/simplex-chat/simplex-chat/releases/download/v6.5.0-beta.3/_sha256sums'
```

Verify the signature:

```sh
gpg --verify _sha256sums.asc _sha256sums
```

**Android APKs**:

Download the APK files and signatures. For example, to verify the `v6.5.0-beta.3` release:

```sh
curl -LO 'https://github.com/simplex-chat/simplex-chat/releases/download/v6.5.0-beta.3/simplex-aarch64.apk'
curl -LO 'https://github.com/simplex-chat/simplex-chat/releases/download/v6.5.0-beta.3/_simplex-aarch64.apk.asc'
curl -LO 'https://github.com/simplex-chat/simplex-chat/releases/download/v6.5.0-beta.3/simplex-armv7a.apk'
curl -LO 'https://github.com/simplex-chat/simplex-chat/releases/download/v6.5.0-beta.3/_simplex-armv7a.apk.asc'
```

Verify the signatures:

```sh
gpg --verify _simplex-armv7a.apk.asc simplex-armv7a.apk
gpg --verify _simplex-aarch64.apk.asc simplex-aarch64.apk
```

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

### Linux desktop apps and CLI

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

### Android apps

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

   1) Downloads and checks that APKs from GitHub are signed with valid key.
   2) Builds Android APKs in a docker container.
   3) Compares the releases by copying the signature from downloaded APKs to locally built APKs.
   4) If the resulting build is bit-by-bit identical, prints the message that this tag was reproduced.

   This will take a while.
