---
title: Accessing files in Android app
revision: 07.02.2023
---

| 07.02.2023 | EN, [CZ](/docs/lang/cs/ANDROID.md), [FR](/docs/lang/fr/ANDROID.md) |

# Accessing files in Android app

SimpleX uses databases and stores its preferences inside private data directory in Android. The directory contains:
- databases
- sent and received files
- temporary files that will be deleted when not needed
- user preferences.


If you want to view what's stored inside SimpleX data directory you need to have:
- Unix-based operating system (or [MinGW](https://www.mingw-w64.org/downloads/) on Windows)
- ADB (Android Debug Bridge) tool installed on a computer ([download it here](https://developer.android.com/studio/releases/platform-tools) and install)
- your device connected via USB or Wi-Fi to the computer.

## The process:

- open SimpleX, go to `Database passphrase & export`, enable `App data backup`. This will make other steps working
- _optional_: if you want to view database contents, change database passphrase from random to yours. To do this, stop a chat in `Database passphrase & export` screen, open `Database passphrase`, enter new passphrase and confirm it, then update it. Do not forget it, otherwise you'll lose all your data in case passphrase will be asked again later
- open a terminal emulator (Windows CMD/Powershell will not work) and change directory to the one you want to use for storing the backup:

```bash
cd /tmp  # just an example
```
Then run the following:
```bash
adb -d backup -f chat.ab -noapk chat.simplex.app && 
tail -n +5 chat.ab > chat.dat && 
printf "\x1f\x8b\x08\x00\x00\x00\x00\x00" | cat - chat.dat > chat.gz && 
tar -xvzf chat.gz
```

Now unlock the device and confirm a backup operation without using a password for encryption, otherwise the commands will not work.

After that the backup should be ended. If you see an error saying `tar: Error is not recoverable: exiting now` but before that you have printed some file names, don't worry, it's ok.

Now the backed-up files will be inside `./apps/chat.simplex.app/`.

Please, note, that if you use a modern version of SimpleX, the databases will be encrypted, and you'll not be able to view contents of them without using `sqlcipher` application and without knowing decryption passphrase (you need to change it to yours from randomly generated in the app firstly).

## Decrypting databases

In order to view database data you need to decrypt it first. Install `sqlcipher` using your favorite package manager and run the following commands in the directory with databases:
```bash
sqlcipher files_chat.db
pragma key="youDecryptionPassphrase";
# Ensure it works fine
select * from users;
```

If you see `Parse error: no such table: users`, make sure you entered correct passphrase, and you have changed passphrase from random in Android app (if you got this database from Android device, of course).
