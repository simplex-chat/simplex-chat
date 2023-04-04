# Building with nix

## Updating nix package config
realToFrac
1. Install `nix`, `gawk` and `jq`.

2. Start nix-shell from repo root:

```sh
nix-shell -p nix-prefetch-git
```

3. Run in nix shell:

```sh
gawk -f ./scripts/nix/update-sha256.awk cabal.project > ./scripts/nix/sha256map.nix
```

## Setting up ARMv7a VM on MacOS

```sh
brew install qemu
qemu-img create -f qcow2 debian-armhf.qcow2 20G
qemu-system-arm -M virt -m 8g -cpu cortex-a15 -smp 4 -cdrom ~/Downloads/debian-11.6.0-armhf-netinst.iso -drive if=none,file=debian-armhf.qcow2,format=qcow2,id=hd -device virtio-blk-device,drive=hd -netdev user,id=vmnic -device virtio-net-device,netdev=vmnic -no-reboot
```