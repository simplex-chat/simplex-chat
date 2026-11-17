#!/bin/bash

set -euo pipefail

if [ "$#" -ne 2 ]; then
  echo "usage: $0 <package dir> <simplexmq clone>"
  exit 1
fi
out=$1/names-chain
simplexmq=$2
repo=$(cd "$(dirname "$0")/../../.." && pwd)
site=$repo/apps/simplex-domains
resolver_service=$simplexmq/scripts/resolver/service

check_python() {
  echo "python3: $(command -v python3), $(python3 --version)"
  if ! python3 -c 'import sys; sys.exit(sys.version_info < (3, 11))'; then
    echo "error: the resolver needs Python 3.11 or newer as python3"
    return 1
  fi
}

dockerfile_arg() {
  local value
  value=$(sed -n "s/^ARG $1=//p" "$site/webapp/dev/Dockerfile")
  if [ -z "$value" ]; then
    echo "error: no ARG $1= in $site/webapp/dev/Dockerfile" >&2
    return 1
  fi
  echo "$value"
}

foundry_platform() {
  case "$(uname -s) $(uname -m)" in
    "Linux x86_64") echo linux_amd64 ;;
    "Darwin arm64") echo darwin_arm64 ;;
    *)
      echo "error: no Foundry release is chosen for $(uname -s) $(uname -m)" >&2
      return 1
      ;;
  esac
}

fetch_anvil() {
  local version platform
  version=$(dockerfile_arg FOUNDRY_VERSION)
  platform=$(foundry_platform)
  curl -fsSL "https://github.com/foundry-rs/foundry/releases/download/$version/foundry_${version}_$platform.tar.gz" | tar -xz -C "$out" anvil
  "$out/anvil" --version
}

compile_contracts() {
  local namespace_commit ens_commit src
  namespace_commit=$(dockerfile_arg NAMESPACE_COMMIT)
  ens_commit=$(dockerfile_arg ENS_CONTRACTS_COMMIT)
  src=$(mktemp -d)/simplex-namespace-contract
  git clone --quiet https://github.com/brenzi/simplex-namespace-contract.git "$src"
  git -C "$src" fetch --quiet origin "$namespace_commit"
  git -C "$src" checkout --quiet "$namespace_commit"
  git -C "$src" submodule update --quiet --init ens-contracts
  git -C "$src/ens-contracts" checkout --quiet "$ens_commit"
  (
    cd "$src/ens-contracts"
    npx --yes pnpm@9 install --frozen-lockfile --ignore-scripts
    npx hardhat compile
  )
  mkdir -p "$out/contracts/artifacts" "$out/test/fixtures"
  cp -R "$src/ens-contracts/artifacts/contracts" "$out/contracts/artifacts/"
  cp -R "$src/test/fixtures/terms" "$out/test/fixtures/"
}

copy_webapp() {
  mkdir -p "$out/webapp"
  cp -R "$site/webapp/package.json" "$site/webapp/package-lock.json" "$site/webapp/src" "$site/webapp/dev" "$out/webapp/"
  (cd "$out/webapp" && npm ci)
}

locked_version() {
  python3 -I -c 'import sys, tomllib; print(next(p["version"] for p in tomllib.load(open(sys.argv[1], "rb"))["package"] if p["name"] == sys.argv[2]))' "$resolver_service/uv.lock" "$1"
}

install_resolver() {
  local eth_hash pycryptodome
  eth_hash=$(locked_version eth-hash)
  pycryptodome=$(locked_version pycryptodome)
  mkdir -p "$out/resolver"
  cp "$resolver_service/snrc-resolve.py" "$out/resolver/"
  python3 -m pip install --quiet --disable-pip-version-check --target "$out/resolver/lib" "eth-hash[pycryptodome]==$eth_hash" "pycryptodome==$pycryptodome"
  PYTHONPATH="$out/resolver/lib" python3 -c 'from eth_hash.auto import keccak; print("eth-hash", keccak(b"").hex())'
}

check_python
git -C "$repo" submodule update --init apps/simplex-domains
git -C "$site" log -1 --format='simplex.domains %H %s'
mkdir -p "$out"
fetch_anvil
compile_contracts
copy_webapp
install_resolver
ls -la "$out" "$out/contracts/artifacts/contracts" "$out/test/fixtures/terms" "$out/webapp" "$out/resolver" "$out/resolver/lib"
